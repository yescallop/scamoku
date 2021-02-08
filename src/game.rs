use crate::board::*;
use crate::rule::*;

use std::sync::mpsc::{channel, Receiver, RecvTimeoutError, Sender};
use std::{any::Any, thread};
use std::{
    fmt,
    time::{Duration, Instant},
};

use anyhow::*;

/// A set of choices that can be made by a player.
#[derive(Debug, Clone)]
pub enum ChoiceSet {
    /// Messages.
    Message(Vec<String>),
    /// Moves.
    Move(Vec<Point>),
    /// Move count ranging from 1 to `max_count`.
    MoveCount { max_count: usize },
}

impl ChoiceSet {
    /// Creates a choice set of messages from a `Vec<&str>`.
    pub fn from_msg(v: Vec<&str>) -> ChoiceSet {
        let v = v.into_iter().map(|s| s.into()).collect();
        ChoiceSet::Message(v)
    }

    /// Checks whether an index is contained in the choice set.
    pub fn contains_index(&self, i: usize) -> bool {
        match self {
            ChoiceSet::Message(v) => i < v.len(),
            ChoiceSet::Move(v) => i < v.len(),
            ChoiceSet::MoveCount { max_count } => i >= 1 && i <= *max_count,
        }
    }
}
/// The side of a player.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Side {
    First,
    Second,
}

impl Side {
    /// Returns the opposite side.
    pub fn opposite(self) -> Side {
        match self {
            Side::First => Side::Second,
            Side::Second => Side::First,
        }
    }

    /// Returns the ordinal of the side, 0 for `First` and 1 for `Second`.
    pub fn ord(self) -> usize {
        match self {
            Side::First => 0,
            Side::Second => 1,
        }
    }
}

impl fmt::Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Side::First => "Player 1",
            Side::Second => "Player 2",
        }
        .fmt(f)
    }
}

/// The result of a game.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct GameResult {
    /// The kind of the result.
    pub kind: GameResultKind,
    /// The winning side, or `None` for a draw.
    pub winning_side: Option<Side>,
}

/// The kind of a result, that is, why the game is ended.
#[derive(displaydoc::Display, Debug, Copy, Clone, Eq, PartialEq)]
pub enum GameResultKind {
    /// A row has been completed.
    RowCompleted,
    /// A forbidden move was made.
    ForbiddenMoveMade,
    /// Timeout.
    Timeout,
    /// Unexpected error occurred.
    Error,
    /// Interrupted.
    Interrupt,
    /// The board is full.
    BoardFull,
    /// Both players passed.
    BothPass,
    /// Player disconnected.
    PlayerDisconnect,
    /// Draw offer has been accepted.
    DrawOfferAccepted,
}

/// A message sent through a channel from a player thread to the game thread.
#[derive(Debug, Copy, Clone)]
pub enum PlayerMsg {
    /// An actual move or a pass, with an attribute of a draw offer or a win claim.
    Move(Option<Point>, MoveAttr),
    /// Claims a win in an intersection, either by moving in or for a forbidden move.
    WinClaim(Point),
    /// A choice of index in the choice set provided.
    Choice(usize),
    /// Accepts a draw offer.
    AcceptDrawOffer,
    /// Disconnects when sender is dropped.
    Disconnect,
}

/// The attribute of a move.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MoveAttr {
    /// Normal.
    Normal,
    /// With a draw offer.
    DrawOffer,
}

/// A raw message sent from players.
struct RawPlayerMsg {
    /// The side for which the message is sent, or `None` for an anonymous message.
    side: Option<Side>,
    /// The inner `PlayerMsg`.
    inner: PlayerMsg,
}

/// A message sender for player.
///
/// Drop the sender to disconnect from the game.
pub struct PlayerMsgSender {
    side: Option<Side>,
    inner: Sender<RawPlayerMsg>,
}

impl PlayerMsgSender {
    /// Makes a move.
    pub fn make_move(&self, p: Point) {
        self.send(PlayerMsg::Move(Some(p), MoveAttr::Normal))
    }

    /// Passes.
    pub fn pass(&self) {
        self.send(PlayerMsg::Move(None, MoveAttr::Normal))
    }

    /// Claims a win in an intersection, either by moving in or for a forbidden move.
    pub fn claim_win(&self, p: Point) {
        self.send(PlayerMsg::WinClaim(p))
    }

    /// Makes a move and offers a draw.
    pub fn draw_move(&self, p: Point) {
        self.send(PlayerMsg::Move(Some(p), MoveAttr::DrawOffer))
    }

    /// Accepts a draw offer.
    pub fn accept_draw_offer(&self) {
        self.send(PlayerMsg::AcceptDrawOffer)
    }

    /// Makes a choice.
    pub fn make_choice(&self, choice: usize) {
        self.send(PlayerMsg::Choice(choice))
    }

    fn send(&self, msg: PlayerMsg) {
        let res = self.inner.send(RawPlayerMsg {
            side: self.side,
            inner: msg,
        });
        drop(res);
    }
}

impl Drop for PlayerMsgSender {
    fn drop(&mut self) {
        // Do not send a message if the sender is anonymous.
        if self.side.is_some() {
            self.send(PlayerMsg::Disconnect);
        }
    }
}

/// The ordinal and total of a set of offered moves.
#[derive(Debug, Copy, Clone)]
pub struct OfferOrd {
    pub ord: usize,
    pub total: usize,
}

/// A message sent through a channel from the game thread to a player thread.
#[derive(Debug, Clone)]
pub enum Msg {
    /// Game started with settings.
    GameStart(GameSettings),
    /// Move requested with an optional `OfferOrd`.
    MoveRequest(Option<OfferOrd>),
    /// Choice requested.
    ChoiceRequest(ChoiceSet),
    /// Deadline updated.
    DeadlineUpdate(Instant),
    /// Move made by the opponent.
    OpponentMove(Option<Point>, MoveAttr),
    /// Stone swapped.
    StoneSwap,
    /// Game ended with a result.
    GameEnd(GameResult),
    /// Error occurred by yourself.
    Error(PlayerMsg, String),
}

/// An event of the game.
#[derive(Debug, Clone)]
pub enum Event {
    /// Game started with settings.
    GameStart(GameSettings),
    /// Move requested with an optional `OfferOrd`.
    MoveRequest(Side, Option<OfferOrd>),
    /// Choice requested.
    ChoiceRequest(Side, ChoiceSet),
    /// Deadline updated.
    DeadlineUpdate(Side, Instant),
    /// Message sent by a player.
    PlayerMsg(Side, PlayerMsg),
    /// Move made by a player.
    Move(Side, Stone, Option<Point>, MoveAttr),
    /// Choice made by a player.
    Choice(Side, ChoiceSet, usize),
    /// Stone swapped.
    StoneSwap,
    /// Game ended with a result.
    GameEnd(GameResult),
    /// Error occurred by a player.
    Error(Side, PlayerMsg, String),
}

/// The settings of a game.
#[derive(Debug, Clone)]
pub struct GameSettings {
    /// The ID of the rule.
    pub rule_id: String,
    /// The variant of the rule.
    pub variant: Variant,
    /// The board size.
    pub board_size: u32,
    /// The timeout for moves.
    pub move_timeout: Option<Duration>,
    /// The timeout for the game.
    pub game_timeout: Option<Duration>,
    /// Whether the game is strict.
    ///
    /// If a game is strict, it will end as soon as a win can be achieved.
    /// No win claim is required.
    pub strict: bool,
}

/// A handle for player to communicate with the game thread.
pub type PlayerHandle = (PlayerMsgSender, Receiver<Msg>);

/// A builder for a game.
pub struct Builder {
    rule: &'static dyn Rule,
    board_size: u32,
    move_timeout: Option<Duration>,
    game_timeout: Option<Duration>,
    strict: bool,
}

impl Builder {
    /// Sets the rule.
    pub fn with_rule(rule: &'static dyn Rule) -> Self {
        Builder {
            rule,
            board_size: 15,
            move_timeout: None,
            game_timeout: None,
            strict: false,
        }
    }

    /// Sets the board size.
    pub fn board_size(mut self, size: u32) -> Self {
        assert!(
            self.rule.variant() != Variant::StandardRenju || size == 15,
            "Renju board must be 15x15"
        );
        self.board_size = size;
        self
    }

    /// Sets the timeout for moves.
    pub fn move_timeout(mut self, timeout: Duration) -> Self {
        self.move_timeout = Some(timeout);
        self
    }

    /// Sets the timeout for the game.
    pub fn game_timeout(mut self, timeout: Duration) -> Self {
        self.game_timeout = Some(timeout);
        self
    }

    /// Makes the game strict.
    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }

    /// Starts the game, returning a handle with message sender.
    pub fn start(self) -> Handle {
        let (p_msg_tx, msg_rx) = channel();
        let p_msg_txs = (
            PlayerMsgSender {
                side: Some(Side::First),
                inner: p_msg_tx.clone(),
            },
            PlayerMsgSender {
                side: Some(Side::Second),
                inner: p_msg_tx,
            },
        );

        // Game message channels
        let g_msg_chans = (channel(), channel());
        let (msg_txs, g_msg_rxs) = (
            ((g_msg_chans.0).0, (g_msg_chans.1).0),
            ((g_msg_chans.0).1, (g_msg_chans.1).1),
        );

        // Game event channel
        let (event_tx, event_rx) = channel();

        // Start the game thread
        let join_handle =
            thread::spawn(move || Control::new(self, Some(msg_txs), msg_rx, event_tx).start());

        Handle {
            player_handles: ((p_msg_txs.0, g_msg_rxs.0), (p_msg_txs.1, g_msg_rxs.1)),
            event_rx,
            join_handle,
        }
    }

    /// Starts the game, returning a raw handle.
    pub fn start_raw(self) -> RawHandle {
        let (msg_tx, msg_rx) = channel();

        // Game event channel
        let (event_tx, event_rx) = channel();

        // Start the game thread
        let join_handle = thread::spawn(move || Control::new(self, None, msg_rx, event_tx).start());

        RawHandle {
            msg_tx: PlayerMsgSender {
                side: None,
                inner: msg_tx,
            },
            event_rx,
            join_handle,
        }
    }
}

/// A raw handle of game.
pub struct RawHandle {
    /// An anonymous sender for player messages.
    pub msg_tx: PlayerMsgSender,
    /// A receiver of game events.
    pub event_rx: Receiver<Event>,
    /// A handle to join on the game thread, which returns the result.
    pub join_handle: thread::JoinHandle<GameResult>,
}

/// A handle of game split between players.
pub struct Handle {
    /// The handles (first, second) that two players use to communicate with the game thread.
    pub player_handles: (PlayerHandle, PlayerHandle),
    /// A receiver of game events.
    pub event_rx: Receiver<Event>,
    /// A handle to join on the game thread, which returns the result.
    pub join_handle: thread::JoinHandle<GameResult>,
    // pub interrupt_tx: Sender<()>,
}

/// Starts a game with a rule and default settings.
pub fn start_with_rule(rule: &'static dyn Rule) -> Handle {
    Builder::with_rule(rule).start()
}

/// Provides control over a game.
pub struct Control {
    /// The message senders, or `None` if started raw.
    msg_txs: Option<(Sender<Msg>, Sender<Msg>)>,
    /// The message receiver.
    msg_rx: Receiver<RawPlayerMsg>,
    /// The event sender.
    event_tx: Sender<Event>,

    /// The board.
    board: Board,
    /// The maximum move index on the board, which equals to `size * size`.
    max_move_index: u32,

    /// A static reference to the rule.
    rule: &'static dyn Rule,
    /// The rule data.
    rule_data: Option<Box<dyn Any>>,
    /// Whether the game is in the opening.
    in_opening: bool,
    /// Whether the game is strict.
    strict: bool,

    /// The current side of the game.
    cur_side: Side,
    /// The current stone of the game.
    cur_stone: Stone,

    /// The timeout for moves.
    move_timeout: Option<Duration>,
    /// The timeout for the game.
    game_timeout: Option<Duration>,
    /// The game timer, or `None` if there is no timeout for the game.
    game_timer: Option<[Duration; 2]>,

    /// The start and deadline time of a request, or `None` if there is no timeout.
    start_deadline: Option<(Instant, Instant)>,

    /// The current choice index, or `0` if no choice has ever been requested.
    cur_choice_index: u32,
    /// The choice data, or `None` if no choice is being requested.
    choice_data: Option<(Side, ChoiceSet)>,

    /// The offered moves, or `None` if no move has been offered.
    offered_moves: Option<Vec<Point>>,

    /// The kind of last move.
    ///
    /// `None` for a pass, `Some(MoveAttr)` for an actual move.
    last_move_kind: Option<MoveAttr>,

    /// The result of the game, or `None` if the game is not ended.
    result: Option<GameResult>,
}

impl Control {
    fn new(
        builder: Builder,
        msg_txs: Option<(Sender<Msg>, Sender<Msg>)>,
        msg_rx: Receiver<RawPlayerMsg>,
        event_tx: Sender<Event>,
    ) -> Control {
        let size = builder.board_size;
        let board = Board::new(size);
        Control {
            msg_txs,
            msg_rx,
            event_tx,
            board,
            max_move_index: size * size - 1,
            rule: builder.rule,
            rule_data: None,
            in_opening: true,
            strict: builder.strict,
            cur_side: Side::First,
            cur_stone: Stone::Black,
            move_timeout: builder.move_timeout,
            game_timeout: builder.game_timeout,
            game_timer: builder.game_timeout.map(|d| [d, d]),
            start_deadline: None,
            cur_choice_index: 0,
            choice_data: None,
            offered_moves: None,
            last_move_kind: Some(MoveAttr::Normal),
            result: None,
        }
    }

    /// Returns the board.
    pub fn board(&self) -> &Board {
        &self.board
    }

    /// Initiates the rule data.
    pub fn init_rule_data(&mut self, data: impl Any) {
        assert!(self.rule_data.is_none());
        self.rule_data = Some(Box::new(data));
    }

    /// Returns the rule data.
    ///
    /// # Panics
    /// Panics if the rule data is uninitialized or data type mismatches.
    pub fn rule_data<T: Any>(&mut self) -> &mut T {
        self.rule_data
            .as_deref_mut()
            .expect("rule data uninitialized")
            .downcast_mut()
            .expect("data type mismatch")
    }

    /// Returns `true` if the game is strict.
    pub fn strict(&self) -> bool {
        self.strict
    }

    /// Returns the current side.
    pub fn cur_side(&self) -> Side {
        self.cur_side
    }

    /// Returns the current stone.
    pub fn cur_stone(&self) -> Stone {
        self.cur_stone
    }

    /// Returns the current choice index.
    pub fn cur_choice_index(&self) -> u32 {
        self.cur_choice_index
    }

    /// Returns the stone of a side.
    pub fn stone_by_side(&self, side: Side) -> Stone {
        if self.cur_side == side {
            self.cur_stone
        } else {
            self.cur_stone.opposite()
        }
    }

    /// Returns the side with a type of stone.
    pub fn side_by_stone(&self, stone: Stone) -> Side {
        if self.cur_stone == stone {
            self.cur_side
        } else {
            self.cur_side.opposite()
        }
    }

    /// Ends the opening.
    ///
    /// # Panics
    /// Panics if the game is not in the opening.
    pub fn end_opening(&mut self) {
        assert!(self.in_opening);
        self.in_opening = false;
        self.rule_data = None;
    }

    /// Swaps the stones.
    ///
    /// # Panics
    /// Panics if the game is not in the opening.
    pub fn swap(&mut self) {
        assert!(self.in_opening);
        self.cur_side = self.cur_side.opposite();
        self.msg_both(Msg::StoneSwap);
        self.event(Event::StoneSwap);
    }

    /// Requests a move offer of the given `count`.
    ///
    /// # Panics
    /// Panics if the game is not in the opening or the `count` is zero.
    pub fn request_move_offer(&mut self, count: usize) {
        assert!(self.in_opening && count != 0);
        self.offered_moves = Some(Vec::with_capacity(count));
    }

    /// Requests a choice.
    ///
    /// # Panics
    /// Panics if the game is not in the opening or a choice has already been requested,
    pub fn request_choice(&mut self, side: Side, choice_set: ChoiceSet) {
        assert!(self.in_opening && self.choice_data.is_none());
        self.choice_data = Some((side, choice_set));
    }

    /// Ends the game with the given result.
    pub fn end(&mut self, kind: GameResultKind, winning_side: Side) {
        if self.result.is_none() {
            self.result = Some(GameResult {
                kind,
                winning_side: Some(winning_side),
            });
        }
    }

    /// Ends the game in a draw with the given result.
    pub fn end_draw(&mut self, kind: GameResultKind) {
        if self.result.is_none() {
            self.result = Some(GameResult {
                kind,
                winning_side: None,
            });
        }
    }

    /// Sends an event.
    fn event(&self, e: Event) {
        let res = self.event_tx.send(e);
        // Ignore if event receiver is dropped.
        drop(res);
    }

    /// Sends a message to one side.
    fn msg(&mut self, side: Side, msg: Msg) {
        if let Some(msg_txs) = &self.msg_txs {
            let res = match side {
                Side::First => &msg_txs.0,
                Side::Second => &msg_txs.1,
            }
            .send(msg);
            // Ignore if message receiver is dropped.
            drop(res);
        }
    }

    /// Sends a message to both sides.
    fn msg_both(&mut self, msg: Msg) {
        if let Some(msg_txs) = &self.msg_txs {
            // Ignore if message receiver is dropped.
            drop(msg_txs.0.send(msg.clone()));
            drop(msg_txs.1.send(msg));
        }
    }

    /// Broadcasts an error.
    fn err(&mut self, side: Side, player_msg: PlayerMsg, msg: String) {
        self.msg(side, Msg::Error(player_msg, msg.clone()));
        self.event(Event::Error(side, player_msg, msg));
    }

    /// Switches the turn.
    fn switch(&mut self) {
        self.cur_side = self.cur_side.opposite();
        self.cur_stone = self.cur_stone.opposite();
    }

    /// Makes a move on the board if it is not a pass,
    /// switches the turn and broadcasts it.
    fn make_move(&mut self, side: Side, stone: Stone, p: Option<Point>, attr: MoveAttr) {
        if let Some(p) = p {
            self.board.make_move(p, stone);
        }
        self.switch();
        self.msg(side.opposite(), Msg::OpponentMove(p, attr));
        self.event(Event::Move(side, stone, p, attr));
    }

    /// Starts the game.
    fn start(mut self) -> GameResult {
        // Broadcast the game settings.
        let settings = GameSettings {
            rule_id: self.rule.id(),
            variant: self.rule.variant(),
            board_size: self.board.size(),
            move_timeout: self.move_timeout,
            game_timeout: self.game_timeout,
            strict: self.strict,
        };
        self.msg_both(Msg::GameStart(settings.clone()));
        self.event(Event::GameStart(settings));

        // Initiate the game with the rule.
        self.rule.init(&mut self);

        // Loop until the game is ended.
        while self.result.is_none() {
            if self.board.cur_move_index() == self.max_move_index {
                // End the game for a full board.
                self.end_draw(GameResultKind::BoardFull);
                break;
            }

            // Request a choice before a move.
            let side = self
                .choice_data
                .as_ref()
                .map(|d| d.0)
                .unwrap_or(self.cur_side);

            let now = Instant::now();
            // Calculate the timeout.
            let timeout = match self.start_deadline {
                // Move or choice is being requested. Reuse the deadline.
                Some((_, deadline)) => Some(deadline - now),
                None => {
                    // Update the deadline and make a new request.
                    let game_timeout = self.game_timer.map(|t| t[side.ord()]);
                    // Take the minimum.
                    let timeout = match (game_timeout, self.move_timeout) {
                        (None, None) => None,
                        (Some(t), None) | (None, Some(t)) => Some(t),
                        (Some(t1), Some(t2)) => Some(t1.min(t2)),
                    };
                    self.start_deadline = timeout.map(|t| (now, now + t));
                    if let Some((_, deadline)) = self.start_deadline {
                        // Broadcast the updated deadline.
                        self.msg(side, Msg::DeadlineUpdate(deadline));
                        self.event(Event::DeadlineUpdate(side, deadline));
                    }
                    self.make_request();
                    timeout
                }
            };

            let res = if let Some(t) = timeout {
                self.msg_rx.recv_timeout(t)
            } else {
                // Infinite timeout
                self.msg_rx
                    .recv()
                    .map_err(|_| RecvTimeoutError::Disconnected)
            };

            match res {
                Ok(msg) => {
                    // If sent by an anonymous sender, a message should belong to the side requested.
                    let msg_side = msg.side.unwrap_or(side);
                    if let Err(e) = self.process_msg(msg_side, msg.inner) {
                        self.err(msg_side, msg.inner, e.to_string());
                    }
                }
                Err(RecvTimeoutError::Disconnected) => {
                    // Reach here after an anonymous sender is dropped.
                    self.end_draw(GameResultKind::Error);
                }
                Err(RecvTimeoutError::Timeout) => {
                    // Timeout reached
                    self.end(GameResultKind::Timeout, side.opposite())
                }
            }
        }

        // Broadcast the result and goodbye.
        let result = self.result.unwrap();
        self.msg_both(Msg::GameEnd(result));
        self.event(Event::GameEnd(result));
        result
    }

    /// Makes a request, either for a choice or a move.
    fn make_request(&mut self) {
        // Here we clone the choice data since the request might fail.
        if let Some((side, choice_set)) = self.choice_data.clone() {
            self.msg(side, Msg::ChoiceRequest(choice_set.clone()));
            self.event(Event::ChoiceRequest(side, choice_set));
        } else {
            // Request a move.
            let side = self.cur_side;
            let offer_ord = self.offered_moves.as_ref().map(|v| OfferOrd {
                ord: v.len(),
                total: v.capacity(),
            });
            self.msg(side, Msg::MoveRequest(offer_ord));
            self.event(Event::MoveRequest(side, offer_ord));
        }
    }

    /// Stops the timer of a side.
    fn stop_timer(&mut self, side: Side) {
        if let Some((start, _)) = self.start_deadline.take() {
            let elapsed = start.elapsed();
            let ord = side.ord();
            if let Some(t) = &mut self.game_timer.map(|t| t[ord]) {
                *t = t.checked_sub(elapsed).unwrap_or_default()
            }
        }
    }

    /// Processes a message.
    fn process_msg(&mut self, side: Side, msg: PlayerMsg) -> Result<()> {
        self.event(Event::PlayerMsg(side, msg));
        match msg {
            PlayerMsg::Move(p, attr) => self.process_move(side, p, attr)?,
            PlayerMsg::WinClaim(p) => {
                ensure!(!self.strict, "win claim is unavailable in strict mode");
                ensure!(!self.in_opening, "win claim in the opening");
                let int = self
                    .board
                    .get(p)
                    .ok_or(anyhow!("win claim out of board: {}", p))?;
                if int.is_empty() {
                    self.process_move(side, Some(p), MoveAttr::Normal)?
                }
                self.rule
                    .variant()
                    .judge(self, p, side, self.stone_by_side(side));
                ensure!(self.result.is_some(), "win claim was unsuccessful: {}", p);
            }
            PlayerMsg::Choice(choice) => {
                let choice_set = match self.choice_data {
                    Some((s, ref set)) => {
                        ensure!(s == side, "not your turn to make a choice");
                        set
                    }
                    None => bail!("no choice is requested"),
                };

                ensure!(
                    choice_set.contains_index(choice),
                    "invalid choice: {} for {:?}",
                    choice,
                    choice_set
                );

                self.stop_timer(side);

                if let Some(v) = self.offered_moves.take() {
                    let p = v[choice];
                    self.make_move(side, self.stone_by_side(side), Some(p), MoveAttr::Normal);
                } else {
                    self.rule.process_choice(self, choice);
                }
                let choice_set = self.choice_data.take().unwrap().1;
                self.event(Event::Choice(side, choice_set, choice));
            }
            PlayerMsg::AcceptDrawOffer => {
                ensure!(
                    self.last_move_kind == Some(MoveAttr::DrawOffer) && side == self.cur_side,
                    "inappropriate draw offer acceptance"
                );
                self.end_draw(GameResultKind::DrawOfferAccepted);
            }
            PlayerMsg::Disconnect => self.end(GameResultKind::PlayerDisconnect, side.opposite()),
        }
        Ok(())
    }

    /// Processes a move.
    fn process_move(&mut self, side: Side, p: Option<Point>, attr: MoveAttr) -> Result<()> {
        ensure!(self.cur_side == side, "not your turn to move");
        ensure!(self.choice_data.is_none(), "make your choice before moving");

        // An offered move
        if let Some(v) = &mut self.offered_moves {
            let p = p.ok_or(anyhow!("pass when offering moves"))?;
            let int = self
                .board
                .get(p)
                .ok_or(anyhow!("moving out of board: {}", p))?;
            ensure!(
                int.is_empty(),
                "moving into an occupied intersection: {}",
                p
            );
            for it in v.iter().copied() {
                ensure!(it != p, "duplicate move offer: {}", p);
                ensure!(
                    !self.board.is_symmetrical(it, p),
                    "symmetrical move offer: {} -> {}",
                    it,
                    p
                );
            }
            // Save p as an offered move.
            v.push(p);
            // Finished
            if v.len() == v.capacity() {
                let v = v.clone();
                self.stop_timer(side);
                self.request_choice(side.opposite(), ChoiceSet::Move(v));
            }
            return Ok(());
        }

        if attr == MoveAttr::DrawOffer {
            ensure!(!self.in_opening, "draw offer in the opening");
            self.last_move_kind = Some(MoveAttr::DrawOffer);
        }

        if let Some(p) = p {
            let int = self
                .board
                .get(p)
                .ok_or(anyhow!("moving out of board: {}", p))?;
            ensure!(
                int.is_empty(),
                "moving into an occupied intersection: {}",
                p
            );
            self.last_move_kind = None;

            // Record the current stone because it'll be switched.
            let stone = self.cur_stone;

            if self.in_opening {
                self.rule
                    .process_move(self, side, p, self.board.cur_move_index() + 1)?;
            }

            self.stop_timer(side);
            self.make_move(side, stone, Some(p), attr);

            if !self.in_opening && self.strict {
                self.rule.variant().judge(self, p, side, stone);
            }
        } else {
            ensure!(!self.in_opening, "pass in the opening");
            if self.last_move_kind.is_none() {
                self.end_draw(GameResultKind::BothPass);
            }
            self.last_move_kind = None;

            self.stop_timer(side);
            self.make_move(side, self.cur_stone, None, attr);
        }
        Ok(())
    }
}
