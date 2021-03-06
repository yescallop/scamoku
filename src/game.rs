use crate::board::*;
use crate::rule::*;

use std::sync::mpsc::{channel, Receiver, RecvError, RecvTimeoutError, Sender};
use std::{any::Any, thread};
use std::{
    fmt,
    time::{Duration, Instant},
};

use anyhow::*;

/// A set of choices that a player can choose from.
#[derive(Debug, Clone)]
pub enum ChoiceSet {
    /// Messages.
    Message(Vec<String>),
    /// Moves.
    Move(Vec<Point>),
    /// Move count ranging from 1 to `max_count`.
    MoveCount {
        /// The maximum move count.
        max_count: usize,
    },
}

impl ChoiceSet {
    /// Creates a choice set of messages from a `Vec<&str>`.
    pub fn from_msgs(v: Vec<&str>) -> ChoiceSet {
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
    /// The side of the player to move first.
    First,
    /// The side of the player to move second.
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
    /// A draw offer has been accepted.
    DrawOfferAccepted,
}

/// A message sent through a channel from a player thread to the game thread.
#[derive(Debug, Clone)]
pub enum PlayerMsg {
    /// An actual move or a pass, optionally with a draw offer.
    Move(Option<Point>, MoveAttr),
    /// A set of offered moves.
    OfferedMoves(Vec<Point>),
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

    /// Offers a set of moves.
    ///
    /// # Panics
    /// Panics if the count of moves is less than 2.
    pub fn offer_moves(&self, v: Vec<Point>) {
        assert!(v.len() > 1, "move count less than 2");
        self.send(PlayerMsg::OfferedMoves(v))
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

/// A message sent through a channel from the game thread to a player thread.
#[derive(Debug, Clone)]
pub enum Msg {
    /// Game started with settings.
    GameStart(Settings, Side),
    /// Move requested with the optional count of remaining moves to offer.
    MoveRequest(Option<usize>),
    /// Choice requested.
    ChoiceRequest(ChoiceSet),
    /// Clock updated. A field is updated if it's `Some`.
    ClockUpdate {
        /// The deadline for next move / choice.
        deadline: Option<Instant>,
        /// The remaining time on the clock (yours, opponent's).
        remaining: Option<(Duration, Duration)>,
    },
    /// Move made.
    Move(Option<Point>, MoveAttr),
    /// Stone swapped.
    StoneSwap,
    /// Game ended with a result.
    GameEnd(GameResult),
    /// Error occurred by yourself.
    Error(String),
}

/// An event of the game.
#[derive(Debug, Clone)]
pub enum Event {
    /// Game started with settings.
    GameStart(Settings),
    /// Move requested with the optional count of remaining moves to offer.
    MoveRequest(Option<usize>),
    /// Choice requested.
    ChoiceRequest(Side, ChoiceSet),
    /// Clock updated. A field is updated if it's `Some`.
    ClockUpdate {
        /// The deadline for next move / choice.
        deadline: Option<Instant>,
        /// The remaining time on the clock (first, second).
        remaining: Option<(Duration, Duration)>,
    },
    /// Move made by a player.
    Move(Option<Point>, MoveAttr),
    /// Choice made by a player.
    Choice(usize),
    /// Stone swapped.
    StoneSwap,
    /// Game ended with a result.
    GameEnd(GameResult),
    /// Error occurred by a player.
    Error(Side, String),
}

impl Event {
    /// Creates an `Event` from a `Msg` and its side.
    pub fn from_msg(msg: Msg, side: Side) -> Event {
        match msg {
            Msg::GameStart(settings, _) => Event::GameStart(settings),
            Msg::MoveRequest(ord) => Event::MoveRequest(ord),
            Msg::ChoiceRequest(set) => Event::ChoiceRequest(side, set),
            Msg::ClockUpdate {
                deadline,
                remaining,
            } => Event::ClockUpdate {
                deadline,
                remaining: remaining.map(|t| match side {
                    Side::First => t,
                    Side::Second => (t.1, t.0),
                }),
            },
            Msg::Move(p, attr) => Event::Move(p, attr),
            Msg::StoneSwap => Event::StoneSwap,
            Msg::GameEnd(res) => Event::GameEnd(res),
            Msg::Error(msg) => Event::Error(side, msg),
        }
    }
}

/// The settings of a game.
#[derive(Debug, Clone)]
pub struct Settings {
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
    /// The time added for each move.
    pub time_added_for_each_move: Option<Duration>,
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
    time_added_for_each_move: Option<Duration>,
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
            time_added_for_each_move: None,
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

    /// Sets the timeout for each move.
    pub fn move_timeout(mut self, timeout: Duration) -> Self {
        self.move_timeout = Some(timeout);
        self
    }

    /// Sets the timeout for the game.
    pub fn game_timeout(mut self, timeout: Duration) -> Self {
        self.game_timeout = Some(timeout);
        self
    }

    /// Sets the time added for each move.
    pub fn time_added_for_each_move(mut self, time: Duration) -> Self {
        self.time_added_for_each_move = Some(time);
        self
    }

    /// Makes the game strict.
    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }

    /// Starts the game and returns a handle split between players.
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

    /// Starts the game and returns a raw handle.
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

enum State {
    PendingMove,
    PendingMoveOffer(Vec<Point>),
    PendingChoice(Side, ChoiceSet),
}

/// The clock of game.
///
/// TODO: To find a standardized rule of timing.
struct Clock {
    /// The timeout for each move.
    move_timeout: Option<Duration>,

    /// The timeout for the game.
    game_timeout: Option<Duration>,
    /// The time added for each move.
    time_added_for_each_move: Option<Duration>,
    /// The game timer, or `None` if there is no timeout for the game.
    game_timer: Option<(Duration, Duration)>,

    /// The start and deadline time of a request, or `None` if there is no timeout.
    start_deadline: Option<(Instant, Instant)>,
}

impl Clock {
    fn new(
        move_timeout: Option<Duration>,
        game_timeout: Option<Duration>,
        time_added_for_each_move: Option<Duration>,
    ) -> Self {
        Self {
            move_timeout,
            game_timeout,
            time_added_for_each_move,
            game_timer: game_timeout.map(|d| (d, d)),
            start_deadline: None,
        }
    }

    fn game_timer_for(&mut self, side: Side) -> Option<&mut Duration> {
        self.game_timer.as_mut().map(|t| match side {
            Side::First => &mut t.0,
            Side::Second => &mut t.1,
        })
    }

    fn deadline(&self) -> Option<Instant> {
        self.start_deadline.map(|x| x.1)
    }

    /// Calculates the timeout for a side.
    ///
    /// Returns a tuple of the optional timeout with a boolean
    /// indicating whether the deadline is reused.
    fn calc_timeout(&mut self, side: Side) -> (Option<Duration>, bool) {
        let now = Instant::now();
        match self.start_deadline {
            // Move or choice is being requested. Reuse the deadline.
            Some((_, deadline)) => (Some(deadline - now), true),
            None => {
                let game_timeout = self.game_timer_for(side).copied();
                // Take the minimum timeout.
                let timeout = match (game_timeout, self.move_timeout) {
                    (None, None) => None,
                    (Some(t), None) | (None, Some(t)) => Some(t),
                    (Some(t1), Some(t2)) => Some(t1.min(t2)),
                };
                let timeout = if let Some(timeout) = timeout {
                    let deadline = now + timeout;
                    self.start_deadline = Some((now, deadline));
                    Some(timeout)
                } else {
                    None
                };
                (timeout, false)
            }
        }
    }

    /// Stops the clock of a side.
    fn stop_clock(&mut self, side: Side, is_move: bool) {
        if is_move {
            if let (Some(t), Some(add)) = (self.game_timer.as_mut(), self.time_added_for_each_move)
            {
                t.0 += add;
                t.1 += add;
            }
        }

        if let Some((start, _)) = self.start_deadline.take() {
            let elapsed = start.elapsed();
            if let Some(t) = self.game_timer_for(side) {
                let remaining = t.checked_sub(elapsed).unwrap_or_default();
                *t = remaining;
            }
        }
    }
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
    /// The maximum move index on the board, which equals to `size * size - 1`.
    max_move_index: u32,

    /// A static reference to the rule.
    rule: &'static dyn Rule,
    /// The rule data.
    rule_data: Box<dyn Any>,
    /// Whether the game is in the opening.
    in_opening: bool,
    /// Whether the game is strict.
    strict: bool,

    /// The current side of the game.
    cur_side: Side,
    /// The current stone of the game.
    cur_stone: Stone,

    /// The clock.
    clock: Clock,

    /// The current choice index, or `0` if no choice has ever been requested.
    cur_choice_index: u32,

    /// The current state.
    state: State,

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
        let clock = Clock::new(
            builder.move_timeout,
            builder.game_timeout,
            builder.time_added_for_each_move,
        );
        Control {
            msg_txs,
            msg_rx,
            event_tx,
            board,
            max_move_index: size * size - 1,
            rule: builder.rule,
            rule_data: Box::new(()),
            in_opening: true,
            strict: builder.strict,
            cur_side: Side::First,
            cur_stone: Stone::Black,
            clock,
            cur_choice_index: 0,
            state: State::PendingMove,
            last_move_kind: Some(MoveAttr::Normal),
            result: None,
        }
    }

    /// Returns the board.
    pub fn board(&self) -> &Board {
        &self.board
    }

    /// Sets the rule data.
    pub fn set_rule_data(&mut self, data: impl Any) {
        self.rule_data = Box::new(data);
    }

    /// Returns the rule data.
    ///
    /// # Panics
    /// Panics if the rule data is uninitialized or data types mismatch.
    pub fn rule_data<T: Any>(&mut self) -> &mut T {
        self.rule_data.downcast_mut().expect("data type mismatch")
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
        self.rule_data = Box::new(());
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
    /// This will do nothing if `count` is `1`.
    ///
    /// # Panics
    /// Panics if the game is not in the opening, the `count` is zero, or a move offer has already been requested.
    pub fn request_move_offer(&mut self, count: usize) {
        assert!(self.in_opening && count != 0);
        if count != 1 {
            self.state = State::PendingMoveOffer(Vec::with_capacity(count));
        }
    }

    /// Requests a choice.
    ///
    /// # Panics
    /// Panics if the game is not in the opening or a choice has already been requested,
    pub fn request_choice(&mut self, side: Side, choice_set: ChoiceSet) {
        assert!(self.in_opening);
        self._request_choice(side, choice_set);
    }

    /// Internal function for requesting a choice without checking the opening status.
    #[inline]
    fn _request_choice(&mut self, side: Side, choice_set: ChoiceSet) {
        self.state = State::PendingChoice(side, choice_set);
        self.cur_choice_index += 1;
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
    fn err(&mut self, side: Side, msg: String) {
        self.msg(side, Msg::Error(msg.clone()));
        self.event(Event::Error(side, msg));
    }

    /// Switches the turn.
    fn switch(&mut self) {
        self.cur_side = self.cur_side.opposite();
        self.cur_stone = self.cur_stone.opposite();
    }

    /// Makes a move on the board if it is not a pass,
    /// switches the turn and broadcasts it.
    fn make_move(&mut self, stone: Stone, p: Option<Point>, attr: MoveAttr) {
        if let Some(p) = p {
            self.board.make_move(p, stone);
        }
        self.switch();
        self.msg_both(Msg::Move(p, attr));
        self.event(Event::Move(p, attr));
    }

    /// Starts the game.
    fn start(mut self) -> GameResult {
        // Broadcast the game settings.
        let settings = Settings {
            rule_id: self.rule.id(),
            variant: self.rule.variant(),
            board_size: self.board.size(),
            move_timeout: self.clock.move_timeout,
            game_timeout: self.clock.game_timeout,
            time_added_for_each_move: self.clock.time_added_for_each_move,
            strict: self.strict,
        };
        self.msg(Side::First, Msg::GameStart(settings.clone(), Side::First));
        self.msg(Side::Second, Msg::GameStart(settings.clone(), Side::Second));
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
            let side = match self.state {
                State::PendingChoice(side, _) => side,
                _ => self.cur_side,
            };

            // If the deadline is reused, do nothing.
            // If the deadline is updated, broadcast it.
            // And then make a request if the deadline is not reused.
            let (timeout, reused) = self.clock.calc_timeout(Side::First);

            if !reused {
                if timeout.is_some() {
                    let deadline = self.clock.deadline();
                    self.msg(
                        side,
                        Msg::ClockUpdate {
                            deadline,
                            remaining: None,
                        },
                    );
                    self.event(Event::ClockUpdate {
                        deadline,
                        remaining: None,
                    });
                }
                self.make_request();
            }

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
                        self.err(msg_side, e.to_string());
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
        if let State::PendingChoice(side, ref set) = self.state {
            let set = set.clone();
            self.msg(side, Msg::ChoiceRequest(set.clone()));
            self.event(Event::ChoiceRequest(side, set));
        } else {
            // Request a move.
            let side = self.cur_side;
            let remaining = if let State::PendingMoveOffer(v) = &self.state {
                Some(v.capacity() - v.len())
            } else {
                None
            };
            self.msg(side, Msg::MoveRequest(remaining));
            self.event(Event::MoveRequest(remaining));
        }
    }

    /// Stops the clock of a side.
    fn stop_clock(&mut self, side: Side, is_move: bool) {
        self.clock.stop_clock(side, is_move);
        if let Some(t) = self.clock.game_timer {
            self.msg_both(Msg::ClockUpdate {
                deadline: None,
                remaining: Some(match side {
                    Side::First => t,
                    Side::Second => (t.1, t.0),
                }),
            });
            self.event(Event::ClockUpdate {
                deadline: None,
                remaining: Some(t),
            });
        }
    }

    /// Processes a message.
    fn process_msg(&mut self, side: Side, msg: PlayerMsg) -> Result<()> {
        match msg {
            PlayerMsg::Move(p, attr) => self.process_move(side, p, attr)?,
            PlayerMsg::OfferedMoves(offered) => {
                if let State::PendingMoveOffer(v) = &mut self.state {
                    ensure!(v.is_empty(), "some move has been offered alone");
                    ensure!(v.capacity() == offered.len(), "move count mismatch");

                    for &p in &offered {
                        if let Err(e) = Self::process_move_offer(&self.board, v, p) {
                            v.clear();
                            return Err(e);
                        }
                    }

                    self.stop_clock(side, false);
                    self._request_choice(side.opposite(), ChoiceSet::Move(offered));
                } else {
                    bail!("no move offer requested");
                }
            }
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
                let set = if let State::PendingChoice(s, ref set) = self.state {
                    ensure!(s == side, "not your turn to make a choice");
                    set
                } else {
                    bail!("no choice is requested")
                };

                ensure!(
                    set.contains_index(choice),
                    "invalid choice: {} for {:?}",
                    choice,
                    set
                );

                self.event(Event::Choice(choice));

                if let ChoiceSet::Move(v) = set {
                    let p = v[choice];
                    self.make_move(
                        // The move is to be made by the opponent.
                        self.stone_by_side(side.opposite()),
                        Some(p),
                        MoveAttr::Normal,
                    );

                    self.stop_clock(side, true);
                    self.state = State::PendingMove;
                } else {
                    self.stop_clock(side, false);
                    self.state = State::PendingMove;
                    self.rule.process_choice(self, choice);
                }
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
        ensure!(
            !matches!(self.state, State::PendingChoice(..)),
            "make your choice before moving"
        );

        // An offered move
        if let State::PendingMoveOffer(v) = &mut self.state {
            let p = p.ok_or(anyhow!("pass when offering moves"))?;

            Self::process_move_offer(&self.board, v, p)?;

            if v.len() == v.capacity() {
                // Finished
                let v = v.clone();
                self.stop_clock(side, false);
                self._request_choice(side.opposite(), ChoiceSet::Move(v));
            }
            return Ok(());
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

            self.last_move_kind = if attr == MoveAttr::DrawOffer {
                ensure!(!self.in_opening, "draw offer in the opening");
                Some(MoveAttr::DrawOffer)
            } else {
                Some(MoveAttr::Normal)
            };

            // Record the current stone and whether the game is in opening
            // because they could be changed.
            let stone = self.cur_stone;
            let in_opening = self.in_opening;

            if in_opening {
                self.rule
                    .process_move(self, p, self.board.cur_move_index() + 1)?;
            }

            self.stop_clock(side, true);
            self.make_move(stone, Some(p), attr);

            if !in_opening && self.strict {
                self.rule.variant().judge(self, p, side, stone);
            }
        } else {
            ensure!(!self.in_opening, "pass in the opening");
            if self.last_move_kind.is_none() {
                self.end_draw(GameResultKind::BothPass);
            }
            self.last_move_kind = None;

            self.stop_clock(side, true);
            self.make_move(self.cur_stone, None, attr);
        }
        Ok(())
    }

    fn process_move_offer(board: &Board, v: &mut Vec<Point>, p: Point) -> Result<()> {
        let int = board.get(p).ok_or(anyhow!("moving out of board: {}", p))?;
        ensure!(
            int.is_empty(),
            "moving into an occupied intersection: {}",
            p
        );
        for &it in v.iter() {
            ensure!(it != p, "duplicate move offer: {}", p);
            ensure!(
                !board.is_symmetrical(it, p),
                "symmetrical move offer: {} -> {}",
                it,
                p
            );
        }
        // Save p as an offered move.
        v.push(p);
        Ok(())
    }
}

/// An update of game that can be recorded on a `Record`.
pub trait Recordable: Sized + Clone {
    /// Records on a `Record`.
    fn record(&self, rec: &mut Record<Self>);
    /// Returns `true` if the update represents an error.
    fn is_error(&self) -> bool;
}

impl Recordable for Msg {
    fn record(&self, rec: &mut Record<Self>) {
        match *self {
            Msg::Move(p, attr) => rec.make_move(p, attr),
            Msg::ChoiceRequest(ref set) => {
                rec.last_choice_data = Some((rec.side.unwrap(), set.clone()));
            }
            Msg::StoneSwap => rec.swap(),
            _ => (),
        }
    }

    fn is_error(&self) -> bool {
        matches!(self, Msg::Error(_))
    }
}

impl Recordable for Event {
    fn record(&self, rec: &mut Record<Self>) {
        match *self {
            Event::Move(p, attr) => rec.make_move(p, attr),
            Event::ChoiceRequest(side, ref set) => {
                rec.last_choice_data = Some((side, set.clone()));
            }
            Event::StoneSwap => rec.swap(),
            _ => (),
        }
    }

    fn is_error(&self) -> bool {
        matches!(self, Event::Error(..))
    }
}

/// The record of a game, updated by either `Event`s or `Msg`'s.
pub struct Record<T: Recordable> {
    settings: Settings,
    side: Option<Side>,
    tx: Receiver<T>,
    board: Board,
    last_move_attr: MoveAttr,
    last_choice_data: Option<(Side, ChoiceSet)>,
    last_side: Side,
    last_stone: Stone,
    last_non_error: Option<T>,
}

impl<T: Recordable> Record<T> {
    fn new(settings: Settings, side: Option<Side>, tx: Receiver<T>) -> Record<T> {
        let size = settings.board_size;
        Record {
            settings,
            side,
            tx,
            board: Board::new(size),
            last_choice_data: None,
            last_move_attr: MoveAttr::Normal,
            last_side: Side::Second,
            last_stone: Stone::White,
            last_non_error: None,
        }
    }

    /// Updates the record and returns the update.
    pub fn update(&mut self) -> Result<T, RecvError> {
        let item = self.tx.recv()?;
        item.record(self);
        if !item.is_error() {
            self.last_non_error = Some(item.clone());
        }
        Ok(item)
    }

    /// Returns the game settings.
    pub fn settings(&self) -> &Settings {
        &self.settings
    }

    /// Returns a reference to the board.
    pub fn board(&self) -> &Board {
        &self.board
    }

    /// Returns the last choice data.
    ///
    /// # Panics
    /// Panics if no choice has been requested.
    pub fn last_choice_data(&self) -> (Side, &ChoiceSet) {
        match self.last_choice_data {
            Some((side, ref set)) => (side, set),
            None => panic!("no choice requested"),
        }
    }

    /// Returns the last move attribute.
    pub fn last_move_attr(&self) -> MoveAttr {
        self.last_move_attr
    }

    /// Returns the last side to move.
    pub fn last_side(&self) -> Side {
        self.last_side
    }

    /// Returns the last stone type to move.
    pub fn last_stone(&self) -> Stone {
        self.last_stone
    }

    /// Returns the last non-error update.
    pub fn last_non_error(&self) -> Option<&T> {
        self.last_non_error.as_ref()
    }

    /// Returns the stone of a side.
    pub fn stone_by_side(&self, side: Side) -> Stone {
        if self.last_side == side {
            self.last_stone
        } else {
            self.last_stone.opposite()
        }
    }

    /// Returns the side with a type of stone.
    pub fn side_by_stone(&self, stone: Stone) -> Side {
        if self.last_stone == stone {
            self.last_side
        } else {
            self.last_side.opposite()
        }
    }

    fn make_move(&mut self, p: Option<Point>, attr: MoveAttr) {
        self.last_side = self.last_side.opposite();
        self.last_stone = self.last_stone.opposite();
        if let Some(p) = p {
            self.board.make_move(p, self.last_stone);
        }
        self.last_move_attr = attr;
    }

    fn swap(&mut self) {
        self.last_side = self.last_side.opposite();
    }
}

impl From<Receiver<Msg>> for Record<Msg> {
    /// Wraps a `Receiver<Msg>` into a `State`.
    fn from(tx: Receiver<Msg>) -> Record<Msg> {
        if let Msg::GameStart(settings, side) = tx.recv().unwrap() {
            Record::new(settings, Some(side), tx)
        } else {
            panic!("invalid receiver")
        }
    }
}

impl From<Receiver<Event>> for Record<Event> {
    /// Wraps a `Receiver<Event>` into a `State`.
    fn from(tx: Receiver<Event>) -> Record<Event> {
        if let Event::GameStart(settings) = tx.recv().unwrap() {
            Record::new(settings, None, tx)
        } else {
            panic!("invalid receiver")
        }
    }
}
