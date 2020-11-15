use crate::board::*;
use crate::rule::*;

use std::{any::Any, thread};
use std::{
    fmt,
    time::{Duration, Instant},
};
use std::{
    rc::Rc,
    sync::mpsc::{channel, Receiver, RecvTimeoutError, Sender},
};

use anyhow::*;

/// A set of choices that can be made by a player.
#[derive(Debug, Clone)]
pub enum ChoiceSet {
    Message(Vec<String>),
    Move(Vec<Point>),
    MoveCount { max_count: usize },
}

impl ChoiceSet {
    pub fn from_msg(v: Vec<&str>) -> ChoiceSet {
        let v = v.into_iter().map(|s| s.to_owned()).collect();
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

    /// Returns the ordinal of the side.
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

/// Represents the result of a game.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct GameResult {
    /// The kind of the result.
    pub kind: GameResultKind,
    /// The winning side, `None` for a draw.
    pub winning_side: Option<(Side, Stone)>,
}

/// The kind of a result, that is why the game is ended.
#[derive(displaydoc::Display, Debug, Copy, Clone, Eq, PartialEq)]
pub enum GameResultKind {
    /// A row has been completed
    RowCompleted,
    /// A forbidden move was made
    ForbiddenMoveMade,
    /// Timeout
    Timeout,
    /// Unexpected error occurred
    Error,
    /// Interrupted
    Interrupt,
    /// The board is full
    BoardFull,
    /// Both players passed
    BothPass,
    /// Player disconnected
    PlayerDisconnect,
    /// Draw offer has been accepted
    DrawOfferAccepted,
}

/// A `PlayerMsg` can be sent through a channel from a player thread to the game thread.
#[derive(Debug, Copy, Clone)]
pub enum PlayerMsg {
    /// An actual move or a pass, with an attribute of a draw offer or a win claim.
    Move(Option<Point>, MoveAttr),
    /// Claims a win in an intersection, either by moving into it or for a forbidden move.
    WinClaim(Point),
    /// A choice of index in the choice set provided.
    Choice(usize),
    /// Accepts a draw offer.
    AcceptDrawOffer,
    /// Disconnects when sender is dropped.
    Disconnect,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MoveAttr {
    Normal,
    DrawOffer,
}

struct RawPlayerMsg {
    side: Side,
    inner: PlayerMsg,
}

/// A message sender for player.
///
/// Drop the sender to disconnect from the game.
pub struct PlayerMsgSender {
    side: Side,
    inner: Sender<RawPlayerMsg>,
}

impl PlayerMsgSender {
    /// Makes a move.
    pub fn make_move(&self, p: Option<Point>) {
        self.send(PlayerMsg::Move(p, MoveAttr::Normal))
    }

    /// Claims a win in an intersection, either by moving into it or for a forbidden move.
    pub fn claim_win(&self, p: Point) {
        self.send(PlayerMsg::WinClaim(p))
    }

    /// Makes a move and offers a draw.
    pub fn draw_move(&self, p: Option<Point>) {
        self.send(PlayerMsg::Move(p, MoveAttr::DrawOffer))
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
        let res = self.send(PlayerMsg::Disconnect);
        drop(res);
    }
}

#[derive(Debug, Copy, Clone)]
pub struct OfferOrd {
    pub ord: usize,
    pub total: usize,
}

#[derive(Debug, Clone)]
pub enum Msg {
    GameStart(GameSettings),
    MoveRequest(Option<OfferOrd>),
    ChoiceRequest(ChoiceSet),
    DeadlineUpdate(Instant),
    OpponentMove(Option<Point>, MoveAttr),
    StoneSwap,
    GameEnd(GameResult),
    Error(String),
}

#[derive(Debug, Clone)]
pub enum Event {
    GameStart(GameSettings),
    MoveRequest(Side, Option<OfferOrd>),
    ChoiceRequest(Side, ChoiceSet),
    DeadlineUpdate(Instant),
    PlayerMsg(Side, PlayerMsg),
    Move(Side, Stone, Option<Point>, MoveAttr),
    Choice(Side, ChoiceSet, usize),
    StoneSwap,
    GameEnd(GameResult),
    Error(Side, String),
}

#[derive(Debug, Clone)]
pub struct GameSettings {
    pub rule_id: String,
    pub variant: Variant,
    pub board_size: u32,
    pub move_timeout: Option<Duration>,
    pub game_timeout: Option<Duration>,
    pub strict: bool,
}

/// A handle for player to communicate with the game thread.
pub type PlayerHandle = (PlayerMsgSender, Receiver<Msg>);

pub struct Builder {
    rule: Box<dyn Rule>,
    board_size: u32,
    move_timeout: Option<Duration>,
    game_timeout: Option<Duration>,
    strict: bool,
}

impl Builder {
    pub fn with_rule(rule: impl Rule) -> Self {
        Builder {
            rule: Box::new(rule),
            board_size: 15,
            move_timeout: None,
            game_timeout: None,
            strict: false,
        }
    }

    pub fn board_size(mut self, size: u32) -> Self {
        assert!(
            self.rule.variant() != Variant::StandardRenju || size == 15,
            "Renju board must be 15x15"
        );
        self.board_size = size;
        self
    }

    pub fn move_timeout(mut self, timeout: Duration) -> Self {
        self.move_timeout = Some(timeout);
        self
    }

    pub fn game_timeout(mut self, timeout: Duration) -> Self {
        self.game_timeout = Some(timeout);
        self
    }

    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }

    pub fn start(self) -> Handle {
        let (p_msg_tx, msg_rx) = channel();
        let p_msg_txs = (
            PlayerMsgSender {
                side: Side::First,
                inner: p_msg_tx.clone(),
            },
            PlayerMsgSender {
                side: Side::Second,
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

        // Starts the game thread
        let join_handle =
            thread::spawn(move || Control::new(self, msg_txs, msg_rx, event_tx).start());

        Handle {
            player_handles: ((p_msg_txs.0, g_msg_rxs.0), (p_msg_txs.1, g_msg_rxs.1)),
            event_rx,
            join_handle,
        }
    }
}

pub struct Handle {
    /// The handles (first, second) that two players use to communicate with the game thread.
    pub player_handles: (PlayerHandle, PlayerHandle),
    // pub join_handle: JoinHandle<()>,
    /// The receiver of game events.
    pub event_rx: Receiver<Event>,
    pub join_handle: thread::JoinHandle<GameResult>,
    // pub interrupt_tx: Sender<()>,
}

pub fn start_with_rule(rule: impl Rule) -> Handle {
    Builder::with_rule(rule).start()
}
pub struct Control {
    msg_txs: (Sender<Msg>, Sender<Msg>),
    msg_rx: Receiver<RawPlayerMsg>,
    event_tx: Sender<Event>,

    board: Board,
    max_move_index: u32,

    rule: Rc<Box<dyn Rule>>,
    rule_data: Option<Box<dyn Any>>,
    in_opening: bool,
    strict: bool,

    cur_side: Side,
    cur_stone: Stone,

    move_timeout: Option<Duration>,
    game_timeout: Option<Duration>,
    game_timer: Option<[Duration; 2]>,

    start_deadline: Option<(Instant, Instant)>,

    cur_choice_index: u32,
    choice_data: Option<(Side, ChoiceSet)>,

    offered_moves: Option<Vec<Point>>,

    last_pass: bool,
    last_draw_offer: bool,

    result: Option<GameResult>,
}

impl Control {
    fn new(
        builder: Builder,
        msg_txs: (Sender<Msg>, Sender<Msg>),
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
            rule: Rc::new(builder.rule),
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
            last_pass: false,
            last_draw_offer: false,
            result: None,
        }
    }

    pub fn board(&self) -> &Board {
        &self.board
    }

    pub fn init_rule_data(&mut self, data: impl Any) {
        assert!(self.rule_data.is_none());
        self.rule_data = Some(Box::new(data));
    }

    pub fn rule_data<T: Any>(&mut self) -> &mut T {
        self.rule_data
            .as_deref_mut()
            .expect("rule data uninitialized")
            .downcast_mut()
            .expect("data type mismatch")
    }

    pub fn strict(&self) -> bool {
        self.strict
    }

    pub fn cur_side(&self) -> Side {
        self.cur_side
    }

    pub fn cur_stone(&self) -> Stone {
        self.cur_stone
    }

    pub fn cur_choice_index(&self) -> u32 {
        self.cur_choice_index
    }

    pub fn stone_by_side(&self, side: Side) -> Stone {
        if self.cur_side == side {
            self.cur_stone
        } else {
            self.cur_stone.opposite()
        }
    }

    pub fn side_by_stone(&self, stone: Stone) -> Side {
        if self.cur_stone == stone {
            self.cur_side
        } else {
            self.cur_side.opposite()
        }
    }

    pub fn end_opening(&mut self) {
        assert!(self.in_opening);
        self.in_opening = false;
        self.rule_data = None;
    }

    pub fn swap(&mut self) {
        assert!(self.in_opening);
        self.cur_side = self.cur_side.opposite();
        self.msg_both(Msg::StoneSwap);
        self.event(Event::StoneSwap);
    }

    pub fn request_move_offer(&mut self, count: usize) {
        assert!(self.in_opening && count != 0);
        self.offered_moves = Some(Vec::with_capacity(count));
    }

    pub fn request_choice(&mut self, side: Side, choice_set: ChoiceSet) {
        assert!(self.in_opening && self.choice_data.is_none());
        self.choice_data = Some((side, choice_set));
    }

    pub fn end(&mut self, kind: GameResultKind, winning_side: Option<Side>) {
        if self.result.is_none() {
            self.result = Some(GameResult {
                kind,
                winning_side: winning_side.map(|s| (s, self.stone_by_side(s))),
            });
        }
    }

    fn event(&self, e: Event) {
        let res = self.event_tx.send(e);
        // Ignore if event receiver is dropped.
        drop(res);
    }

    fn msg(&mut self, side: Side, msg: Msg) {
        let res = match side {
            Side::First => &self.msg_txs.0,
            Side::Second => &self.msg_txs.1,
        }
        .send(msg);
        // Ignore if message receiver is dropped.
        drop(res);
    }

    fn msg_both(&mut self, msg: Msg) {
        let res = self
            .msg_txs
            .0
            .send(msg.clone())
            .and_then(|_| self.msg_txs.1.send(msg));
        // Ignore if message receiver is dropped.
        drop(res);
    }

    fn err(&mut self, side: Side, msg: String) {
        self.msg(side, Msg::Error(msg.clone()));
        self.event(Event::Error(side, msg));
    }

    fn switch(&mut self) {
        self.cur_side = self.cur_side.opposite();
        self.cur_stone = self.cur_stone.opposite();
    }

    fn make_move(&mut self, side: Side, stone: Stone, p: Option<Point>, attr: MoveAttr) {
        if let Some(p) = p {
            self.board.make_move(p, stone);
        }
        self.switch();
        self.msg(side.opposite(), Msg::OpponentMove(p, attr));
        self.event(Event::Move(side, stone, p, attr));
    }

    fn start(mut self) -> GameResult {
        self.rule.clone().init(&mut self);

        let settings = GameSettings {
            rule_id: self.rule.id().to_string(),
            variant: self.rule.variant(),
            board_size: self.board.size(),
            move_timeout: self.move_timeout,
            game_timeout: self.game_timeout,
            strict: self.strict,
        };
        self.msg_both(Msg::GameStart(settings.clone()));
        self.event(Event::GameStart(settings));

        while self.result.is_none() {
            if self.board.cur_move_index() == self.max_move_index {
                self.end(GameResultKind::BoardFull, None);
                break;
            }

            let side = self
                .choice_data
                .as_ref()
                .map(|d| d.0)
                .unwrap_or(self.cur_side);

            let res = if let Some(t) = self.timeout_for_side(side) {
                self.msg_rx.recv_timeout(t)
            } else {
                self.msg_rx
                    .recv()
                    .map_err(|_| RecvTimeoutError::Disconnected)
            };

            match res {
                Ok(msg) => {
                    if let Err(e) = self.process_msg(msg.side, msg.inner) {
                        self.err(msg.side, e.to_string());
                    }
                }
                Err(RecvTimeoutError::Disconnected) => {
                    // Might never reach here, but we do handle it.
                    self.end(GameResultKind::PlayerDisconnect, None)
                }
                Err(RecvTimeoutError::Timeout) => {
                    self.end(GameResultKind::Timeout, Some(side.opposite()))
                }
            }
        }

        let result = self.result.unwrap();
        self.msg_both(Msg::GameEnd(result));
        self.event(Event::GameEnd(result));
        result
    }

    fn make_request(&mut self) {
        if let Some((side, choice_set)) = self.choice_data.clone() {
            self.msg(side, Msg::ChoiceRequest(choice_set.clone()));
            self.event(Event::ChoiceRequest(side, choice_set));
        } else {
            let side = self.cur_side;
            let offer_ord = if let Some(v) = &self.offered_moves {
                Some(OfferOrd {
                    ord: v.len(),
                    total: v.capacity(),
                })
            } else {
                None
            };
            self.msg(side, Msg::MoveRequest(offer_ord));
            self.event(Event::MoveRequest(side, offer_ord));
        }
    }

    fn timeout_for_side(&mut self, side: Side) -> Option<Duration> {
        let ord = side.ord();
        let now = Instant::now();
        match self.start_deadline {
            // Move or choice is already requested.
            Some((_, deadline)) => Some(deadline - now),
            None => {
                let game_timeout = self.game_timer.map(|t| t[ord]);
                let timeout = match (game_timeout, self.move_timeout) {
                    (None, None) => None,
                    (None, Some(t)) => Some(t),
                    (Some(t), None) => Some(t),
                    (Some(t1), Some(t2)) => Some(t1.min(t2)),
                };
                self.start_deadline = timeout.map(|t| (now, now + t));
                if let Some((_, deadline)) = self.start_deadline {
                    self.msg(side, Msg::DeadlineUpdate(deadline));
                    self.event(Event::DeadlineUpdate(deadline));
                }
                self.make_request();
                timeout
            }
        }
    }

    fn stop_timer(&mut self, side: Side) {
        if let Some((start, _)) = self.start_deadline.take() {
            let elapsed = start.elapsed();
            let ord = side.ord();
            if let Some(t) = &mut self.game_timer.map(|t| t[ord]) {
                *t = t.checked_sub(elapsed).unwrap_or_default()
            }
        }
    }

    fn process_msg(&mut self, side: Side, msg: PlayerMsg) -> Result<()> {
        self.event(Event::PlayerMsg(side, msg));
        match msg {
            PlayerMsg::Move(p, attr) => self.process_move(side, p, attr)?,
            PlayerMsg::WinClaim(p) => {
                ensure!(!self.strict, "win claim is not available in strict mode");
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
                if self.result.is_none() {
                    self.err(side, "win claim was unsuccessful".to_string());
                }
            }
            PlayerMsg::Choice(choice) => {
                let choice_set = &self
                    .choice_data
                    .as_ref()
                    .filter(|(s, _)| side == *s)
                    .ok_or(anyhow!("inappropriate choice"))?
                    .1;
                ensure!(
                    choice_set.contains_index(choice),
                    "illegal choice: {} for {:?}",
                    choice,
                    choice_set
                );

                self.stop_timer(side);

                if let Some(v) = self.offered_moves.take() {
                    let p = v[choice];
                    self.make_move(side, self.stone_by_side(side), Some(p), MoveAttr::Normal);
                } else {
                    self.rule.clone().process_choice(self, choice);
                }
                let choice_set = self.choice_data.take().unwrap().1;
                self.event(Event::Choice(side, choice_set, choice));
            }
            PlayerMsg::AcceptDrawOffer => {
                ensure!(
                    self.last_draw_offer && side == self.cur_side,
                    "inappropriate draw offer acceptance"
                );
                self.end(GameResultKind::DrawOfferAccepted, None);
            }
            PlayerMsg::Disconnect => {
                self.end(GameResultKind::PlayerDisconnect, Some(side.opposite()))
            }
        }
        Ok(())
    }

    fn process_move(&mut self, side: Side, p: Option<Point>, attr: MoveAttr) -> Result<()> {
        ensure!(
            self.cur_side == side && self.choice_data.is_none(),
            "inappropriate move"
        );

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
            for it in v.iter() {
                ensure!(*it != p, "duplicate move offer: {}", p);
                ensure!(
                    !self.board.is_symmetrical(*it, p),
                    "symmetrical move offer: {} -> {}",
                    it,
                    p
                );
            }
            v.push(p);
            if v.len() == v.capacity() {
                let v = v.clone();
                self.stop_timer(side);
                self.request_choice(side.opposite(), ChoiceSet::Move(v));
            }
            return Ok(());
        }

        if attr == MoveAttr::DrawOffer {
            ensure!(!self.in_opening, "move offer in the opening");
            self.last_draw_offer = true;
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
            self.last_pass = false;

            // Record the current stone because the rule might change it by swap.
            let stone = self.cur_stone;

            if self.in_opening {
                self.rule
                    .clone()
                    .process_move(self, side, p, self.board.cur_move_index() + 1)?;
            }

            self.stop_timer(side);
            self.make_move(side, stone, Some(p), attr);

            if !self.in_opening && self.strict {
                self.rule.variant().judge(self, p, side, stone);
            }
        } else {
            ensure!(!self.in_opening, "pass in the opening");
            if self.last_pass {
                self.end(GameResultKind::BothPass, None);
            }
            self.last_pass = true;

            self.stop_timer(side);
            self.make_move(side, self.cur_stone, None, attr);
        }
        Ok(())
    }
}
