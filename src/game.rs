/// Module for working with moves on the board.
pub mod moves;

use crate::{
    board::*,
    rule::{Rule, Variant},
};

use tokio::sync::mpsc::{self, UnboundedReceiver as Receiver, UnboundedSender as Sender};

use std::{
    any::Any,
    fmt,
    mem::{self, ManuallyDrop},
    ptr,
    sync::Arc,
    time::Duration,
};

use self::moves::*;
use self::Error::*;

macro_rules! ensure {
    ($cond:expr, $err:expr) => {
        if !$cond {
            return Err($err);
        }
    };
}

/// The side of a player.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Side {
    /// The side to move first.
    First = 0,
    /// The side to move second.
    Second = 1,
}

impl Side {
    /// Returns the opposite side.
    #[inline]
    pub const fn opposite(self) -> Side {
        match self {
            Side::First => Side::Second,
            Side::Second => Side::First,
        }
    }
}

impl fmt::Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Side::First => "Player 1",
            Side::Second => "Player 2",
        })
    }
}

/// Timing parameters.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TimingParam {
    /// Timeout of each move.
    MoveTimeout,
    /// Timeout of a game.
    GameTimeout,
    /// The time added after each move.
    TimeIncrement,
}

/// Game settings.
#[derive(Debug, Clone)]
pub enum Settings {
    /// Opening rule info.
    Rule {
        /// The unique identifier.
        id: &'static str,
        /// The variant.
        variant: Variant,
    },
    /// Timing parameter. Defaults to none if unset.
    Timing {
        /// The parameter.
        param: TimingParam,
        /// The value.
        value: Duration,
    },
    /// Board size.
    BoardSize(u32),
    /// Indicates a strict game.
    ///
    /// A strict game will end immediately when a win is possible.
    /// Otherwise, an explicit claim is required for a win.
    Strict,
    /// Moves that were originally on the board.
    Moves(Box<[(Point, Stone)]>),
}

/// Errors occurred by an invalid command.
#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {
    /// The slot at the point is occupied.
    #[error("occupied: {0}")]
    Occupied(Point),
    /// The slot at the point is out of range.
    #[error("out of range: {0}, expected: {1}")]
    OutOfRange(Point, Range),
    /// The point is out of board.
    #[error("out of board: {0}")]
    OutOfBoard(Point),
    /// The move offer is duplicate.
    #[error("duplicate offer: {0}")]
    DuplicateOffer(Point),
    /// The move offer is symmetrical to another one.
    #[error("symmetrical offer: {0} -> {1}")]
    SymmetricalOffer(Point, Point),
    /// The win claim failed.
    #[error("win claim failed: {0}")]
    FailedWinClaim(Point),
    /// The choice index is invalid.
    #[error("invalid choice: {0} in {1:?}")]
    InvalidChoice(u32, Arc<ChoiceSet>),
    /// An ill-timed command.
    #[error("ill-timed command: {0}")]
    IllTimed(&'static str),
}

/// A set of choices that a player can choose from.
#[derive(Debug, Clone)]
pub enum ChoiceSet {
    /// Messages.
    Message(&'static [&'static str]),
    /// Moves.
    Move(Box<[Point]>),
    /// Move count ranging from 1 to `max_count`.
    MoveCount {
        /// The maximum move count.
        max: u32,
    },
}

impl ChoiceSet {
    /// Checks whether an index is contained in the choice set.
    pub fn contains_index(&self, i: u32) -> bool {
        match self {
            ChoiceSet::Message(v) => i < v.len() as u32,
            ChoiceSet::Move(v) => i < v.len() as u32,
            ChoiceSet::MoveCount { max } => i >= 1 && i <= *max,
        }
    }
}

/// A message sent from the game task.
#[derive(Debug, Clone)]
pub enum Msg {
    /// Game settings.
    Settings(Arc<Vec<Settings>>),
    /// Game started.
    GameStart(Stone),
    /// Range on a board where the next move can be made.
    /// Reset to none after every move.
    MoveRange(Range),
    /// Move requested with an optional count of remaining moves to offer.
    MoveRequest(Option<u32>),
    /// Choice requested.
    ChoiceRequest(Arc<ChoiceSet>),
    /// Move made.
    Move(Option<Point>),
    /// Choice made.
    Choice(u32),
    /// Stone swapped.
    StoneSwap,
    /// Game ended.
    GameEnd(GameResult),
    /// Error occurred by the last command.
    Error(Box<Error>),
}

/// A game event.
#[derive(Debug, Clone)]
pub struct Event {
    /// The message sent.
    pub msg: Msg,
    /// The side the message is sent to, or `None` if broadcast.
    pub side: Option<Side>,
}

/// A command sent from the player task.
#[derive(Debug, Clone)]
pub enum Cmd {
    /// An actual move.
    Move(Point),
    /// A pass.
    Pass,
    /// A choice of index in the choice set provided.
    Choice(u32),
    /// Claims a win in an intersection, either by moving in or for a forbidden move.
    ClaimWin(Point),
    /// Accepts or offers a draw.
    AcceptOrOfferDraw,
    /// Disconnects when the sender is dropped.
    Disconnect,
}

/// An optionally sided command.
#[derive(Debug, Clone)]
pub struct SidedCmd {
    /// The command.
    pub cmd: Cmd,
    /// The optional side.
    pub side: Option<Side>,
}

/// A command sender.
///
/// Drop the sender to disconnect from the game.
pub struct CmdSender {
    tx: Sender<SidedCmd>,
    side: Option<Side>,
}

impl CmdSender {
    /// Splits this anonymous sender into sided senders.
    pub fn split(self) -> (CmdSender, CmdSender) {
        assert!(self.side.is_none());
        let me = ManuallyDrop::new(self);
        (
            CmdSender {
                tx: me.tx.clone(),
                side: Some(Side::First),
            },
            CmdSender {
                tx: unsafe { ptr::read(&me.tx) },
                side: Some(Side::Second),
            },
        )
    }

    /// Makes a move.
    pub fn make_move(&self, p: Point) {
        self.send(Cmd::Move(p));
    }

    /// Passes.
    pub fn pass(&self) {
        self.send(Cmd::Pass);
    }

    /// Claims a win in an intersection, either by moving in or for a forbidden move.
    pub fn claim_win(&self, p: Point) {
        self.send(Cmd::ClaimWin(p));
    }

    /// Accepts or offers a draw.
    pub fn accept_or_offer_draw(&self) {
        self.send(Cmd::AcceptOrOfferDraw);
    }

    fn send(&self, cmd: Cmd) {
        let _ = self.tx.send(SidedCmd {
            cmd,
            side: self.side,
        });
    }
}

impl Drop for CmdSender {
    fn drop(&mut self) {
        if self.side.is_some() {
            self.send(Cmd::Disconnect);
        }
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

/// The reason for the end of a game.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum GameResultKind {
    /// A row has been completed.
    RowCompleted,
    /// A forbidden move was made.
    ForbiddenMoveMade,
    /// Timeout.
    Timeout,
    /// The board is full.
    BoardFull,
    /// Both players passed.
    BothPass,
    /// A draw has been accepted.
    DrawAccepted,
    /// Player or server disconnected.
    Disconnected,
}

/// Builder for a game.
pub struct Builder {
    rule: &'static dyn Rule,
    board_size: u32,
    strict: bool,
}

impl Builder {
    /// Creates a builder with the given opening rule.
    pub fn with_rule(rule: &'static dyn Rule) -> Self {
        Builder {
            rule,
            board_size: 15,
            strict: false,
        }
    }

    /// Sets the board size.
    ///
    /// # Panics
    ///
    /// Panics if the size is not `15` for a Renju rule.
    pub fn board_size(&mut self, size: u32) -> &mut Self {
        assert!(
            self.rule.variant() != Variant::StandardRenju || size == 15,
            "Renju board must be 15x15"
        );
        self.board_size = size;
        self
    }

    /// Sets the strictness of the game.
    ///
    /// A strict game will end immediately a win is possible.
    /// Otherwise, an explicit claim is required for a win.
    pub fn strict(&mut self, strict: bool) -> &mut Self {
        self.strict = strict;
        self
    }

    /// Builds the game handle.
    pub fn build(&self) -> Handle {
        let (event_tx, event_rx) = mpsc::unbounded_channel();
        let (cmd_tx, cmd_rx) = mpsc::unbounded_channel();
        Handle {
            event_rx,
            cmd_tx: CmdSender {
                tx: cmd_tx,
                side: None,
            },
            ctrl: Control::new(self, event_tx, cmd_rx),
        }
    }
}

/// A game handle.
pub struct Handle {
    /// The event receiver.
    pub event_rx: Receiver<Event>,
    /// The command sender.
    pub cmd_tx: CmdSender,
    /// The game control.
    pub ctrl: Box<Control>,
}

enum State {
    Move,
    MoveOffer(Vec<Point>),
    Choice(Side, Arc<ChoiceSet>),
}

/// A game control.
///
/// Some methods on this struct panic if the game is not in the opening.
pub struct Control {
    /// The event sender.
    event_tx: Sender<Event>,
    /// The message senders.
    msg_txs: Option<[Sender<Msg>; 2]>,
    /// The command receiver.
    cmd_rx: Receiver<SidedCmd>,

    /// The board.
    board: Board,

    /// A static reference to the opening rule.
    rule: &'static dyn Rule,
    /// The extra data used by the opening rule.
    rule_data: Option<Box<dyn Any + Send>>,
    /// Indicates whether the game is strict.
    strict: bool,
    /// Indicates whether the game is in the opening.
    in_opening: bool,

    /// The current side.
    cur_side: Side,
    /// The current stone.
    cur_stone: Stone,

    /// The current state.
    state: State,
    request_sent: bool,
    /// The range on the board where the next move can be made.
    move_range: Option<Range>,
    /// The kind of last move.
    last_move_kind: MoveKind,
    /// The current choice index, or `0` if no choice has ever been requested.
    cur_choice_index: u16,

    /// The result of the game, or `None` if the game is not ended.
    result: Option<GameResult>,
}

impl Control {
    fn new(builder: &Builder, event_tx: Sender<Event>, cmd_rx: Receiver<SidedCmd>) -> Box<Self> {
        Box::new(Self {
            event_tx,
            msg_txs: None,
            cmd_rx,
            board: Board::new(builder.board_size),
            rule: builder.rule,
            rule_data: None,
            strict: builder.strict,
            in_opening: true,
            cur_side: Side::First,
            cur_stone: Stone::Black,
            state: State::Move,
            request_sent: false,
            move_range: None,
            last_move_kind: MoveKind::Actual,
            cur_choice_index: 0,
            result: None,
        })
    }

    /// Subscribes two split message channels from the game.
    pub fn subscribe_split(&mut self) -> (Receiver<Msg>, Receiver<Msg>) {
        let first = mpsc::unbounded_channel();
        let second = mpsc::unbounded_channel();
        self.msg_txs = Some([first.0, second.0]);
        (first.1, second.1)
    }

    /// Sends a message to one side.
    fn msg(&self, side: Side, msg: Msg) {
        let _ = self.event_tx.send(Event {
            msg: msg.clone(),
            side: Some(side),
        });
        if let Some(txs) = &self.msg_txs {
            let _ = txs[side as usize].send(msg);
        }
    }

    /// Sends a message to both sides.
    fn msg_all(&self, msg: Msg) {
        let _ = self.event_tx.send(Event {
            msg: msg.clone(),
            side: None,
        });
        if let Some(txs) = &self.msg_txs {
            let _ = txs[0].send(msg.clone());
            let _ = txs[1].send(msg);
        }
    }

    /// Returns `true` if the game is ended.
    #[inline]
    pub fn is_ended(&self) -> bool {
        self.result.is_none()
    }

    /// Returns a reference to the board.
    #[inline]
    pub fn board(&self) -> &Board {
        &self.board
    }

    /// Returns the stone of a side.
    pub fn stone_by_side(&self, side: Side) -> Stone {
        if self.cur_side == side {
            self.cur_stone
        } else {
            self.cur_stone.opposite()
        }
    }

    /// Returns the side of a stone.
    pub fn side_by_stone(&self, stone: Stone) -> Side {
        if self.cur_stone == stone {
            self.cur_side
        } else {
            self.cur_side.opposite()
        }
    }

    /// Sets the rule data.
    pub fn set_rule_data(&mut self, data: impl Any + Send) {
        self.rule_data = Some(Box::new(data));
    }

    /// Returns the rule data.
    ///
    /// # Panics
    ///
    /// Panics if the rule data is uninitialized or data types mismatch.
    pub fn rule_data<T: Any>(&mut self) -> &mut T {
        self.rule_data
            .as_deref_mut()
            .expect("uninitialized rule data")
            .downcast_mut()
            .expect("data type mismatch")
    }

    /// Ends the opening.
    pub fn end_opening(&mut self) {
        assert!(self.in_opening);
        self.in_opening = false;
        self.rule_data = None;
    }

    /// Sets the range for the next move.
    pub fn move_range(&mut self, range: i32) {
        assert!(self.in_opening);
        self.move_range = Some(Range::new(range));
    }

    /// Swaps the stones.
    pub fn swap(&mut self) {
        assert!(self.in_opening);
        self.cur_side = self.cur_side.opposite();
        self.msg_all(Msg::StoneSwap);
    }

    /// Switches the turn.
    fn switch(&mut self) {
        self.cur_side = self.cur_side.opposite();
        self.cur_stone = self.cur_stone.opposite();
    }

    /// Makes a move on the board if it is not a pass,
    /// switches the turn and broadcasts it.
    fn make_move(&mut self, stone: Stone, mov: Option<Point>) {
        if let Some(p) = mov {
            self.board.make_move(p, stone);
        }
        self.switch();
        self.msg_all(Msg::Move(mov));
    }

    /// Requests a move offer of the given `count`.
    ///
    /// This will do nothing if `count` is `1`.
    pub fn request_move_offer(&mut self, count: usize) {
        assert!(self.in_opening && count != 0);
        if count != 1 {
            self.state = State::MoveOffer(Vec::with_capacity(count));
        }
    }

    /// Requests a choice.
    pub fn request_choice(&mut self, side: Side, choice_set: ChoiceSet) {
        assert!(self.in_opening);
        self._request_choice(side, choice_set);
    }

    /// Internal function for requesting a choice without checking the opening status.
    fn _request_choice(&mut self, side: Side, choice_set: ChoiceSet) {
        self.state = State::Choice(side, Arc::new(choice_set));
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

    /// Starts the game.
    pub async fn start(mut self: Box<Self>) -> GameResult {
        // Broadcast the game settings.
        let mut settings = Vec::with_capacity(3);

        settings.push(Settings::Rule {
            id: self.rule.id(),
            variant: self.rule.variant(),
        });
        settings.push(Settings::BoardSize(self.board.size()));

        if self.strict {
            settings.push(Settings::Strict);
        }

        self.msg_all(Msg::Settings(Arc::new(settings)));

        for side in [Side::First, Side::Second] {
            self.msg(side, Msg::GameStart(self.stone_by_side(side)));
        }

        // Initialize the game with the opening rule.
        self.rule.init(&mut self);

        // Loop until the game is ended.
        while !self.is_ended() {
            if self.board.is_full() {
                // End the game for a full board.
                self.end_draw(GameResultKind::BoardFull);
                break;
            }

            // Request a choice before a move.
            let side = match self.state {
                State::Choice(side, _) => side,
                _ => self.cur_side,
            };

            // TODO: Calculate timeout.
            if !self.request_sent {
                self.send_request();
            }

            if let Some(cmd) = self.cmd_rx.recv().await {
                // If sent anonymously, a command should belong to the side requested.
                let cmd_side = cmd.side.unwrap_or(side);
                if let Err(e) = self.process_cmd(cmd_side, cmd.cmd) {
                    self.msg(cmd_side, Msg::Error(Box::new(e)));
                }
            } else {
                self.end_draw(GameResultKind::Disconnected)
            }
        }

        // Broadcast the result and goodbye.
        let result = self.result.unwrap();
        self.msg_all(Msg::GameEnd(result));
        result
    }

    fn send_request(&mut self) {
        if let State::Choice(side, ref set) = self.state {
            self.msg(side, Msg::ChoiceRequest(set.clone()))
        } else {
            // Request a move.
            let remaining = match &self.state {
                State::MoveOffer(v) => Some((v.capacity() - v.len()) as u32),
                _ => None,
            };
            if let Some(range) = self.move_range {
                self.msg(self.cur_side, Msg::MoveRange(range));
            }
            self.msg(self.cur_side, Msg::MoveRequest(remaining));
        }
        self.request_sent = true;
    }

    fn process_cmd(&mut self, side: Side, cmd: Cmd) -> Result<(), Error> {
        match cmd {
            Cmd::Move(p) => self.process_move(side, Some(p))?,
            Cmd::Pass => self.process_move(side, None)?,
            Cmd::ClaimWin(p) => {
                ensure!(
                    !self.strict,
                    IllTimed("win claim is unavailable in strict mode")
                );
                ensure!(!self.in_opening, IllTimed("win claim in the opening"));

                let slot = self.board.get(p).ok_or(OutOfBoard(p))?;
                if slot.is_empty() {
                    self.process_move(side, Some(p))?;
                }

                self.rule.variant().judge(self, p, self.stone_by_side(side));
                ensure!(self.is_ended(), FailedWinClaim(p));
            }
            Cmd::Choice(choice) => {
                let set = if let State::Choice(s, ref set) = self.state {
                    ensure!(s == side, IllTimed("not your turn to make a choice"));
                    set
                } else {
                    return Err(IllTimed("no choice is requested"));
                };

                ensure!(
                    set.contains_index(choice),
                    InvalidChoice(choice, set.clone())
                );
                self.msg_all(Msg::Choice(choice));

                if let ChoiceSet::Move(moves) = &**set {
                    let p = moves[choice as usize];
                    // The move is to be made by the opponent.
                    self.make_move(self.stone_by_side(side.opposite()), Some(p));
                } else {
                    self.rule.process_choice(self, choice);
                }
                self.state = State::Move;
            }
            Cmd::AcceptOrOfferDraw => todo!(),
            Cmd::Disconnect => self.end(GameResultKind::Disconnected, side.opposite()),
        }
        Ok(())
    }

    fn process_move(&mut self, side: Side, mov: Option<Point>) -> Result<(), Error> {
        ensure!(self.cur_side == side, IllTimed("not your turn to move"));
        ensure!(
            !matches!(self.state, State::Choice(..)),
            IllTimed("make your choice before moving")
        );

        if let State::MoveOffer(saved) = &mut self.state {
            let p = mov.ok_or(IllTimed("pass when offering moves"))?;

            process_move_offer(&self.board, saved, p)?;

            if saved.len() == saved.capacity() {
                let moves = mem::take(saved).into_boxed_slice();
                self._request_choice(side.opposite(), ChoiceSet::Move(moves));
            }
            self.request_sent = false;
            return Ok(());
        }

        if let Some(p) = mov {
            let slot = self.board.get(p).ok_or(OutOfBoard(p))?;
            ensure!(slot.is_empty(), Occupied(p));

            if let Some(range) = self.move_range.take() {
                ensure!(
                    range.contains(self.board.dist_to_center(p)),
                    OutOfRange(p, range)
                );
            }

            // Record the current stone.
            let stone = self.cur_stone;
            self.make_move(stone, mov);

            if self.in_opening {
                self.rule.process_move(self, p, self.board.cur_move_index());
            } else if self.strict {
                self.rule.variant().judge(self, p, stone);
            }
        } else {
            ensure!(!self.in_opening, IllTimed("pass in the opening"));
            if self.last_move_kind == MoveKind::Pass {
                self.end_draw(GameResultKind::BothPass);
            }
            self.last_move_kind = MoveKind::Pass;

            self.make_move(self.cur_stone, mov);
        }

        self.request_sent = false;
        Ok(())
    }
}

fn process_move_offer(board: &Board, saved: &mut Vec<Point>, p: Point) -> Result<(), Error> {
    let slot = board.get(p).ok_or(OutOfBoard(p))?;
    ensure!(slot.is_empty(), Occupied(p));
    for &it in saved.iter() {
        ensure!(it != p, DuplicateOffer(p));
        ensure!(!board.is_symmetrical(it, p), SymmetricalOffer(p, it));
    }
    // Save as an offered move.
    saved.push(p);
    Ok(())
}
