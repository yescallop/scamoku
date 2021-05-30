/// The renju utilities.
pub mod renju;
/// The standard rules.
pub mod standard;

use crate::{board::*, game::*};
use anyhow::*;
use std::fmt;

use self::renju::ShapesKind;

/// An interface for a rule.
pub trait Rule: Sync {
    /// Returns the ID of the rule.
    fn id(&self) -> String;

    /// Returns the variant of the rule.
    fn variant(&self) -> Variant;

    /// Processes a move.
    fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()>;

    /// Processes a choice.
    fn process_choice(&self, ctrl: &mut Control, choice: usize);

    /// Initiates the game.
    fn init(&self, _ctrl: &mut Control) {}
}

/// A variant of gomoku, which also serves as a rule.
///
/// Reference: [Wikipedia](https://en.wikipedia.org/wiki/Gomoku#Variations)
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Variant {
    /// Standard Gomoku
    StandardGomoku,
    /// Freestyle Gomoku
    FreestyleGomoku,
    /// Standard Renju
    StandardRenju,
}

impl Rule for Variant {
    fn id(&self) -> String {
        match *self {
            Variant::StandardGomoku => "standard_gomoku",
            Variant::FreestyleGomoku => "freestyle_gomoku",
            Variant::StandardRenju => "standard_renju",
        }
        .into()
    }

    fn variant(&self) -> Variant {
        *self
    }

    fn process_move(&self, _: &mut Control, _: Point, _: u32) -> Result<()> {
        Ok(())
    }

    fn process_choice(&self, _: &mut Control, _: usize) {}

    fn init(&self, ctrl: &mut Control) {
        ctrl.end_opening();
    }
}

impl Variant {
    /// Judges the move at a point and ends the game
    /// if there's a win or loss for the given stone.
    ///
    /// TODO:
    /// 1) Improve the process to prohibit the abuse of win claims.
    ///
    ///    Possible solution: require the claimer to specify the kind of claim and limit the count of claims;
    /// 2) Add forbidden move messages in the result.
    pub fn judge(self, ctrl: &mut Control, p: Point, side: Side, stone: Stone) {
        let board = ctrl.board();
        let stone_on_board = board[p].stone().unwrap();
        match self {
            Variant::StandardGomoku => {
                // A win requires a five.
                // Note: Any five results in a win, not necessarily the longest row on board.
                if stone_on_board == stone && board.row_len_iter(p).unwrap().any(|l| l == 5) {
                    ctrl.end(GameResultKind::RowCompleted, side);
                }
            }
            Variant::FreestyleGomoku => {
                // A win requires a five or an overline.
                if stone_on_board == stone && board.row_len_iter(p).unwrap().any(|l| l >= 5) {
                    ctrl.end(GameResultKind::RowCompleted, side);
                }
            }
            Variant::StandardRenju => {
                match (stone_on_board, stone) {
                    // For black:
                    // 1) A win requires a five;
                    // 2) A loss (only in strict games) requires a forbidden move.
                    (Stone::Black, Stone::Black) => {
                        if ctrl.strict() {
                            match renju::identify_shapes_at(board, p) {
                                ShapesKind::Win => ctrl.end(GameResultKind::RowCompleted, side),
                                ShapesKind::Forbidden => {
                                    ctrl.end(GameResultKind::ForbiddenMoveMade, side.opposite())
                                }
                                ShapesKind::None => {}
                            }
                        } else if board.row_len_iter(p).unwrap().any(|l| l == 5) {
                            ctrl.end(GameResultKind::RowCompleted, side);
                        }
                    }
                    // For white:
                    // 1) A win requires any forbidden move when judging the last move,
                    //    or specifically an overline when not;
                    // 2) No loss in this case.
                    (Stone::Black, Stone::White) => {
                        if board[p].move_index() == board.cur_move_index() {
                            if renju::identify_shapes_at(board, p).is_forbidden() {
                                ctrl.end(GameResultKind::ForbiddenMoveMade, side);
                            }
                        } else if board.row_len_iter(p).unwrap().any(|l| l > 5) {
                            ctrl.end(GameResultKind::ForbiddenMoveMade, side);
                        }
                    }
                    // For white:
                    // 1) A win requires a five or an overline;
                    // 2) No loss in this case.
                    (Stone::White, Stone::White) => {
                        if board.row_len_iter(p).unwrap().any(|l| l >= 5) {
                            ctrl.end(GameResultKind::RowCompleted, side);
                        }
                    }
                    // White is never judged by black.
                    (Stone::White, Stone::Black) => {}
                }
            }
        }
    }
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Variant::StandardGomoku => "Standard Gomoku",
            Variant::FreestyleGomoku => "Freestyle Gomoku",
            Variant::StandardRenju => "Standard Renju",
        }
        .fmt(f)
    }
}

const ILLEGAL_MOVE_MESSAGES: [&'static str; 3] = [
    "the first move not in the center",
    "the second move outside central 3x3 area",
    "the third move outside central 5x5 area",
];

fn validate_standard_opening(ctrl: &Control, p: Point, index: u32) -> Result<u32> {
    let d = ctrl.board().chebyshev_dist_to_center(p);
    if index <= 3 {
        ensure!(d < index, ILLEGAL_MOVE_MESSAGES[index as usize - 1]);
    }
    Ok(d)
}
