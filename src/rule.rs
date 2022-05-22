use crate::{board::*, game::*};

use std::fmt;
use Variant::*;

/// A variant of gomoku, which also serves as an opening rule.
///
/// Reference: [Wikipedia](https://en.wikipedia.org/wiki/Gomoku#Variations).
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Variant {
    /// Standard Gomoku.
    StandardGomoku = 0,
    /// Freestyle Gomoku.
    FreestyleGomoku = 1,
    /// Standard Renju.
    StandardRenju = 2,
    /// Caro.
    Caro = 3,
}

impl Variant {
    /// Judges a move on the board and ends the game if there is
    /// a win or loss for the given stone.
    pub fn judge(self, ctrl: &mut Control, p: Point, stone: Stone) {
        let side = ctrl.side_by_stone(stone);
        let board = ctrl.board();
        let stone_on_board = board[p].stone().unwrap();

        match self {
            StandardGomoku => {
                // A win requires a five.
                // Note: Any five results in a win, not necessarily the longest row on board.
                if stone_on_board == stone && board.row_len_any(p, |l| l == 5) {
                    ctrl.end(GameResultKind::RowCompleted, side);
                }
            }
            FreestyleGomoku => {
                // A win requires a five or an overline.
                if stone_on_board == stone && board.row_len_any(p, |l| l >= 5) {
                    ctrl.end(GameResultKind::RowCompleted, side);
                }
            }
            _ => (),
        }
    }
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            StandardGomoku => "Standard Gomoku",
            FreestyleGomoku => "Freestyle Gomoku",
            StandardRenju => "Standard Renju",
            Caro => "Caro",
        })
    }
}

/// A trait for opening rules.
pub trait Rule: 'static + Sync {
    /// Returns the unique identifier of the rule.
    fn id(&self) -> &str;

    /// Returns the variant of the rule.
    fn variant(&self) -> Variant;

    /// Processes a move in the opening.
    fn process_move(&self, ctrl: &mut Control, p: Point, index: u32);

    /// Processes a choice in the opening.
    fn process_choice(&self, ctrl: &mut Control, choice: u32);

    /// Initializes the game.
    fn init(&self, ctrl: &mut Control);
}

impl Rule for Variant {
    fn id(&self) -> &str {
        match self {
            StandardGomoku => "standard_gomoku",
            FreestyleGomoku => "freestyle_gomoku",
            StandardRenju => "standard_renju",
            Caro => "caro",
        }
    }

    fn variant(&self) -> Variant {
        *self
    }

    fn process_move(&self, ctrl: &mut Control, _: Point, index: u32) {
        // Standard Renju
        match index {
            1 => ctrl.move_range(2),
            2 => ctrl.move_range(3),
            _ => return ctrl.end_opening(),
        }
        ctrl.swap();
    }

    fn process_choice(&self, _: &mut Control, _: u32) {}

    fn init(&self, ctrl: &mut Control) {
        if *self == StandardRenju {
            ctrl.move_range(1);
        } else {
            ctrl.end_opening();
        }
    }
}
