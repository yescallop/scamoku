use crate::{board::*, game::*};

use std::fmt;
use Variant::*;

/// A variant of gomoku, which also serves as an opening rule.
///
/// Reference: [Wikipedia](https://en.wikipedia.org/wiki/Gomoku#Variations)
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

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Variant::StandardGomoku => "Standard Gomoku",
            Variant::FreestyleGomoku => "Freestyle Gomoku",
            Variant::StandardRenju => "Standard Renju",
            Variant::Caro => "Caro",
        })
    }
}

/// A trait for opening rules.
pub trait OpRule: Sync {
    /// Returns the ID of the rule;
    fn id(&self) -> &str;

    /// Returns the variant of the rule.
    fn variant(&self) -> Variant;

    /// Processes a move in the opening.
    fn process_move(&self, ctrl: &mut Control, p: Point, index: u16);

    /// Processes a choice in the opening.
    fn process_choice(&self, ctrl: &mut Control, choice: u8);

    /// Initializes the game.
    fn init(&self, ctrl: &mut Control);
}

impl OpRule for Variant {
    fn id(&self) -> &str {
        match self {
            StandardGomoku => "std_gomoku",
            FreestyleGomoku => "fst_gomoku",
            StandardRenju => "std_renju",
            Caro => "caro",
        }
    }

    fn variant(&self) -> Variant {
        *self
    }

    fn process_move(&self, ctrl: &mut Control, _: Point, index: u16) {
        // Standard Renju
        match index {
            1 => ctrl.move_range(2),
            2 => ctrl.move_range(3),
            _ => return ctrl.end_opening(),
        }
        ctrl.swap();
    }

    fn process_choice(&self, _: &mut Control, _: u8) {}

    fn init(&self, ctrl: &mut Control) {
        if *self == StandardRenju {
            ctrl.move_range(1);
        } else {
            ctrl.end_opening();
        }
    }
}
