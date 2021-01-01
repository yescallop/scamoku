use std::fmt;

use super::board::*;
use super::game::*;

use anyhow::*;
pub trait Rule: Send + 'static {
    fn id(&self) -> String;

    fn variant(&self) -> Variant;

    fn process_move(&self, ctrl: &mut Control, side: Side, p: Point, index: u32) -> Result<()>;

    fn process_choice(&self, ctrl: &mut Control, choice: usize);

    fn init(&self, _ctrl: &mut Control) {}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Variant {
    StandardGomoku,
    FreestyleGomoku,
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

    fn process_move(&self, _: &mut Control, _: Side, _: Point, _: u32) -> Result<()> {
        Ok(())
    }

    fn process_choice(&self, _: &mut Control, _: usize) {}

    fn init(&self, ctrl: &mut Control) {
        ctrl.end_opening();
    }
}

impl Variant {
    /// Judges the move at the given point and ends the game
    /// if there's a win for the given stone.
    pub fn judge(self, ctrl: &mut Control, p: Point, side: Side, _stone: Stone) {
        let board = ctrl.board();
        match self {
            Variant::StandardGomoku => {
                if board.longest_row_len(p) == 5 {
                    ctrl.end(GameResultKind::RowCompleted, Some(side));
                }
            }
            Variant::FreestyleGomoku => {
                if board.longest_row_len(p) >= 5 {
                    ctrl.end(GameResultKind::RowCompleted, Some(side));
                }
            }
            Variant::StandardRenju => todo!("standard renju"),
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

pub mod standard {
    pub use super::Variant::*;
    use super::*;

    pub struct Pro;

    impl Rule for Pro {
        fn id(&self) -> String {
            "pro".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, _: Side, p: Point, index: u32) -> Result<()> {
            let board = ctrl.board();
            match index {
                1 => ensure!(
                    board.chebyshev_dist_to_center(p) == 0,
                    "the first move not in the center"
                ),
                3 => {
                    ensure!(
                        board.chebyshev_dist_to_center(p) > 2,
                        "the third move inside central 5x5 area"
                    );
                    ctrl.end_opening();
                }
                _ => {}
            }
            Ok(())
        }

        fn process_choice(&self, _: &mut Control, _: usize) {}
    }

    pub struct LongPro;

    impl Rule for LongPro {
        fn id(&self) -> String {
            "long_pro".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, _: Side, p: Point, index: u32) -> Result<()> {
            let board = ctrl.board();
            match index {
                1 => ensure!(
                    board.chebyshev_dist_to_center(p) == 0,
                    "the first move not in the center"
                ),
                3 => {
                    ensure!(
                        board.chebyshev_dist_to_center(p) > 3,
                        "the third move inside central 7x7 area"
                    );
                    ctrl.end_opening();
                }
                _ => {}
            }
            Ok(())
        }

        fn process_choice(&self, _: &mut Control, _: usize) {}
    }

    pub struct Swap;

    impl Rule for Swap {
        fn id(&self) -> String {
            "swap".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, _: Side, _: Point, index: u32) -> Result<()> {
            if index == 3 {
                ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msg(vec!["choose black", "choose white"]),
                );
            } else {
                ctrl.swap();
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize) {
            if choice == 0 {
                ctrl.swap()
            }
            ctrl.end_opening();
        }
    }
    pub struct Swap2;

    impl Rule for Swap2 {
        fn id(&self) -> String {
            "swap2".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, _: Side, _: Point, index: u32) -> Result<()> {
            match index {
                3 => ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msg(vec![
                        "choose black",
                        "choose white",
                        "make 2 moves and choose color by the opponent",
                    ]),
                ),
                5 => ctrl.request_choice(
                    Side::First,
                    ChoiceSet::from_msg(vec!["choose black", "choose white"]),
                ),
                _ => ctrl.swap(),
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize) {
            if choice == 0 {
                ctrl.swap()
            }
            if choice != 2 {
                ctrl.end_opening();
            }
        }
    }
    pub struct ChineseSwap;

    impl Rule for ChineseSwap {
        fn id(&self) -> String {
            "chinese_swap".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, _: Side, _: Point, _: u32) -> Result<()> {
            ctrl.request_choice(
                Side::Second,
                ChoiceSet::from_msg(vec!["choose black", "choose white"]),
            );
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize) {
            if choice == 0 {
                ctrl.swap()
            }
            ctrl.end_opening();
        }
    }
}
