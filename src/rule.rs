use std::fmt;

use crate::{board::*, game::*};

use anyhow::*;

/// An interface for a rule.
pub trait Rule: Send + Sync + 'static {
    /// Returns the ID of the rule.
    fn id(&self) -> String;

    /// Returns the variant of the rule.
    fn variant(&self) -> Variant;

    /// Processes a move.
    fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()>;

    /// Processes a choice.
    fn process_choice(&self, ctrl: &mut Control, choice: usize, side: Side);

    /// Initiates the game.
    fn init(&self, _ctrl: &mut Control) {}
}

/// A variant of gomoku, which also serves as a rule.
///
/// Reference: https://en.wikipedia.org/wiki/Gomoku#Variations
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

    fn process_choice(&self, _: &mut Control, _: usize, _: Side) {}

    fn init(&self, ctrl: &mut Control) {
        ctrl.end_opening();
    }
}

impl Variant {
    /// Judges the move at a point and ends the game
    /// if there's a win for the given stone.
    pub fn judge(self, ctrl: &mut Control, p: Point, side: Side, stone: Stone) {
        let board = ctrl.board();
        match self {
            Variant::StandardGomoku => {
                if board[p].stone() == Some(stone) && board.longest_row_len(p) == 5 {
                    ctrl.end(GameResultKind::RowCompleted, side);
                }
            }
            Variant::FreestyleGomoku => {
                if board[p].stone() == Some(stone) && board.longest_row_len(p) >= 5 {
                    ctrl.end(GameResultKind::RowCompleted, side);
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

/// The standard rules.
pub mod standard {
    pub use super::Variant::*;
    use super::*;

    /// A list of all available standard rules.
    pub const LIST: [&'static dyn Rule; 14] = [
        &StandardGomoku,
        &FreestyleGomoku,
        &StandardRenju,
        &Pro,
        &LongPro,
        &Swap,
        &Swap2,
        &ChineseSwap,
        &RIF,
        &Sakata,
        &Yamaguchi,
        &Tarannikov,
        &Taraguchi(5),
        &Soosyrv(8),
    ];

    pub struct Pro;

    impl Rule for Pro {
        fn id(&self) -> String {
            "pro".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            let d = ctrl.board().chebyshev_dist_to_center(p);
            match index {
                1 => ensure!(d == 0, "the first move not in the center"),
                3 => {
                    ensure!(d > 2, "the third move inside central 5x5 area");
                    ctrl.end_opening();
                }
                _ => {}
            }
            Ok(())
        }

        fn process_choice(&self, _: &mut Control, _: usize, _: Side) {}
    }

    pub struct LongPro;

    impl Rule for LongPro {
        fn id(&self) -> String {
            "long_pro".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            let d = ctrl.board().chebyshev_dist_to_center(p);
            match index {
                1 => ensure!(d == 0, "the first move not in the center"),
                3 => {
                    ensure!(d > 3, "the third move inside central 7x7 area");
                    ctrl.end_opening();
                }
                _ => {}
            }
            Ok(())
        }

        fn process_choice(&self, _: &mut Control, _: usize, _: Side) {}
    }

    pub struct Swap;

    impl Rule for Swap {
        fn id(&self) -> String {
            "swap".into()
        }

        fn variant(&self) -> Variant {
            StandardGomoku
        }

        fn process_move(&self, ctrl: &mut Control, _: Point, index: u32) -> Result<()> {
            if index == 3 {
                ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
                );
            } else {
                ctrl.swap();
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, _: Side) {
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

        fn process_move(&self, ctrl: &mut Control, _: Point, index: u32) -> Result<()> {
            match index {
                3 => ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msgs(vec![
                        "choose black",
                        "choose white",
                        "make 2 moves and choose color by the opponent",
                    ]),
                ),
                5 => ctrl.request_choice(
                    Side::First,
                    ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
                ),
                _ => ctrl.swap(),
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, _: Side) {
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

        fn process_move(&self, ctrl: &mut Control, _: Point, _: u32) -> Result<()> {
            ctrl.request_choice(
                Side::Second,
                ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
            );
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, _: Side) {
            if choice == 0 {
                ctrl.swap()
            }
            ctrl.end_opening();
        }
    }

    pub struct RIF;

    impl Rule for RIF {
        fn id(&self) -> String {
            "rif".into()
        }

        fn variant(&self) -> Variant {
            StandardRenju
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            validate_standard_opening(ctrl, p, index)?;

            if index < 3 {
                ctrl.swap();
            } else if index == 3 {
                ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
                );
            } else {
                // index == 4
                ctrl.request_move_offer(2);
                ctrl.end_opening();
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, _: Side) {
            if choice == 0 {
                ctrl.swap();
            }
        }
    }

    pub struct Sakata;

    impl Rule for Sakata {
        fn id(&self) -> String {
            "sakata".into()
        }

        fn variant(&self) -> Variant {
            StandardRenju
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            let d = validate_standard_opening(ctrl, p, index)?;
            if index == 4 {
                ensure!(d <= 3, "the fourth move outside central 7x7 area");
            } else if index == 5 {
                ensure!(d <= 4, "the fifth move outside central 9x9 area");
                ctrl.request_choice(
                    Side::First,
                    ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
                );
                return Ok(());
            }
            if index != 3 {
                ctrl.swap();
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, _: Side) {
            if choice == 0 {
                ctrl.swap();
            }
            ctrl.end_opening();
        }
    }

    pub struct Yamaguchi;

    impl Rule for Yamaguchi {
        fn id(&self) -> String {
            "yamaguchi".into()
        }

        fn variant(&self) -> Variant {
            StandardRenju
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            validate_standard_opening(ctrl, p, index)?;

            if index < 3 {
                ctrl.swap();
            } else if index == 3 {
                let size = ctrl.board().size();
                ctrl.request_choice(
                    ctrl.cur_side(),
                    ChoiceSet::MoveCount {
                        max_count: (size * size - 4) as usize,
                    },
                );
            } else {
                // index == 4
                let count = *ctrl.rule_data();
                ctrl.request_move_offer(count);
                ctrl.end_opening();
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, _: Side) {
            if ctrl.cur_choice_index() == 1 {
                // Save the move count
                ctrl.set_rule_data(choice);
                ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
                );
            } else if choice == 0 {
                ctrl.swap();
            }
        }
    }

    pub struct Tarannikov;

    impl Rule for Tarannikov {
        fn id(&self) -> String {
            "tarannikov".into()
        }

        fn variant(&self) -> Variant {
            StandardRenju
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            let d = validate_standard_opening(ctrl, p, index)?;
            if index == 4 {
                ensure!(d <= 3, "the fourth move outside central 7x7 area");
            } else if index == 5 {
                ensure!(d <= 4, "the fifth move outside central 9x9 area");
            }
            ctrl.request_choice(
                ctrl.cur_side().opposite(),
                ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
            );
            if index == 5 {
                ctrl.end_opening();
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, side: Side) {
            if (choice == 0) != (ctrl.stone_by_side(side) == Stone::Black) {
                ctrl.swap();
            }
        }
    }

    pub struct Taraguchi(u32);

    impl Taraguchi {
        pub fn with_n(n: u32) -> Self {
            assert!(n > 1);
            Self(n)
        }
    }
    impl Default for Taraguchi {
        fn default() -> Self {
            Self(5)
        }
    }

    impl Rule for Taraguchi {
        fn id(&self) -> String {
            format!("taraguchi-{}", self.0)
        }

        fn variant(&self) -> Variant {
            StandardRenju
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            let d = validate_standard_opening(ctrl, p, index)?;
            if index == 4 {
                ensure!(d <= 3, "the fourth move outside central 7x7 area");
                ctrl.request_choice(
                    ctrl.cur_side().opposite(),
                    ChoiceSet::Message(vec![
                        "choose black".into(),
                        "choose white".into(),
                        format!("offer {} moves", self.0),
                    ]),
                );
            } else if index == 5 {
                ensure!(d <= 4, "the fifth move outside central 9x9 area");
            }
            ctrl.request_choice(
                ctrl.cur_side().opposite(),
                ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
            );
            if index == 5 {
                ctrl.end_opening();
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, side: Side) {
            if ctrl.cur_choice_index() == 4 && choice == 2 {
                ctrl.request_move_offer(self.0 as usize);
            }
            if (choice == 0) != (ctrl.stone_by_side(side) == Stone::Black) {
                ctrl.swap();
            }
        }
    }

    pub struct Soosyrv(u32);

    impl Soosyrv {
        pub fn with_n(n: u32) -> Self {
            assert!(n > 1);
            Self(n)
        }
    }

    impl Default for Soosyrv {
        fn default() -> Self {
            Self(4)
        }
    }

    impl Rule for Soosyrv {
        fn id(&self) -> String {
            format!("soosyrv-{}", self.0)
        }

        fn variant(&self) -> Variant {
            StandardRenju
        }

        fn process_move(&self, ctrl: &mut Control, p: Point, index: u32) -> Result<()> {
            validate_standard_opening(ctrl, p, index)?;
            if index < 3 {
                ctrl.swap();
            } else if index == 3 {
                ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
                );
            } else {
                ctrl.request_choice(
                    ctrl.cur_side(),
                    ChoiceSet::MoveCount {
                        max_count: self.0 as usize,
                    },
                );
            }
            Ok(())
        }

        fn process_choice(&self, ctrl: &mut Control, choice: usize, side: Side) {
            let index = ctrl.cur_choice_index();
            if index == 2 {
                // Save the move count
                ctrl.set_rule_data(choice);
                return ctrl.request_choice(
                    Side::Second,
                    ChoiceSet::from_msgs(vec!["choose black", "choose white"]),
                );
            }
            if (choice == 0) != (ctrl.stone_by_side(side) == Stone::Black) {
                ctrl.swap();
            }
            if index == 3 {
                let count = *ctrl.rule_data();
                ctrl.request_move_offer(count);
                ctrl.end_opening();
            }
        }
    }
}
