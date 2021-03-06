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

    fn process_move(&self, ctrl: &mut Control, _: Point, index: u32) -> Result<()> {
        if index == 3 {
            ctrl.request_choice(Side::Second, ChoiceSet::from_msgs(vec!["swap", "remain"]));
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

    fn process_move(&self, ctrl: &mut Control, _: Point, index: u32) -> Result<()> {
        match index {
            3 => ctrl.request_choice(
                Side::Second,
                ChoiceSet::from_msgs(vec![
                    "swap",
                    "remain",
                    "make 2 moves and choose by the opponent",
                ]),
            ),
            5 => ctrl.request_choice(Side::First, ChoiceSet::from_msgs(vec!["swap", "remain"])),
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

    fn process_move(&self, ctrl: &mut Control, _: Point, _: u32) -> Result<()> {
        ctrl.request_choice(Side::Second, ChoiceSet::from_msgs(vec!["swap", "remain"]));
        Ok(())
    }

    fn process_choice(&self, ctrl: &mut Control, choice: usize) {
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
            ctrl.request_choice(Side::Second, ChoiceSet::from_msgs(vec!["swap", "remain"]));
        } else {
            // index == 4
            ctrl.request_move_offer(2);
            ctrl.end_opening();
        }
        Ok(())
    }

    fn process_choice(&self, ctrl: &mut Control, choice: usize) {
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
            ctrl.request_choice(Side::First, ChoiceSet::from_msgs(vec!["swap", "remain"]));
            return Ok(());
        }
        if index != 3 {
            ctrl.swap();
        }
        Ok(())
    }

    fn process_choice(&self, ctrl: &mut Control, choice: usize) {
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

    fn process_choice(&self, ctrl: &mut Control, choice: usize) {
        if ctrl.cur_choice_index() == 1 {
            // Save the move count
            ctrl.set_rule_data(choice);
            ctrl.request_choice(Side::Second, ChoiceSet::from_msgs(vec!["swap", "remain"]));
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
            ChoiceSet::from_msgs(vec!["swap", "remain"]),
        );
        if index == 5 {
            ctrl.end_opening();
        }
        Ok(())
    }

    fn process_choice(&self, ctrl: &mut Control, choice: usize) {
        if choice == 0 {
            ctrl.swap();
        }
    }
}

pub struct Taraguchi(pub u32);

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
                    "swap".into(),
                    "remain".into(),
                    format!("offer {} moves", self.0),
                ]),
            );
        } else if index == 5 {
            ensure!(d <= 4, "the fifth move outside central 9x9 area");
        }
        ctrl.request_choice(
            ctrl.cur_side().opposite(),
            ChoiceSet::from_msgs(vec!["swap", "remain"]),
        );
        if index == 5 {
            ctrl.end_opening();
        }
        Ok(())
    }

    fn process_choice(&self, ctrl: &mut Control, choice: usize) {
        if ctrl.cur_choice_index() == 4 && choice == 2 {
            ctrl.request_move_offer(self.0 as usize);
        }
        if choice == 0 {
            ctrl.swap();
        }
    }

    fn init(&self, _: &mut Control) {
        assert!(self.0 != 0);
    }
}

pub struct Soosyrv(pub u32);

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
            ctrl.request_choice(Side::Second, ChoiceSet::from_msgs(vec!["swap", "remain"]));
        } else {
            // index == 4
            ctrl.request_choice(
                ctrl.cur_side(),
                ChoiceSet::MoveCount {
                    max_count: self.0 as usize,
                },
            );
        }
        Ok(())
    }

    fn process_choice(&self, ctrl: &mut Control, choice: usize) {
        let index = ctrl.cur_choice_index();
        if index == 2 {
            // Save the move count
            ctrl.set_rule_data(choice);
            return ctrl.request_choice(Side::Second, ChoiceSet::from_msgs(vec!["swap", "remain"]));
        }
        if choice == 0 {
            ctrl.swap();
        }
        if index == 3 {
            let count = *ctrl.rule_data();
            ctrl.request_move_offer(count);
            ctrl.end_opening();
        }
    }

    fn init(&self, _: &mut Control) {
        assert!(self.0 != 0);
    }
}
