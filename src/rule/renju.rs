use crate::{board::*, game::*, rule::*};
use StoneShape::*;

/// The maximum depth for checking forbidden moves.
const FORBIDDEN_MOVE_MAX_DEPTH: u32 = 5;

/// Various stone shapes.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum StoneShape {
    Overline,
    BrokenOverline,
    Five,
    Four,
    OpenFour,
    SemiOpenFour,
    Three,
    OpenThree,
    SemiOpenThree,
    Two,
    OpenTwo,
    SemiOpenTwo,
}

impl StoneShape {
    /// Returns the stone shape of the given length and openness.
    ///
    /// Note: Only lengths from 2 to 4 are implemented now.
    pub fn of_length(len: u32, open: bool) -> StoneShape {
        if open {
            match len {
                2 => OpenTwo,
                3 => OpenThree,
                4 => OpenFour,
                _ => unimplemented!(),
            }
        } else {
            match len {
                2 => SemiOpenTwo,
                3 => SemiOpenThree,
                4 => SemiOpenFour,
                _ => unimplemented!(),
            }
        }
    }
}

/// The kind of a set of shapes.
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum ShapesKind {
    /// A win (five).
    Win,
    /// A forbidden move.
    Forbidden,
    /// None.
    None,
}

impl ShapesKind {
    /// Returns `true` if the kind is a forbidden move.
    pub fn is_forbidden(self) -> bool {
        self == ShapesKind::Forbidden
    }
}

/// Identifies the stone shapes as a win, a forbidden move or none.
pub fn identify_shapes(shapes: &[StoneShape]) -> ShapesKind {
    if shapes.contains(&Five) {
        return ShapesKind::Win;
    }
    let (mut open_threes, mut fours) = (0u32, 0u32);
    for s in shapes {
        match s {
            Overline => return ShapesKind::Forbidden,
            OpenThree => open_threes += 1,
            Four => fours += 1,
            _ => (),
        }
    }
    if open_threes >= 2 || fours >= 2 {
        ShapesKind::Forbidden
    } else {
        ShapesKind::None
    }
}

/// Identifies the stone shapes at a point on the board.
pub fn identify_shapes_at(board: &Board, p: Point) -> ShapesKind {
    identify_shapes(&search_shapes(board, p))
}

/// Searches for stone shapes at a point on the board.
pub fn search_shapes(board: &Board, p: Point) -> Vec<StoneShape> {
    todo!()
}
