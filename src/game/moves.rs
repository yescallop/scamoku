use std::{fmt, num::NonZeroI32};

/// Specifies the range on a board where a move can be made.
///
/// The range is represented as a signed non-zero integer `n`, where a
/// positive one specifies all points such that their Chebyshev distance
/// to the center is less than `n` (the center square of size `2|n| - 1`),
/// and a negative one the rest of the board.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range(NonZeroI32);

impl Range {
    /// Creates a new `Range` from an `i32` value.
    ///
    /// # Panics
    ///
    /// Panics if the value is zero.
    #[inline]
    pub fn new(value: i32) -> Range {
        Range(NonZeroI32::new(value).expect("zero value"))
    }

    /// Returns the integer representation of this `Range`.
    #[inline]
    pub fn get(self) -> i32 {
        self.0.get()
    }

    /// Returns `true` if a Chebyshev distance to the center is contained in the range.
    #[inline]
    pub fn contains(self, dist: u32) -> bool {
        let v = self.get();
        if v < 0 {
            dist >= -v as u32
        } else {
            dist < v as u32
        }
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get() {
            1 => f.write_str("center"),
            -1 => f.write_str("non-center"),
            x => {
                if x < 0 {
                    f.write_str("outside ")?;
                }
                write!(f, "center {0}x{0} square", x.unsigned_abs() * 2 - 1)
            }
        }
    }
}

/// The kind of a move made by a player.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveKind {
    /// A pass.
    Pass,
    /// An actual move on the board.
    Actual,
    /// An actual move with a draw offer.
    DrawOffer,
}
