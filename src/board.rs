use std::{convert::TryInto, fmt, str::FromStr};
use thiserror::Error;

use Direction::*;

/// A 2D point with `u8` coordinates.
#[derive(Eq, PartialEq, Copy, Clone)]
pub struct Point {
    /// The horizontal coordinate.
    pub x: u8,
    /// The vertical coordinate.
    pub y: u8,
}

impl Point {
    /// Creates a new `Point` with the given coordinates.
    #[inline]
    pub const fn new(x: u8, y: u8) -> Point {
        Point { x, y }
    }

    /// Returns the point adjacent to this point in the given direction.
    ///
    /// `None` is returned when out of bounds.
    pub fn adjacent(self, d: Direction) -> Option<Point> {
        let (x, y) = (self.x, self.y);
        let adj = match d {
            Up => (Some(x), y.checked_add(1)),
            UpRight => (x.checked_add(1), y.checked_add(1)),
            Right => (x.checked_add(1), Some(y)),
            DownRight => (x.checked_add(1), y.checked_sub(1)),
            Down => (Some(x), y.checked_sub(1)),
            DownLeft => (x.checked_sub(1), y.checked_sub(1)),
            Left => (x.checked_sub(1), Some(y)),
            UpLeft => (x.checked_sub(1), y.checked_add(1)),
        };
        Some(Point::new(adj.0?, adj.1?))
    }
}

impl From<(u8, u8)> for Point {
    #[inline]
    fn from(t: (u8, u8)) -> Self {
        Point::new(t.0, t.1)
    }
}

impl fmt::Display for Point {
    /// Formats a `Point` as a point reference.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.x < 26 {
            return write!(f, "{}{}", (b'A' + self.x) as char, self.y as u16 + 1);
        }

        let low = self.x % 26;
        let high = self.x / 26 - 1;

        write!(
            f,
            "{}{}{}",
            (b'A' + high) as char,
            (b'A' + low) as char,
            self.y as u16 + 1
        )
    }
}

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
#[error("invalid point reference")]
/// An error parsing a point.
pub struct ParsePointError;

impl FromStr for Point {
    type Err = ParsePointError;
    /// Parses a point reference, e.g. C5, i7, into a `Point`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[inline]
        fn alpha_to_num(b: u8) -> Option<u8> {
            match b {
                b'a'..=b'z' => Some(b - b'a'),
                b'A'..=b'Z' => Some(b - b'A'),
                _ => None,
            }
        }

        let bytes = s.as_bytes();
        let len = bytes.len();
        // Range: A1 - IV256
        if len < 2 || len > 5 {
            return Err(ParsePointError);
        }

        let parse = || {
            let mut x = alpha_to_num(bytes[0])?;

            let row = if let Some(n) = alpha_to_num(bytes[1]) {
                x = (x + 1).checked_mul(26)?.checked_add(n)?;
                &s[2..]
            } else {
                &s[1..]
            };

            let y = row
                .parse::<u16>()
                .ok()
                .and_then(|n| n.checked_sub(1))
                .and_then(|n| n.try_into().ok())?;
            Some((x, y))
        };
        parse().map(Point::from).ok_or(ParsePointError)
    }
}

/// The kind of a stone, either black or white.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Stone {
    /// A black stone.
    Black,
    /// A white stone.
    White,
}

impl Stone {
    /// Returns the opposite stone.
    #[inline]
    pub fn opposite(self) -> Stone {
        match self {
            Stone::Black => Stone::White,
            Stone::White => Stone::Black,
        }
    }
}

impl fmt::Display for Stone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stone::Black => "Black",
            Stone::White => "White",
        }
        .fmt(f)
    }
}

/// A direction on the board.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum Direction {
    Up,
    UpRight,
    Right,
    DownRight,
    Down,
    DownLeft,
    Left,
    UpLeft,
}

impl Direction {
    /// Returns the opposite direction.
    #[inline]
    pub fn opposite(self) -> Direction {
        match self {
            Up => Down,
            UpRight => DownLeft,
            Right => Left,
            DownRight => UpLeft,
            Down => Up,
            DownLeft => UpRight,
            Left => Right,
            UpLeft => DownRight,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn point_fmt_parse() {
        assert_eq!(Point::new(0, 0).to_string(), "A1");
        assert_eq!(Point::new(255, 255).to_string(), "IV256");
        assert_eq!(Point::new(7, 6).to_string(), "H7");
        assert_eq!(Point::new(26, 9).to_string(), "AA10");

        assert_eq!("A1".parse(), Ok(Point::new(0, 0)));
        assert_eq!("iV256".parse(), Ok(Point::new(u8::MAX, u8::MAX)));

        for i in 0..=u8::MAX {
            let p = Point::new(i, i);
            assert_eq!(p.to_string().parse(), Ok(p));
        }

        assert!(Point::from_str("A").is_err());
        assert!(Point::from_str("1").is_err());
        assert!(Point::from_str("A0").is_err());
        assert!(Point::from_str("A257").is_err());
        assert!(Point::from_str("A65535").is_err());
        assert!(Point::from_str("IW1").is_err());
        assert!(Point::from_str("AAA1").is_err());
    }

    #[test]
    fn point_adjacent() {
        let p = Point::new(1, 1);
        assert_eq!(p.adjacent(Up), Some(Point::new(1, 2)));
        assert_eq!(p.adjacent(UpRight), Some(Point::new(2, 2)));
        assert_eq!(p.adjacent(Right), Some(Point::new(2, 1)));
        assert_eq!(p.adjacent(DownRight), Some(Point::new(2, 0)));
        assert_eq!(p.adjacent(Down), Some(Point::new(1, 0)));
        assert_eq!(p.adjacent(DownLeft), Some(Point::new(0, 0)));
        assert_eq!(p.adjacent(Left), Some(Point::new(0, 1)));
        assert_eq!(p.adjacent(UpLeft), Some(Point::new(0, 2)));

        for &d in &[DownRight, Down, DownLeft, Left, UpLeft] {
            assert!(Point::new(0, 0).adjacent(d).is_none());
        }

        for &d in &[Up, UpRight, Right, DownRight, UpLeft] {
            assert!(Point::new(255, 255).adjacent(d).is_none());
        }
    }
}
