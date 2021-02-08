use std::{
    fmt,
    ops::{Index, IndexMut},
    str::FromStr,
};
use Direction::*;

/// A 2D point with `u32` coordinates.
#[derive(Eq, PartialEq, Copy, Clone)]
pub struct Point {
    pub x: u32,
    pub y: u32,
}

impl Point {
    /// Creates a new `Point` with the given coordinates.
    pub fn new(x: u32, y: u32) -> Point {
        Point { x, y }
    }
}

impl From<(u32, u32)> for Point {
    fn from(t: (u32, u32)) -> Self {
        Point::new(t.0, t.1)
    }
}

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

use anyhow::{ensure, Context};

impl FromStr for Point {
    type Err = anyhow::Error;
    /// Parses a point reference, e.g. C5, ab12, etc., into a `Point`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let bytes = s.as_bytes();
        let len = bytes.len();
        ensure!(len >= 2, "length < 2");

        let mut x = 0;

        let mut i = 0;
        while i < len {
            let b = bytes[i];
            let n = match b {
                b'a'..=b'z' => b - b'a',
                b'A'..=b'Z' => b - b'A',
                _ => {
                    ensure!(x != 0, "invalid column");
                    break;
                }
            } as u32;

            x *= 26;
            x += n + 1;
            i += 1;
        }
        let x = x - 1;
        let y = s[i..].parse::<u32>().context("invalid row")? - 1;
        Ok(Point { x, y })
    }
}

impl fmt::Display for Point {
    /// Formats a `Point` as a point reference.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.x < 26 {
            // Avoid unnecessary calculations for x < 26
            return write!(f, "{}{}", (b'A' + self.x as u8) as char, self.y + 1);
        }

        let mut v = Vec::with_capacity(2);
        let mut x = self.x;
        loop {
            v.push(b'A' + (x % 26) as u8);
            x /= 26;
            if x == 0 {
                break;
            }
            x -= 1;
        }
        v.reverse();
        write!(
            f,
            "{}{}",
            // SAFETY: The bytes are all ASCII thus valid UTF-8 and reversible.
            unsafe { String::from_utf8_unchecked(v) },
            self.y + 1
        )
    }
}

/// An intersection on the board.
#[derive(Debug, Copy, Clone, Default)]
pub struct Intersection {
    /// The stone type in the intersection, or `None` if empty.
    stone: Option<Stone>,
    /// The move index of the intersection, or `0` if empty.
    move_index: u32,
}

impl Intersection {
    /// Returns the stone type in the intersection, or `None` if empty.
    pub fn stone(&self) -> Option<Stone> {
        self.stone
    }

    /// Returns `true` if the intersection is empty.
    pub fn is_empty(&self) -> bool {
        self.stone.is_none()
    }

    /// Returns the move index of the intersection starting from 1, or `0` if empty.
    pub fn move_index(&self) -> u32 {
        self.move_index
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

/// A square gomoku board.
#[derive(Debug, Clone)]
pub struct Board {
    size: u32,
    ints: Vec<Intersection>,
    cur_move_index: u32,
}

impl Board {
    /// Creates a new `Board` with the given `size`.
    ///
    /// # Panics
    /// Panics if the `size` is less than 5 or even.
    pub fn new(size: u32) -> Self {
        assert!(size >= 5, "size < 5");
        assert!(size % 2 == 1, "even size: {}", size);
        Board {
            size,
            ints: vec![Default::default(); (size * size) as usize],
            cur_move_index: 0,
        }
    }

    /// Returns the size of the board, that is, the number of columns or rows on the board.
    pub fn size(&self) -> u32 {
        self.size
    }

    /// Returns the current move index of the board, or `0` if the board is empty.
    pub fn cur_move_index(&self) -> u32 {
        self.cur_move_index
    }

    /// Returns the index of an intersection in `ints`,
    /// or `None` if the given point is out of board.
    fn index_at(&self, p: Point) -> Option<usize> {
        let size = self.size;
        if p.x < size && p.y < size {
            Some((p.y * size + p.x) as usize)
        } else {
            None
        }
    }

    /// Returns a reference to an intersection,
    /// or `None` if the given point is out of board.
    pub fn get(&self, p: Point) -> Option<&Intersection> {
        self.index_at(p).map(|i| &self.ints[i])
    }

    /// Returns a mutable reference to an intersection,
    /// or `None` if the given point is out of board.
    pub fn get_mut(&mut self, p: Point) -> Option<&mut Intersection> {
        match self.index_at(p) {
            Some(i) => Some(&mut self.ints[i]),
            None => None,
        }
    }
}

impl Default for Board {
    /// Returns a default board of size 15 (Renju Board).
    fn default() -> Self {
        Board::new(15)
    }
}

impl Index<Point> for Board {
    type Output = Intersection;

    fn index(&self, i: Point) -> &Self::Output {
        self.get(i).expect("index out of bounds")
    }
}

impl IndexMut<Point> for Board {
    fn index_mut(&mut self, i: Point) -> &mut Self::Output {
        self.get_mut(i).expect("index out of bounds")
    }
}

/// A direction on the board.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

/// The extensions for a board.
pub trait BoardExt {
    /// Makes a move on the board.
    ///
    /// # Panics
    /// Panics when moving out of board or into an occupied intersection.
    fn make_move(&mut self, p: Point, stone: Stone);

    /// Tests if two points are symmetrical on the board.
    ///
    /// # Panics
    /// Panics if any of the points is out of board.
    fn is_symmetrical(&self, p1: Point, p2: Point) -> bool;

    /// Returns the adjacent point on the given direction of a point.
    ///
    /// `None` is returned when reaching out of board.
    ///
    /// # Panics
    /// Panics if the point is out of board.
    fn adjacent(&self, p: Point, d: Direction) -> Option<Point>;

    /// Returns the length of the longest row at a point, or `0` if empty.
    ///
    /// # Panics
    /// Panics if the point is out of board.
    fn longest_row_len(&self, p: Point) -> u32;

    /// Returns the [Chebyshev distance][1] from a point to the board center.
    ///
    /// [1]: https://en.wikipedia.org/wiki/Chebyshev_distance
    ///
    /// # Panics
    /// Panics if the point is out of board.
    fn chebyshev_dist_to_center(&self, p: Point) -> u32;
}

impl BoardExt for Board {
    fn make_move(&mut self, p: Point, stone: Stone) {
        let index = self.cur_move_index + 1;
        self.cur_move_index = index;

        let int = self.get_mut(p).expect("moving out of board");
        assert!(int.is_empty(), "moving into an occupied intersection");
        int.stone = Some(stone);
        int.move_index = index;
    }

    fn is_symmetrical(&self, p1: Point, p2: Point) -> bool {
        let size = self.size();
        let (x1, y1, x2, y2) = (p1.x, p1.y, p2.x, p2.y);
        assert!(
            x1 < size && y1 < size && x2 < size && y2 < size,
            "out of board"
        );

        if x1 == x2 {
            y1 + y2 + 1 == size
        } else if x1 + x2 + 1 == size {
            y1 == y2 || y1 + y2 + 1 == size
        } else {
            false
        }
    }

    fn adjacent(&self, p: Point, d: Direction) -> Option<Point> {
        let (x, y) = (p.x, p.y);
        let size = self.size();
        assert!(x < size && y < size, "out of board");

        let res = match d {
            Up => (Some(x), Some(y + 1)),
            UpRight => (Some(x + 1), Some(y + 1)),
            Right => (Some(x + 1), Some(y)),
            DownRight => (Some(x + 1), y.checked_sub(1)),
            Down => (Some(x), y.checked_sub(1)),
            DownLeft => (x.checked_sub(1), y.checked_sub(1)),
            Left => (x.checked_sub(1), Some(y)),
            UpLeft => (x.checked_sub(1), Some(y + 1)),
        };
        let size = self.size();
        match res {
            (Some(x), Some(y)) if x < size && y < size => Some(Point { x, y }),
            _ => None,
        }
    }

    fn longest_row_len(&self, p: Point) -> u32 {
        /// Calculates the row length on the given direction at a point.
        fn row_len(board: &Board, p: Point, stone: Stone, d: Direction) -> u32 {
            let mut res = 1;

            for cur_d in [d, d.opposite()].iter().copied() {
                let mut cur_p = p;
                loop {
                    // Should panic here if out of board.
                    cur_p = match board.adjacent(cur_p, cur_d) {
                        Some(p) => p,
                        None => break,
                    };
                    if board[cur_p].stone == Some(stone) {
                        res += 1;
                    } else {
                        break;
                    }
                }
            }
            res
        }

        if let Some(stone) = self[p].stone {
            let mut res = 1;
            for d in [Right, UpRight, Up, UpLeft].iter().copied() {
                let len = row_len(self, p, stone, d);
                if len > res {
                    res = len;
                }
            }
            res
        } else {
            0
        }
    }

    fn chebyshev_dist_to_center(&self, p: Point) -> u32 {
        let size = self.size();
        assert!(p.x < size && p.y < size, "out of board");
        let center = ((size - 1) / 2) as i32;
        let dx = (p.x as i32 - center).abs() as u32;
        let dy = (p.y as i32 - center).abs() as u32;
        dx.max(dy)
    }
}
