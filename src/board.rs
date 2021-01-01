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
    /// Constructs a new `Point` with specific coordinates.
    pub fn new(x: u32, y: u32) -> Point {
        Point { x, y }
    }
}

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

use anyhow::ensure;

impl FromStr for Point {
    type Err = anyhow::Error;
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
                    anyhow::ensure!(x != 0, "no column");
                    break;
                }
            } as u32;

            x *= 26;
            x += n + 1;
            i += 1;
        }
        let x = x - 1;
        let y = s[i..].parse::<u32>()? - 1;
        Ok(Point { x, y })
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.x < 26 {
            // Avoid unnecessary calculations for x < 26
            return f.write_fmt(format_args!(
                "{}{}",
                (b'A' + self.x as u8) as char,
                self.y + 1
            ));
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
        f.write_fmt(format_args!(
            "{}{}",
            unsafe { String::from_utf8_unchecked(v) },
            self.y + 1
        ))
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
    pub fn stone(&self) -> Option<Stone> {
        self.stone
    }

    /// Returns `true` if the intersection is empty.
    pub fn is_empty(&self) -> bool {
        self.stone.is_none()
    }

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
///
/// Optimized for the most common Renju board (15x15).
#[derive(Debug, Clone)]
pub enum Board {
    Renju {
        ints: [Intersection; 15 * 15],
        cur_move_index: u32,
    },
    Vec {
        size: u32,
        ints: Vec<Intersection>,
        cur_move_index: u32,
    },
}

impl Board {
    /// Constructs a new `Board` with the specified size.
    pub fn new(size: u32) -> Self {
        assert!(size >= 5, "size < 5");
        assert!(size % 2 == 1, "even size: {}", size);
        if size == 15 {
            Board::Renju {
                ints: [Default::default(); 15 * 15],
                cur_move_index: 0,
            }
        } else {
            let cap = size * size;
            let mut ints = Vec::with_capacity(cap as usize);
            for _ in 0..cap {
                ints.push(Default::default());
            }
            Board::Vec {
                size,
                ints,
                cur_move_index: 0,
            }
        }
    }

    /// Returns the size of the board, that is the number of columns or rows on the board.
    pub fn size(&self) -> u32 {
        match *self {
            Board::Renju { .. } => 15,
            Board::Vec { size, .. } => size,
        }
    }

    /// Returns the current move index of the board, or `0` if the board is empty.
    pub fn cur_move_index(&self) -> u32 {
        match *self {
            Board::Renju { cur_move_index, .. } | Board::Vec { cur_move_index, .. } => {
                cur_move_index
            }
        }
    }

    /// Returns the index of an intersection in `ints`,
    /// or `None` if the given point is out of the board.
    fn index_at(&self, p: Point) -> Option<usize> {
        let size = self.size();
        if p.x < size && p.y < size {
            Some((p.y * size + p.x) as usize)
        } else {
            None
        }
    }

    /// Returns a reference to an intersection,
    /// or `None` if the given point is out of the board.
    pub fn get(&self, p: Point) -> Option<&Intersection> {
        self.index_at(p).map(|i| match self {
            Board::Renju { ints, .. } => &ints[i],
            Board::Vec { ints, .. } => &ints[i],
        })
    }

    /// Returns a mutable reference to an intersection,
    /// or `None` if the given point is out of the board.
    pub fn get_mut(&mut self, p: Point) -> Option<&mut Intersection> {
        match self.index_at(p) {
            Some(i) => Some(match self {
                Board::Renju { ints, .. } => &mut ints[i],
                Board::Vec { ints, .. } => &mut ints[i],
            }),
            None => None,
        }
    }
}

impl Default for Board {
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

pub trait BoardExt {
    fn make_move(&mut self, p: Point, stone: Stone);
    fn is_symmetrical(&self, p1: Point, p2: Point) -> bool;
    fn adjacent(&self, p: Point, d: Direction) -> Option<Point>;
    fn longest_row_len(&self, p: Point) -> u32;
    fn chebyshev_dist_to_center(&self, p: Point) -> u32;
}

impl BoardExt for Board {
    fn make_move(&mut self, p: Point, stone: Stone) {
        let index_mut = match self {
            Board::Renju { cur_move_index, .. } | Board::Vec { cur_move_index, .. } => {
                cur_move_index
            }
        };
        let index = *index_mut + 1;
        *index_mut = index;

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
        fn row_len(board: &Board, p: Point, d: Direction) -> u32 {
            let stone = board[p].stone.expect("empty intersection");
            let mut res = 1;

            for cur_d in [d, d.opposite()].iter().copied() {
                let mut cur_p = p;
                loop {
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

        let mut res = 1;
        for d in &[Right, UpRight, Up, UpLeft] {
            let len = row_len(self, p, *d);
            if len > res {
                res = len;
            }
        }
        res
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
