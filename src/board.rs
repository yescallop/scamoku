use std::{
    alloc::{self, Layout},
    fmt, mem,
    ops::{Index, IndexMut},
    ptr::NonNull,
    str::{self, FromStr},
};
use thiserror::Error;

/// A stone on the board, either black or white.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum Stone {
    /// A black stone.
    Black = 1,
    /// A white stone.
    White = 2,
}

impl Stone {
    /// Returns the opposite stone.
    #[inline]
    pub const fn opposite(self) -> Stone {
        // SAFETY: 1 ^ 3 = 2, 2 ^ 3 = 1.
        unsafe { mem::transmute(self as u8 ^ 3) }
    }
}

impl fmt::Display for Stone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Stone::Black => "Black",
            Stone::White => "White",
        })
    }
}

/// Axes on the board.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Axis {
    /// The vertical axis, with a unit vector of `(0, 1)`.
    Vertical,
    /// The ascending diagonal axis, with a unit vector of `(1, 1)`.
    Ascending,
    /// The horizontal axis, with a unit vector of `(1, 0)`.
    Horizontal,
    /// The descending diagonal axis, with a unit vector of `(1, -1)`.
    Descending,
}

impl Axis {
    /// Returns the unit vector in the direction of the axis.
    #[inline]
    pub const fn unit_vec(self) -> (i32, i32) {
        match self {
            Axis::Vertical => (0, 1),
            Axis::Ascending => (1, 1),
            Axis::Horizontal => (1, 0),
            Axis::Descending => (1, -1),
        }
    }
}

/// A 2D point with `u32` coordinates.
///
/// # Attention
///
/// This struct may not be used with coordinates greater than `i32::MAX`.
/// This is required to avoid integer boundary checks.
///
/// A `Point` must be checked to be inside the board before use.
#[derive(Eq, PartialEq, Copy, Clone)]
pub struct Point {
    /// The horizontal coordinate.
    pub x: u32,
    /// The vertical coordinate.
    pub y: u32,
}

impl Point {
    /// Creates a new `Point` with the given coordinates.
    #[inline]
    pub const fn new(x: u32, y: u32) -> Point {
        debug_assert!(x <= i32::MAX as _ && y <= i32::MAX as _);
        Point { x, y }
    }

    /// Returns the forward adjacent point in the direction of the axis.
    ///
    /// This wraps around at the boundary of `u32`.
    #[inline]
    pub fn forward(self, axis: Axis) -> Point {
        let (dx, dy) = axis.unit_vec();
        Point::new(self.x.wrapping_add(dx as _), self.y.wrapping_add(dy as _))
    }

    /// Returns the backward adjacent point in the direction of the axis.
    ///
    /// This wraps around at the boundary of `u32`.
    #[inline]
    pub fn backward(self, axis: Axis) -> Point {
        let (dx, dy) = axis.unit_vec();
        Point::new(self.x.wrapping_sub(dx as _), self.y.wrapping_sub(dy as _))
    }
}

impl From<(u32, u32)> for Point {
    #[inline]
    fn from(t: (u32, u32)) -> Self {
        Point::new(t.0, t.1)
    }
}

impl PartialEq<(u32, u32)> for Point {
    #[inline]
    fn eq(&self, other: &(u32, u32)) -> bool {
        self.x == other.0 && self.y == other.1
    }
}

impl fmt::Display for Point {
    /// Formats a `Point` as a point reference.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.x < 26 {
            return write!(f, "{}{}", (b'A' + self.x as u8) as char, self.y + 1);
        }

        let mut arr = [0; 7];
        let mut x = self.x;
        let mut i = 6;
        loop {
            arr[i] = b'A' + (x % 26) as u8;
            x = match (x / 26).checked_sub(1) {
                Some(x) => x,
                None => break,
            };
            i -= 1;
        }

        write!(
            f,
            "{}{}",
            // SAFETY: The bytes are all ASCII and thus valid UTF-8.
            unsafe { str::from_utf8_unchecked(&arr[i..]) },
            self.y + 1,
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
/// An error which can be returned when parsing a point.
pub struct ParsePointError;

impl FromStr for Point {
    type Err = ParsePointError;

    /// Parses a point reference into a `Point`.
    ///
    /// # Examples
    ///
    /// ```
    /// use scamoku::board::Point;
    /// assert_eq!("A1".parse(), Ok(Point::new(0, 0)));
    /// assert_eq!("h7".parse(), Ok(Point::new(7, 6)));
    /// assert_eq!("xFD11".parse(), Ok(Point::new(16383, 10)));
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn alpha_to_num(b: &u8) -> Option<i32> {
            match b {
                b'a'..=b'z' => Some((b - b'a') as i32),
                b'A'..=b'Z' => Some((b - b'A') as i32),
                _ => None,
            }
        }

        if s.len() < 2 {
            return Err(ParsePointError);
        }

        let parse = || {
            let bytes = s.as_bytes();
            let mut x = alpha_to_num(&bytes[0])?;
            let mut i = 1;

            while let Some(n) = bytes.get(i).and_then(alpha_to_num) {
                // If the addition of 1 wasn't checked, it would overflow when `x == i32::MAX`,
                // that is when the string starts with "FXSHRXX".
                x = x.checked_add(1)?.checked_mul(26)?.checked_add(n)?;
                i += 1;
            }

            // SAFETY: `i` is one past an ASCII byte, which must
            // be within bounds and on a UTF-8 sequence boundary.
            let y = unsafe { s.get_unchecked(i..) }
                .parse::<u32>()
                .ok()
                .and_then(|n| n.checked_sub(1))
                .filter(|&n| n <= i32::MAX as _)?;

            Some((x as u32, y))
        };

        parse().map(Point::from).ok_or(ParsePointError)
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
enum OptStone {
    None = 0,
    Black = 1,
    White = 2,
}

impl From<Option<Stone>> for OptStone {
    #[inline]
    fn from(o: Option<Stone>) -> OptStone {
        // A safe match doesn't get optimized out.
        match o {
            None => OptStone::None,
            // SAFETY: `1u8` and `2u8` are both valid as `OptStone`.
            Some(s) => unsafe { mem::transmute(s) },
        }
    }
}

impl Into<Option<Stone>> for OptStone {
    #[inline]
    fn into(self) -> Option<Stone> {
        // This match gets optimized out into a single `mov`.
        match self {
            OptStone::None => None,
            OptStone::Black => Some(Stone::Black),
            OptStone::White => Some(Stone::White),
        }
    }
}

/// A slot (namely intersection) on the board.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct Slot {
    stone: OptStone,
    move_index: u16,
}

impl Slot {
    /// Returns `true` if the slot is empty.
    #[inline]
    pub fn is_empty(self) -> bool {
        self.stone == OptStone::None
    }

    /// Returns the stone in the slot, or `None` if empty.
    #[inline]
    pub fn stone(self) -> Option<Stone> {
        self.stone.into()
    }

    /// Returns the move index of the slot starting from 1, or `0` if empty.
    #[inline]
    pub fn move_index(self) -> u16 {
        self.move_index
    }
}

/// A square matrix.
#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
struct SqrMat {
    ptr: NonNull<Slot>,
}

impl SqrMat {
    /// Calculates the layout of matrix data.
    ///
    /// The returned layout will have non-zero size as long as `size != 0`.
    fn layout(size: u32) -> Layout {
        let size = size.pow(2) as usize * mem::size_of::<Slot>();
        let align = mem::align_of::<Slot>();

        // SAFETY: `align` meets the conditions as it is obtained from `mem::align_of`,
        // and `size` will never overflow since it is small enough.
        unsafe { Layout::from_size_align_unchecked(size, align) }
    }

    /// Creates a new `SqrMat` with the given size.
    ///
    /// # Safety
    ///
    /// `size` must not be zero.
    unsafe fn new(size: u32) -> SqrMat {
        let layout = Self::layout(size);

        // SAFETY: The caller must ensure that `size != 0`.
        // A zero-initialized `Slot` is valid since all its fields can hold 0.
        let ptr = NonNull::new(unsafe { alloc::alloc_zeroed(layout) })
            .unwrap_or_else(|| alloc::handle_alloc_error(layout))
            .cast();

        SqrMat { ptr }
    }

    /// Returns a pointer to the allocated matrix data.
    #[inline]
    fn as_ptr(self) -> *mut Slot {
        self.ptr.as_ptr()
    }
}

/// A square gomoku board.
#[derive(Debug)]
pub struct Board {
    /// Slot matrix.
    mat: SqrMat,
    /// Size of the board.
    size: u32,
    /// Current move index.
    cur_move_index: u16,
    /// Maximum move index, equal to `size * size`.
    max_move_index: u16,
}

impl Board {
    /// Creates a new `Board` of the given size.
    ///
    /// # Panics
    ///
    /// Panics if the size is less than 5 or greater than 255.
    pub fn new(size: u32) -> Self {
        assert!(size >= 5 && size <= 255);
        Board {
            // SAFETY: `size` is checked to be non-zero.
            mat: unsafe { SqrMat::new(size) },
            size,
            cur_move_index: 0,
            max_move_index: size.pow(2) as _,
        }
    }

    /// Returns the size of the board.
    #[inline]
    pub fn size(&self) -> u32 {
        self.size
    }

    /// Returns the index of a slot, or `None` if out of bounds.
    #[inline]
    fn index(&self, p: Point) -> Option<usize> {
        let size = self.size;
        if p.x < size && p.y < size {
            Some((p.y * size + p.x) as usize)
        } else {
            None
        }
    }

    /// Returns a reference to a slot, or `None` if out of bounds.
    #[inline]
    pub fn get(&self, p: Point) -> Option<&Slot> {
        // SAFETY: `Board::index` ensures that `i` is within bounds.
        self.index(p).map(|i| unsafe { &*self.mat.as_ptr().add(i) })
    }

    /// Returns a mutable reference to a slot, or `None` if out of bounds.
    #[inline]
    pub fn get_mut(&mut self, p: Point) -> Option<&mut Slot> {
        // SAFETY: `Board::index` ensures that `i` is within bounds.
        self.index(p)
            .map(|i| unsafe { &mut *self.mat.as_ptr().add(i) })
    }

    /// Returns `true` if the board contains a point.
    #[inline]
    pub fn contains_point(&self, p: Point) -> bool {
        let size = self.size();
        p.x < size && p.y < size
    }

    /// Returns the current move index, or `0` if empty.
    #[inline]
    pub fn cur_move_index(&self) -> u16 {
        self.cur_move_index
    }

    /// Returns the maximum move index.
    #[inline]
    pub fn max_move_index(&self) -> u16 {
        self.max_move_index
    }

    /// Makes a move on the board.
    ///
    /// # Panics
    ///
    /// Panics when moving out of board or into an occupied slot.
    pub fn make_move(&mut self, p: Point, stone: Stone) {
        let index = self.cur_move_index + 1;
        self.cur_move_index = index;

        let slot = &mut self[p];
        assert!(slot.is_empty(), "moving into an occupied slot");
        slot.stone = Some(stone).into();
        slot.move_index = index;
    }

    /// Returns the [Chebyshev distance][1] from a point to the board center.
    ///
    /// If the board has an even size, the center coordinates will be floored.
    ///
    /// [1]: https://en.wikipedia.org/wiki/Chebyshev_distance
    #[inline]
    pub fn dist_to_center(&self, p: Point) -> u32 {
        let center = (self.size() / 2) as i32;
        let dx = (p.x as i32 - center).unsigned_abs();
        let dy = (p.y as i32 - center).unsigned_abs();
        dx.max(dy)
    }
}

impl Drop for Board {
    fn drop(&mut self) {
        // SAFETY: The pointer is currently allocated via `GlobalAlloc`,
        // and the layout calculates the same as before with the same `size`.
        unsafe { alloc::dealloc(self.mat.as_ptr().cast(), SqrMat::layout(self.size)) }
    }
}

impl<P: Into<Point>> Index<P> for Board {
    type Output = Slot;
    #[inline]
    fn index(&self, p: P) -> &Slot {
        self.get(p.into()).expect("out of board")
    }
}

impl<P: Into<Point>> IndexMut<P> for Board {
    #[inline]
    fn index_mut(&mut self, p: P) -> &mut Slot {
        self.get_mut(p.into()).expect("out of board")
    }
}

#[cfg(test)]
mod tests {
    use super::{Axis::*, *};

    #[test]
    fn stone_opposite() {
        assert_eq!(Stone::Black.opposite(), Stone::White);
        assert_eq!(Stone::White.opposite(), Stone::Black);
    }

    #[test]
    fn point_fmt_parse() {
        assert_eq!(Point::new(0, 0).to_string(), "A1");
        assert_eq!(Point::new(7, 6).to_string(), "H7");
        assert_eq!(Point::new(16383, 127).to_string(), "XFD128");

        let max = Point::new(i32::MAX as _, i32::MAX as _);
        let max_s = "FXSHRXX2147483648";
        assert_eq!(max.to_string(), max_s);
        assert_eq!(max_s.parse(), Ok(max));

        for i in 0..1000 {
            let p = Point::new(i, i);
            assert_eq!(p.to_string().parse(), Ok(p));
        }

        assert!(Point::from_str("A").is_err());
        assert!(Point::from_str("1").is_err());
        assert!(Point::from_str("A0").is_err());
        assert!(Point::from_str("A2147483649").is_err());
        assert!(Point::from_str("FXSHRXY1").is_err());

        // This should not overflow.
        assert!(Point::from_str("FXSHRXXA1").is_err());
    }

    #[test]
    fn point_adjacent() {
        let p = Point::new(1, 1);
        assert_eq!(p.forward(Vertical), (1, 2));
        assert_eq!(p.forward(Ascending), (2, 2));
        assert_eq!(p.forward(Horizontal), (2, 1));
        assert_eq!(p.forward(Descending), (2, 0));
        assert_eq!(p.backward(Vertical), (1, 0));
        assert_eq!(p.backward(Ascending), (0, 0));
        assert_eq!(p.backward(Horizontal), (0, 1));
        assert_eq!(p.backward(Descending), (0, 2));
    }

    #[test]
    fn opt_stone() {
        assert_eq!(OptStone::from(None), OptStone::None);
        assert_eq!(OptStone::from(Some(Stone::Black)), OptStone::Black);
        assert_eq!(OptStone::from(Some(Stone::White)), OptStone::White);

        let mut o: Option<Stone>;
        o = OptStone::None.into();
        assert_eq!(o, None);
        o = OptStone::Black.into();
        assert_eq!(o, Some(Stone::Black));
        o = OptStone::White.into();
        assert_eq!(o, Some(Stone::White));
    }

    #[test]
    fn board() {
        let mut board = Board::new(15);

        assert_eq!(board.max_move_index(), 225);

        assert!(board.get((8, 8).into()).is_some());
        assert!(board.get((15, 8).into()).is_none());

        assert!(board.contains_point((8, 8).into()));
        assert!(!board.contains_point((15, 8).into()));

        let slot = board[(0, 0)];
        assert!(slot.is_empty());
        assert_eq!(slot.stone(), None);
        assert_eq!(slot.move_index(), 0);

        assert_eq!(board.cur_move_index(), 0);
        board.make_move((0, 0).into(), Stone::Black);
        assert_eq!(board.cur_move_index(), 1);

        let slot = board[(0, 0)];
        assert!(!slot.is_empty());
        assert_eq!(slot.stone(), Some(Stone::Black));
        assert_eq!(slot.move_index(), 1);

        assert_eq!(board.dist_to_center((0, 0).into()), 7);
        assert_eq!(board.dist_to_center((6, 8).into()), 1);
        assert_eq!(board.dist_to_center((7, 7).into()), 0);
        assert_eq!(board.dist_to_center((9, 12).into()), 5);
        assert_eq!(board.dist_to_center((14, 14).into()), 7);
    }

    #[test]
    fn board_npo() {
        assert_eq!(mem::size_of::<Board>(), mem::size_of::<Option<Board>>());
    }
}
