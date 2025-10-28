use log::trace;

use crate::utility::{display_slice, extract_from_vec_at_index, split_vec_at_index};
use std::fmt::{Debug, Display};

// -----------------------------------------------------------------------------
// Index
// -----------------------------------------------------------------------------

/// An index between kids, or left the first kid, or right of the last kid of an
/// [Expr].
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct Index(pub usize);

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Index {
    pub fn is_left_of_index(&self, other: &Self) -> bool {
        self.0 < other.0
    }

    pub fn is_left_of_step(&self, s: Step) -> bool {
        self.0 <= s.0
    }

    pub fn sub_offset(&self, offset: &Offset) -> Self {
        Self(self.0 - offset.0)
    }

    pub fn add_offset(&self, offset: &Offset) -> Self {
        Self(self.0 + offset.0)
    }

    pub fn is_right_of_step(&self, s: Step) -> bool {
        self.0 > s.0
    }

    pub fn to_offset(&self) -> Offset {
        Offset(self.0)
    }

    pub fn left_step(&self) -> Step {
        Step(self.0 - 1)
    }

    pub fn right_step(&self) -> Step {
        Step(self.0)
    }
}

// -----------------------------------------------------------------------------
// Step
// -----------------------------------------------------------------------------

/// A step from an [Expr] to one of its kids.
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct Step(pub usize);

impl Display for Step {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Step {
    pub fn left_step(&self) -> Self {
        self.sub_offset(&Offset(1))
    }

    pub fn right_step(&self) -> Self {
        self.add_offset(&Offset(1))
    }

    pub fn left_index(&self) -> Index {
        Index(self.0)
    }

    pub fn right_index(&self) -> Index {
        Index(self.0 + 1)
    }

    pub fn is_left_of_index(&self, i: Index) -> bool {
        self.0 < i.0
    }

    pub fn sub_offset(&self, offset: &Offset) -> Self {
        Self(self.0 - offset.0)
    }

    pub fn add_offset(&self, offset: &Offset) -> Self {
        Self(self.0 + offset.0)
    }
}

// -----------------------------------------------------------------------------
// Path
// -----------------------------------------------------------------------------

/// A path from the top [Expr] to an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct Path(pub Vec<Step>);

#[macro_export]
macro_rules! path {
    ( $( $step:expr ),* $(,)? ) => {
        Path(vec![ $( Step($step) ),* ])
    };
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "path!{}", display_slice(&self.0))
    }
}

impl Path {
    pub fn add_offset_to_first_step(self, offset: &Offset) -> Option<Self> {
        let mut path = self.0;
        if path.is_empty() {
            return None;
        }
        let s0 = path.remove(0);
        path.insert(0, s0.add_offset(offset));
        Some(Self(path))
    }

    pub fn sub_offset_to_first_step(self, offset: &Offset) -> Option<Self> {
        let mut path = self.0;
        if path.is_empty() {
            return None;
        }
        let s0 = path.remove(0);
        path.insert(0, s0.sub_offset(offset));
        Some(Self(path))
    }

    pub fn empty() -> Self {
        Self(vec![])
    }

    /// If `self` is a sibling of an ancestor of `other`, then is [`Option::Some`] of:
    ///   - the [Step] into the parent of `self` that goes down towards `other`
    ///   - the path `path` such that `self.path.append(path) = other.path`
    ///
    /// Otherwise is [`Option::None`].
    pub fn is_sibling_of_ancestor_of(&self, other: &Self) -> Option<(Step, Self)> {
        for (i, s0) in self.0.iter().enumerate() {
            let s1 = other.0.get(i)?;
            if !(s0 == s1) {
                return None;
            }
        }
        let mut other_suffix = other.0[self.0.len()..].to_vec();
        let s = other_suffix.remove(0);
        Some((s, Self(other_suffix)))
    }

    pub fn append(self, other: Self) -> Self {
        let mut path0 = self.0;
        let mut path1 = other.0;
        path0.append(&mut path1);
        Self(path0)
    }

    /// If `self` is a prefix of `other`, then return the path `suffix` such
    /// that `self.append(suffix) == other`
    pub fn diff(&self, other: &Self) -> Option<Self> {
        let suffix = other.0.strip_prefix(self.0.as_slice())?;
        Some(Self(suffix.to_vec()))
    }

    pub fn is_prefix_of(&self, other: &Self) -> bool {
        other.0.strip_prefix(self.0.as_slice()).is_some()
    }
}

// -----------------------------------------------------------------------------
// Point
// -----------------------------------------------------------------------------

/// A point between two [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct Point {
    pub path: Path,
    pub i: Index,
}

#[macro_export]
macro_rules! point {
    ( [ $( $s:expr ),* $(,)? ], $i:expr ) => {
        Point {
            path: path![ $( $s ),* ],
            i: Index($i),
        }
    };
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "point![{}, {}]", display_slice(&self.path.0), self.i)
    }
}

impl Default for Point {
    fn default() -> Self {
        Self {
            path: Path::empty(),
            i: Index(0),
        }
    }
}

impl Point {
    pub fn add_offset_at_top(self, offset: &Offset) -> Self {
        let mut p = self;
        if let Some(s0) = p.path.0.get_mut(0) {
            *s0 = s0.add_offset(offset);
        } else {
            p.i = p.i.add_offset(offset);
        }
        p
    }

    pub fn sub_offset_at_top(self, offset: &Offset) -> Self {
        let mut p = self;
        if let Some(s0) = p.path.0.get_mut(0) {
            *s0 = s0.sub_offset(offset);
        } else {
            p.i = p.i.sub_offset(offset);
        }
        p
    }

    pub fn is_left_adjacent_to_point(&self, other: &Self) -> bool {
        self.path == other.path && self.i.is_left_of_index(&other.i)
    }

    pub fn move_dir<L: Debug + Display>(
        &mut self,
        expr: &Expr<L>,
        dir: &MoveDir,
    ) -> Result<(), MoveError> {
        let sub_expr = expr.get_subexpr(&self.path);
        let leftmost = sub_expr.kids.leftmost_index();
        let rightmost = sub_expr.kids.rightmost_index();
        match dir {
            MoveDir::Prev => {
                if leftmost.is_left_of_index(&self.i) {
                    self.path.0.push(self.i.left_step());
                    let e = expr.get_subexpr(&self.path);
                    self.i = e.kids.rightmost_index();
                    Ok(())
                } else if let Some(s) = self.path.0.pop() {
                    self.i = s.left_index();
                    Ok(())
                } else {
                    Err(MoveError::Boundary)
                }
            }
            MoveDir::Next => {
                if self.i.is_left_of_index(&rightmost) {
                    self.path.0.push(self.i.right_step());
                    let e = expr.get_subexpr(&self.path);
                    self.i = e.kids.leftmost_index();
                    Ok(())
                } else if let Some(s) = self.path.0.pop() {
                    self.i = s.right_index();
                    Ok(())
                } else {
                    Err(MoveError::Boundary)
                }
            }
        }
    }

    pub fn to_empty_span_handle(&self) -> SpanHandle {
        SpanHandle {
            path: self.path.clone(),
            i_l: self.i,
            i_r: self.i,
            focus: SpanFocus::Right,
        }
    }

    pub fn to_empty_zipper_handle(&self) -> ZipperHandle {
        ZipperHandle {
            path_o: self.path.clone(),
            i_ol: self.i,
            i_or: self.i,
            path_m: Path::empty(),
            i_il: self.i,
            i_ir: self.i,
            focus: ZipperFocus::InnerLeft,
        }
    }

    pub fn new(path: Path, i: Index) -> Self {
        Self { path, i }
    }
}

// -----------------------------------------------------------------------------
// Handle
// -----------------------------------------------------------------------------

/// A handle for a [Fragment].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum Handle {
    Point(Point),
    Span(SpanHandle),
    Zipper(ZipperHandle),
}

impl Display for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Point(p) => std::fmt::Display::fmt(&p, f),
            Self::Span(h) => std::fmt::Display::fmt(&h, f),
            Self::Zipper(h) => std::fmt::Display::fmt(&h, f),
        }
    }
}

impl Default for Handle {
    fn default() -> Self {
        Self::Point(Default::default())
    }
}

impl Handle {
    /// When a [`SpanHandle`] is empty (the two endpoints are the same),
    /// then collapse it to a [Point].
    ///
    /// When the middle path of the zipper handle is empty, then collapse
    /// it to a [`SpanHandle`].
    pub fn norm(self) -> Self {
        match &self {
            Self::Span(h) if h.i_l == h.i_r => Self::Point(h.focus_point()),
            // NOTE: the choice to return h_o rather than h_i here should be
            // arbitrary, but perhaps there might be edge cases where it matters
            // which one, even when the middle [Path] is empty? Nothing really
            // should depend on using a "flat" ZipperHandle, right?
            Self::Zipper(h) if h.path_m.0.is_empty() => Self::Span(h.handle_o()),
            _ => self,
        }
    }

    pub fn move_up<L: Debug + Display>(&mut self, expr: &Expr<L>) -> Result<(), MoveError> {
        match self {
            Self::Point(p) => {
                let e = expr.get_subexpr(&p.path);
                let path = Path(p.path.0.drain(..).collect::<Vec<_>>());
                let mut h = SpanHandle {
                    path,
                    i_l: e.kids.leftmost_index(),
                    i_r: e.kids.rightmost_index(),
                    focus: SpanFocus::Right,
                };
                // TODO: combine this logic with what appears in the Span case, since it's the same
                if h.i_l == e.kids.leftmost_index() && h.i_r == e.kids.rightmost_index() {
                    let mut path = Path(h.path.0.drain(..).collect::<Vec<_>>());
                    let s = path.0.pop().ok_or(MoveError::Boundary)?;
                    *self = Self::Span(SpanHandle {
                        path,
                        i_l: s.left_index(),
                        i_r: s.right_index(),
                        focus: SpanFocus::Right,
                    });
                } else {
                    *self = Self::Span(h);
                }
                Ok(())
            }
            Self::Span(h) => {
                let e = expr.get_subexpr(&h.path);
                if h.i_l == e.kids.leftmost_index() && h.i_r == e.kids.rightmost_index() {
                    let mut path = Path(h.path.0.drain(..).collect::<Vec<_>>());
                    let s = path.0.pop().ok_or(MoveError::Boundary)?;
                    *self = Self::Span(SpanHandle {
                        path,
                        i_l: s.left_index(),
                        i_r: s.right_index(),
                        focus: SpanFocus::Right,
                    });
                    Ok(())
                } else {
                    h.i_l = e.kids.leftmost_index();
                    h.i_r = e.kids.rightmost_index();
                    Ok(())
                }
            }
            Self::Zipper(_) => Err(MoveError::Undefined),
        }
    }

    pub fn move_dir<L: Debug + Display>(
        &mut self,
        expr: &Expr<L>,
        dir: &MoveDir,
    ) -> Result<(), MoveError> {
        trace!(target: "expr.move", "self = {self}");
        trace!(target: "expr.move", "expr = {expr}");
        trace!(target: "expr.move", "dir = {dir}");
        match self {
            Self::Point(point) => {
                point.move_dir(expr, dir)?;
                Ok(())
            }
            Self::Span(h) => match dir {
                MoveDir::Prev => {
                    *self = Self::Point(h.p_l());
                    Ok(())
                }
                MoveDir::Next => {
                    *self = Self::Point(h.p_r());
                    Ok(())
                }
            },
            Self::Zipper(h) => {
                *self = Self::Point(h.focus_point());
                Ok(())
            }
        }
    }

    pub fn focus_point(&self) -> Point {
        match self {
            Self::Point(point) => point.clone(),
            Self::Span(handle) => handle.focus_point(),
            Self::Zipper(handle) => handle.focus_point(),
        }
    }

    pub fn rotate_focus_dir(&mut self, dir: &MoveDir) {
        match self {
            Self::Point(_handle) => (),
            Self::Span(handle) => handle.focus = handle.focus.rotate_dir(dir),
            Self::Zipper(handle) => handle.focus = handle.focus.rotate_dir(dir),
        }
    }

    pub fn escape(&mut self) -> Result<(), MoveError> {
        match self {
            Self::Point(_) => Err(MoveError::Undefined),
            Self::Span(h) => {
                *self = Self::Point(h.focus_point());
                Ok(())
            }
            Self::Zipper(h) => {
                *self = Self::Point(h.focus_point());
                Ok(())
            }
        }
    }

    pub fn drag<L: Debug + Display>(self, e: &Expr<L>, target: &Point) -> Option<Self> {
        trace!(target: "expr.drag", "self   = {self}");
        trace!(target: "expr.drag", "e      = {e}");
        trace!(target: "expr.drag", "target = {target}");

        match self {
            Self::Point(source) => {
                if target.path == source.path {
                    if target.i.is_left_of_index(&source.i) {
                        Some(Self::Span(SpanHandle {
                            path: source.path,
                            i_l: target.i,
                            i_r: source.i,
                            focus: SpanFocus::Left,
                        }))
                    } else if target.i == source.i {
                        Some(Self::Point(source))
                    } else if source.i.is_left_of_index(&target.i) {
                        Some(Self::Span(SpanHandle {
                            path: source.path,
                            i_l: source.i,
                            i_r: target.i,
                            focus: SpanFocus::Right,
                        }))
                    } else {
                        panic!("impossible")
                    }
                } else {
                    // drag from inner left to outer left
                    #[expect(irrefutable_let_patterns)]
                    if let p_il = &source
                        && let p_ol = &target
                        && let Some(path_m) = p_ol.path.diff(&p_il.path)
                        && let Some(s) = path_m.0.first()
                        && p_ol.i.is_left_of_step(*s)
                    {
                        trace!(target: "expr.drag", "drag from inner left to outer left");
                        let path_o = p_ol.path.clone();
                        let i_or = s.right_index();
                        let i_ol = p_ol.i;
                        let i_il = p_il.i;
                        let i_ir = e.get_subexpr(&p_il.path).kids.rightmost_index();
                        let focus = ZipperFocus::OuterLeft;
                        Some(Self::Zipper(ZipperHandle {
                            path_o,
                            i_ol,
                            i_or,
                            path_m,
                            i_il,
                            i_ir,
                            focus,
                        }))
                    } else
                    // drag from inner right to outer right
                    if let p_ir = &source
                        && let p_or = &target
                        && let Some(path_m) = p_or.path.diff(&p_ir.path)
                        && let Some(s) = path_m.0.first()
                        && s.is_left_of_index(p_or.i)
                    {
                        trace!(target: "expr.drag", "drag from inner right to outer right");
                        let path_o = p_or.path.clone();
                        let i_ol = s.left_index();
                        let i_or = p_or.i;
                        let i_il = e.get_subexpr(&p_ir.path).kids.leftmost_index();
                        let i_ir = p_ir.i;
                        let focus = ZipperFocus::OuterRight;
                        Some(Self::Zipper(ZipperHandle {
                            path_o,
                            i_ol,
                            i_or,
                            path_m,
                            i_il,
                            i_ir,
                            focus,
                        }))
                    } else
                    // drag from outer left to inner left
                    if let p_ol = &source
                        && let p_il = &target
                        && let Some(path_m) = p_ol.path.diff(&p_il.path)
                        && let Some(s) = path_m.0.first()
                        && p_ol.i.is_left_of_step(*s)
                    {
                        trace!(target: "expr.drag", "drag from outer left to inner left");
                        let path_o = p_ol.path.clone();
                        let i_ol = p_ol.i;
                        let i_or = s.right_index();
                        let i_il = p_il.i;
                        let i_ir = e.get_subexpr(&p_il.path).kids.rightmost_index();
                        let focus = ZipperFocus::InnerLeft;
                        Some(Self::Zipper(ZipperHandle {
                            path_o,
                            i_ol,
                            i_or,
                            path_m,
                            i_il,
                            i_ir,
                            focus,
                        }))
                    } else
                    // drag from outer right to inner right
                    if let p_or = &source
                        && let p_ir = &target
                        && let Some(path_m) = p_or.path.diff(&p_ir.path)
                        && let Some(s) = path_m.0.first()
                        && s.is_left_of_index(p_or.i)
                    {
                        trace!(target: "expr.drag", "drag from outer right to inner right");
                        let path_o = p_or.path.clone();
                        let i_ol = s.left_index();
                        let i_or = p_or.i;
                        let i_il = e.get_subexpr(&p_ir.path).kids.leftmost_index();
                        let i_ir = p_ir.i;
                        let focus = ZipperFocus::InnerRight;
                        Some(Self::Zipper(ZipperHandle {
                            path_o,
                            i_ol,
                            i_or,
                            path_m,
                            i_il,
                            i_ir,
                            focus,
                        }))
                    } else {
                        None
                    }
                }
            }
            Self::Span(handle) => Self::Point(handle.nonfocus_point()).drag(e, target),
            Self::Zipper(source) => {
                let path_i = source.path_i();

                match source.focus {
                    ZipperFocus::OuterLeft if target.path.is_prefix_of(&path_i) => {
                        trace!(target: "expr.drag", "adjust i_ol");
                        Self::Point(source.p_il()).drag(e, target).map(|h| match h {
                            Self::Zipper(h) => {
                                let mut h = h;
                                h.i_or = if target.is_left_adjacent_to_point(&Point {
                                    path: source.path_o.clone(),
                                    i: source
                                        .path_m
                                        .0
                                        .first()
                                        .expect("Expected path_m to be non-empty")
                                        .left_index(),
                                }) {
                                    source.i_or
                                } else {
                                    h.i_or
                                };
                                h.i_il = source.i_il;
                                h.i_ir = source.i_ir;
                                Self::Zipper(h)
                            }
                            _ => h,
                        })
                    }
                    ZipperFocus::OuterRight if target.path.is_prefix_of(&path_i) => {
                        trace!(target: "expr.drag", "adjust i_or");
                        Self::Point(source.p_ir()).drag(e, target).map(|h| match h {
                            Self::Zipper(h) => {
                                let mut h = h;
                                h.i_ol = if (Point {
                                    path: source.path_o.clone(),
                                    i: source
                                        .path_m
                                        .0
                                        .first()
                                        .expect("Expected path_m to be non-empty")
                                        .right_index(),
                                })
                                .is_left_adjacent_to_point(target)
                                {
                                    source.i_ol
                                } else {
                                    h.i_ol
                                };
                                h.i_il = source.i_il;
                                h.i_ir = source.i_ir;
                                Self::Zipper(h)
                            }
                            _ => h,
                        })
                    }
                    ZipperFocus::InnerLeft if source.path_o.is_prefix_of(&target.path) => {
                        trace!(target: "expr.drag", "adjust i_il");
                        Self::Point(source.p_ol()).drag(e, target).map(|h| match h {
                            Self::Zipper(h) => {
                                let mut h = h;
                                h.i_ol = source.i_ol;
                                h.i_or = source.i_or;
                                h.i_ir = if target.is_left_adjacent_to_point(&source.p_ir()) {
                                    source.i_ir
                                } else {
                                    h.i_ir
                                };
                                Self::Zipper(h)
                            }
                            _ => h,
                        })
                    }
                    ZipperFocus::InnerRight if source.path_o.is_prefix_of(&target.path) => {
                        trace!(target: "expr.drag", "adjust i_ir");
                        Self::Point(source.p_or()).drag(e, target).map(|h| match h {
                            Self::Zipper(h) => {
                                let mut h = h;
                                h.i_ol = source.i_ol;
                                h.i_or = source.i_or;
                                h.i_il = if source.p_il().is_left_adjacent_to_point(target) {
                                    source.i_il
                                } else {
                                    h.i_il
                                };
                                Self::Zipper(h)
                            }
                            _ => h,
                        })
                    }
                    _ => None,
                }
            }
        }
    }

    pub fn contains_point(&self, p: &Point) -> bool {
        match self {
            Self::Point(_) => false,
            Self::Span(h) => h.contains_point(p),
            Self::Zipper(h) => h.contains_point(p),
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        match self {
            Self::Point(_) => false,
            Self::Span(h) => h.contains_path(path),
            Self::Zipper(h) => h.contains_path(path),
        }
    }
}

// -----------------------------------------------------------------------------
// SpanHandle
// -----------------------------------------------------------------------------

/// A handle for a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct SpanHandle {
    pub path: Path,
    pub i_l: Index,
    pub i_r: Index,
    pub focus: SpanFocus,
}

#[macro_export]
macro_rules! span_handle {
    ([ $( $s:expr ),* $(,)? ], $i_l:expr, $i_r:expr, $focus:expr) => {
        SpanHandle {
            path: path![ $( $s ),* ],
            i_l: Index($i_l),
            i_r: Index($i_r),
            focus: $focus,
        }
    };
}

impl Display for SpanHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "span_handle![{}, {}, {}, {}]",
            display_slice(self.path.0.as_slice()),
            self.i_l,
            self.i_r,
            self.focus
        )
    }
}

impl SpanHandle {
    pub fn focus_point(&self) -> Point {
        match self.focus {
            SpanFocus::Left => self.p_l(),
            SpanFocus::Right => self.p_r(),
        }
    }

    pub fn nonfocus_point(&self) -> Point {
        match self.focus {
            SpanFocus::Left => self.p_r(),
            SpanFocus::Right => self.p_l(),
        }
    }

    /// Left point of [Span].
    pub fn p_l(&self) -> Point {
        Point {
            path: self.path.clone(),
            i: self.i_l,
        }
    }

    /// Right point of [Span].
    pub fn p_r(&self) -> Point {
        Point {
            path: self.path.clone(),
            i: self.i_r,
        }
    }

    pub fn contains_point(&self, p: &Point) -> bool {
        self.path
            .diff(&p.path)
            .map(|suffix| match suffix.0.first() {
                None => self.i_l.is_left_of_index(&p.i) && p.i.is_left_of_index(&self.i_r),
                Some(s0) => self.i_l.is_left_of_step(*s0) && s0.is_left_of_index(self.i_r),
            })
            .unwrap_or(false)
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        self.path
            .diff(path)
            .and_then(|suffix| suffix.0.first().copied())
            .map(|s0| self.i_l.is_left_of_step(s0) && s0.is_left_of_index(self.i_r))
            .unwrap_or(false)
    }

    pub fn to_empty_zipper_handle(&self) -> ZipperHandle {
        ZipperHandle {
            path_o: self.path.clone(),
            i_ol: self.i_l,
            i_or: self.i_r,
            path_m: Path::empty(),
            i_il: self.i_l,
            i_ir: self.i_r,
            focus: self.focus.to_inner_zipper_focus(),
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum SpanFocus {
    Left,
    Right,
}

impl Display for SpanFocus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Left => write!(f, "Left"),
            Self::Right => write!(f, "Right"),
        }
    }
}

impl SpanFocus {
    pub fn rotate_dir(&self, _dir: &MoveDir) -> Self {
        match self {
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }

    pub fn to_outer_zipper_focus(&self) -> ZipperFocus {
        match self {
            Self::Left => ZipperFocus::OuterLeft,
            Self::Right => ZipperFocus::OuterRight,
        }
    }

    pub fn to_inner_zipper_focus(&self) -> ZipperFocus {
        match self {
            Self::Left => ZipperFocus::InnerLeft,
            Self::Right => ZipperFocus::InnerRight,
        }
    }
}

// -----------------------------------------------------------------------------
// ZipperHandle
// -----------------------------------------------------------------------------

/// A handle for a [Zipper].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct ZipperHandle {
    pub path_o: Path,
    pub i_ol: Index,
    pub i_or: Index,
    pub path_m: Path,
    pub i_il: Index,
    pub i_ir: Index,
    pub focus: ZipperFocus,
}

#[macro_export]
macro_rules! zipper_handle {
    ( [ $( $s_o:expr ),* $(,)? ] , $i_ol:expr, $i_or:expr, [ $( $s_m:expr ),* $(,)? ], $i_il:expr, $i_ir:expr, $focus:expr) => {
        ZipperHandle {
            path_o: path![ $( $s_o ),* ],
            i_ol: Index($i_ol),
            i_or: Index($i_or),
            path_m: path![ $( $s_m ),* ],
            i_il: Index($i_il),
            i_ir: Index($i_ir),
            focus: $focus,
        }
    };
}

impl Display for ZipperHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "zipper_handle![{}, {}, {}, {}, {}, {}, {}]",
            display_slice(self.path_o.0.as_slice()),
            self.i_ol,
            self.i_or,
            display_slice(self.path_m.0.as_slice()),
            self.i_il,
            self.i_ir,
            self.focus,
        )
    }
}

impl ZipperHandle {
    pub fn path_i(&self) -> Path {
        self.path_o.clone().append(self.path_m.clone())
    }

    pub fn p_il(&self) -> Point {
        Point {
            path: self.path_i(),
            i: self.i_il,
        }
    }

    pub fn p_ir(&self) -> Point {
        Point {
            path: self.path_i(),
            i: self.i_ir,
        }
    }

    pub fn p_ol(&self) -> Point {
        Point {
            path: self.path_o.clone(),
            i: self.i_ol,
        }
    }

    pub fn p_or(&self) -> Point {
        Point {
            path: self.path_o.clone(),
            i: self.i_or,
        }
    }

    pub fn focus_point(&self) -> Point {
        match self.focus {
            ZipperFocus::OuterLeft => self.p_ol(),
            ZipperFocus::InnerLeft => self.p_il(),
            ZipperFocus::InnerRight => self.p_ir(),
            ZipperFocus::OuterRight => self.p_or(),
        }
    }

    pub fn contains_point(&self, p: &Point) -> bool {
        self.handle_o().contains_point(p) && !self.handle_i().contains_point(p)
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        self.handle_o().contains_path(path) && !self.handle_i().contains_path(path)
    }

    /// Outer [`SpanHandle`].
    pub fn handle_o(&self) -> SpanHandle {
        SpanHandle {
            path: self.path_o.clone(),
            i_l: self.i_ol,
            i_r: self.i_or,
            focus: self.focus.to_span_focus(),
        }
    }

    /// Inner [`SpanHandle`].
    pub fn handle_i(&self) -> SpanHandle {
        SpanHandle {
            path: self.path_i(),
            i_l: self.i_il,
            i_r: self.i_ir,
            focus: self.focus.to_span_focus(),
        }
    }

    pub fn first_middle_step(&self) -> Step {
        *self
            .path_m
            .0
            .first()
            .unwrap_or_else(|| panic!("zipper handle middle path should not be empty"))
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum ZipperFocus {
    OuterLeft,
    InnerLeft,
    InnerRight,
    OuterRight,
}

impl Display for ZipperFocus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OuterLeft => write!(f, "OuterLeft"),
            Self::InnerLeft => write!(f, "InnerLeft"),
            Self::InnerRight => write!(f, "InnerRight"),
            Self::OuterRight => write!(f, "OuterRight"),
        }
    }
}

impl ZipperFocus {
    pub fn rotate_dir(&self, dir: &MoveDir) -> Self {
        match self {
            Self::OuterLeft => match dir {
                MoveDir::Prev => Self::OuterRight,
                MoveDir::Next => Self::InnerLeft,
            },
            Self::InnerLeft => match dir {
                MoveDir::Prev => Self::OuterLeft,
                MoveDir::Next => Self::InnerRight,
            },
            Self::InnerRight => match dir {
                MoveDir::Prev => Self::InnerLeft,
                MoveDir::Next => Self::OuterRight,
            },
            Self::OuterRight => match dir {
                MoveDir::Prev => Self::InnerRight,
                MoveDir::Next => Self::OuterLeft,
            },
        }
    }

    pub fn to_span_focus(&self) -> SpanFocus {
        match self {
            Self::OuterLeft | Self::InnerLeft => SpanFocus::Left,
            Self::OuterRight | Self::InnerRight => SpanFocus::Right,
        }
    }
}

// -----------------------------------------------------------------------------
// Expr
// -----------------------------------------------------------------------------

/// An expression of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Expr<L> {
    pub label: L,
    pub kids: Span<L>,
}

#[macro_export]
macro_rules! ex {
    ( $label:expr, [ $( $e:expr ),* $(,)? ] ) => {
        Expr {
            label: $label,
            kids: Span(vec![ $( $e ),* ]),
        }
    };
}

impl<L: Display> Display for Expr<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ex![{}, {}]",
            self.label,
            display_slice(self.kids.0.as_slice())
        )
    }
}

impl<L1> Expr<L1> {
    pub fn map<L2, F>(&self, f: &mut F) -> Expr<L2>
    where
        F: FnMut(&L1) -> L2,
    {
        Expr {
            label: f(&self.label),
            kids: Span(self.kids.0.iter().map(|e| e.map(f)).collect()),
        }
    }

    pub fn map_owned<L2, F>(self, f: &mut F) -> Expr<L2>
    where
        F: FnMut(L1) -> L2,
    {
        Expr {
            label: f(self.label),
            kids: Span(self.kids.0.into_iter().map(|e| e.map_owned(f)).collect()),
        }
    }
}

impl<L: Debug + Display> Expr<L> {
    pub fn pat(&self) -> (&L, &[Self]) {
        (&self.label, self.kids.0.as_slice())
    }

    pub fn pat_owned<const N: usize>(self) -> (L, [Self; N]) {
        (
            self.label,
            self.kids
                .0
                .try_into()
                .unwrap_or_else(|_| panic!("impossible")),
        )
    }

    pub fn new(label: L, kids: Span<L>) -> Self {
        Self { label, kids }
    }

    pub fn height(&self) -> u32 {
        self.kids
            .0
            .iter()
            .fold(0, |h, e| std::cmp::max(h, 1 + e.height()))
    }

    pub fn insert_fragment(&mut self, h: Handle, frag: Fragment<L>) -> Handle {
        trace!(target: "expr.insert", "self = {self}");
        trace!(target: "expr.insert", "h    = {h}");
        trace!(target: "expr.insert", "frag = {frag}");

        match h {
            Handle::Point(p) => match frag {
                Fragment::Span(span) => {
                    let (_, h) = self.replace_span(&p.to_empty_span_handle(), span);
                    Handle::Span(h)
                }
                Fragment::Zipper(zipper) => {
                    let (_, h) = self.replace_zipper(&p.to_empty_zipper_handle(), zipper);
                    Handle::Zipper(h)
                }
            },
            Handle::Span(h) => match frag {
                Fragment::Span(span) => {
                    let (_, h) = self.replace_span(&h, span);
                    Handle::Span(h)
                }
                Fragment::Zipper(zipper) => {
                    let (_, h) = self.replace_zipper(&h.to_empty_zipper_handle(), zipper);
                    Handle::Zipper(h)
                }
            },
            Handle::Zipper(h) => match frag {
                Fragment::Span(span) => {
                    let (_, h) = self.replace_span(&h.handle_o(), span);
                    Handle::Span(h)
                }
                Fragment::Zipper(zipper) => {
                    let (_, h) = self.replace_zipper(&h, zipper);
                    Handle::Zipper(h)
                }
            },
        }
    }

    pub fn get_fragment(&self, h: &Handle) -> Option<FragmentRef<'_, L>>
    where
        L: Debug,
    {
        match h {
            Handle::Point(_) => None,
            Handle::Span(h) => Some(FragmentRef::Span(self.get_span(h))),
            Handle::Zipper(h) => Some(FragmentRef::Zipper(self.get_zipper(h))),
        }
    }

    /// Replaces the span at the handle with the new span and returns the old
    /// span.
    pub fn replace_span(&mut self, h: &SpanHandle, new_span: Span<L>) -> (Span<L>, SpanHandle) {
        trace!(target: "expr.replace_span", "self     = {self}");
        trace!(target: "expr.replace_span", "h        = {h}");
        trace!(target: "expr.replace_span", "new_span = {new_span}");

        let parent = self.get_subexpr_mut(&h.path);
        let new_span_offset = &new_span.offset();
        let old_span = Span(parent.kids.0.splice(h.i_l.0..h.i_r.0, new_span.0).collect());
        (
            old_span,
            SpanHandle {
                path: h.path.clone(),
                i_l: h.i_l,
                i_r: h.i_l.add_offset(new_span_offset),
                focus: h.focus,
            },
        )
    }

    /// Replaces the zipper at the handle with the new zipper and returns the
    /// old zipper.
    pub fn replace_zipper(
        &mut self,
        h: &ZipperHandle,
        new_zipper: Zipper<L>,
    ) -> (Zipper<L>, ZipperHandle) {
        trace!(target: "expr.replace_zipper", "self       = {self}");
        trace!(target: "expr.replace_zipper", "h          = {h}");
        trace!(target: "expr.replace_zipper", "new_zipper = {new_zipper}");

        // take the inner span
        let e_o = self.get_subexpr_mut(&h.path_o);
        let (span_i, _) = e_o.replace_span(
            &SpanHandle {
                path: h.path_m.clone(),
                i_l: h.i_il,
                i_r: h.i_ir,
                focus: h.focus.to_span_focus(),
            },
            Span::empty(),
        );
        let span_i_offset = span_i.offset();

        // wrap the inner span with the zipper
        let new_zipper_inner_point = new_zipper.inner_point();
        let point_m = new_zipper_inner_point.add_offset_at_top(&h.i_il.to_offset());
        let new_span_m: Span<L> = new_zipper.surround(span_i);
        let new_span_m_offset = &new_span_m.offset();

        // replace outer span handle of zipper with result
        let old_span_m = if h.path_m.0.is_empty() {
            // If the middle path of the zipper handle is empty, then when we
            // replaced the span above with an empty span, we actually did that
            // in e_o's kids directly, Which means we need to just go to a point
            // here.
            e_o.kids.replace_subspan(h.i_ol, h.i_ol, new_span_m)
        } else {
            e_o.kids.replace_subspan(h.i_ol, h.i_or, new_span_m)
        };
        trace!(target: "expr.replace_zipper", "old_span_m = {old_span_m}");

        let old_zipper: Zipper<L> = old_span_m.into_zipper(
            &(Point {
                path: h.path_m.clone(),
                i: h.i_ol,
            })
            .sub_offset_at_top(&h.i_il.to_offset()),
        );
        trace!(target: "expr.replace_zipper", "old_zipper = {old_zipper}");

        (
            old_zipper,
            ZipperHandle {
                path_o: h.path_o.clone(),
                i_ol: h.i_ol,
                i_or: h.i_ol.add_offset(new_span_m_offset),
                path_m: point_m.path,
                i_il: point_m.i,
                i_ir: point_m.i.add_offset(&span_i_offset),
                focus: h.focus,
            },
        )
    }

    pub fn cut(&mut self, h: &Handle) -> Option<(Fragment<L>, Handle)> {
        trace!(target: "expr.cut", "self = {self}");
        trace!(target: "expr.cut", "h    = {h}");
        match h {
            Handle::Point(_) => None,
            Handle::Span(h) => {
                let (span, h) = self.replace_span(h, Span::empty());
                Some((Fragment::Span(span), Handle::Span(h)))
            }
            Handle::Zipper(h) => {
                let (zipper, h) = self.replace_zipper(h, Zipper::empty());
                Some((Fragment::Zipper(zipper).norm(), Handle::Zipper(h).norm()))
            }
        }
    }

    pub fn into_subexpr(self, path: &Path) -> (Context<L>, Self) {
        let mut ctx: Context<L> = Context::empty();
        let mut e = self;
        for s in &path.0 {
            let (th, e_kid) = e.into_kid(*s);
            ctx.0.push(th);
            e = e_kid;
        }
        (ctx, e)
    }

    pub fn into_kid(self, s: Step) -> (Tooth<L>, Self) {
        let (left, middle, right) = self.kids.into_kid(s);
        (
            Tooth {
                label: self.label,
                span_l: left,
                span_r: right,
            },
            middle,
        )
    }

    pub fn example<F: FnMut() -> L>(mk_label: &mut F, width: i32, depth: i32) -> Self {
        Self {
            label: mk_label(),
            kids: Span(if depth == 0 {
                vec![]
            } else {
                (0..width)
                    .map(|_| Self::example(mk_label, width, depth - 1))
                    .collect()
            }),
        }
    }

    pub fn get_subexpr_mut(&mut self, path: &Path) -> &mut Self {
        let mut expr = self;
        for step in &path.0 {
            expr = expr.get_kid_mut(*step);
        }
        expr
    }

    pub fn get_kid_mut(&mut self, step: Step) -> &mut Self {
        self.kids.get_kid_mut(step)
    }

    pub fn get_span(&self, h: &SpanHandle) -> SpanRef<'_, L> {
        let e = self.get_subexpr(&h.path);
        e.kids.get_subspan(h.i_l, h.i_r)
    }

    pub fn get_zipper(&self, h: &ZipperHandle) -> ZipperRef<'_, L> {
        let s0 = h.first_middle_step();
        let mut e = self.get_subexpr(&h.path_o);
        let span_ol = e.kids.get_subspan(h.i_ol, s0.left_index());
        let span_or = e.kids.get_subspan(s0.right_index(), h.i_or);
        let mut middle = ContextRef(vec![]);
        for s in &h.path_m.0 {
            let (th, e_sub) = e.at_kid(*s);
            middle.0.push(th);
            e = e_sub;
        }
        ZipperRef {
            span_ol,
            span_or,
            middle,
        }
    }

    pub fn into_tooth(self, i: Index) -> Tooth<L> {
        let (l, r) = self.kids.into_index(i);
        Tooth {
            label: self.label,
            span_l: l,
            span_r: r,
        }
    }

    pub fn at_kid(&self, s: Step) -> (ToothRef<'_, L>, &Self) {
        (
            ToothRef {
                label: &self.label,
                span_l: self
                    .kids
                    .get_subspan(self.kids.leftmost_index(), s.left_index()),
                span_r: self
                    .kids
                    .get_subspan(s.right_index(), self.kids.rightmost_index()),
            },
            self.kids.at_kid(s),
        )
    }

    pub fn get_subexpr(&self, path: &Path) -> &Self {
        let mut e = self;
        for s in &path.0 {
            e = e.at_kid(*s).1;
        }
        e
    }

    pub fn at_subexpr(&self, path: &Path) -> (ContextRef<'_, L>, &Self) {
        let mut ths = vec![];
        let mut e = self;
        for s in &path.0 {
            let (th, e_sub) = e.at_kid(*s);
            ths.push(th);
            e = e_sub;
        }
        (ContextRef(ths), e)
    }

    pub fn into_context(self, p: &Point) -> Context<L> {
        let (mut ctx, e) = self.into_subexpr(&p.path);
        ctx.0.push(e.into_tooth(p.i));
        ctx
    }

    pub fn at_tooth(&self, i: Index) -> ToothRef<'_, L> {
        ToothRef {
            label: &self.label,
            span_l: self.kids.get_subspan(self.kids.leftmost_index(), i),
            span_r: self.kids.get_subspan(i, self.kids.rightmost_index()),
        }
    }
}

// -----------------------------------------------------------------------------
// Fragment
// -----------------------------------------------------------------------------

/// A fragment of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Fragment<L> {
    Span(Span<L>),
    Zipper(Zipper<L>),
}

impl<L: Display> Display for Fragment<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Span(span) => span.fmt(f),
            Self::Zipper(zipper) => zipper.fmt(f),
        }
    }
}

impl<L: Debug + Display> Fragment<L> {
    pub fn to_ref(&self) -> FragmentRef<'_, L> {
        match self {
            Self::Span(span) => FragmentRef::Span(span.to_ref()),
            Self::Zipper(zipper) => FragmentRef::Zipper(ZipperRef {
                span_ol: zipper.span_ol.to_ref(),
                span_or: zipper.span_or.to_ref(),
                middle: zipper.middle.to_ref(),
            }),
        }
    }

    fn norm(self) -> Self {
        match self {
            Self::Zipper(zipper)
                if zipper.span_ol.0.is_empty()
                    && zipper.span_or.0.is_empty()
                    && zipper.middle.0.is_empty() =>
            {
                Self::Span(Span::empty())
            }
            frag => frag,
        }
    }
}

// -----------------------------------------------------------------------------
// Span
// -----------------------------------------------------------------------------

/// A span of [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span<L>(pub Vec<Expr<L>>);

#[macro_export]
macro_rules! span {
    ( $( $e:expr ),* $(,)? ) => {
        Span(vec![$( $e ),*])
    };
}

impl<L: Display> Display for Span<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "span!{}", display_slice(&self.0))
    }
}

impl<L: Debug + Display> Span<L> {
    pub fn replace_subspan(&mut self, i_l: Index, i_r: Index, new_span: Self) -> Self {
        trace!(target: "expr.replace_subspan", "self     = {self}");
        trace!(target: "expr.replace_subspan", "i_l      = {i_l}");
        trace!(target: "expr.replace_subspan", "i_r      = {i_r}");
        trace!(target: "expr.replace_subspan", "new_span = {new_span}");
        Self(self.0.splice(i_l.0..i_r.0, new_span.0).collect())
    }

    pub fn into_zipper(self, p: &Point) -> Zipper<L> {
        trace!(target: "expr.into_zipper", "self = {self}");
        trace!(target: "expr.into_zipper", "p    = {p}");

        if let Some(s) = p.path.0.first() {
            self.assert_step_in_bounds(*s);
            let (span_ol, e, span_or) = self.into_kid(*s);
            trace!(target: "expr.into_zipper", "span_ol = {span_ol}");
            trace!(target: "expr.into_zipper", "e       = {e}");
            trace!(target: "expr.into_zipper", "span_or = {span_or}");
            let middle = e.into_context(&Point::new(Path(p.path.0[1..].to_vec()), p.i));
            Zipper {
                span_ol,
                span_or,
                middle,
            }
        } else {
            let (span_ol, span_or) = self.into_index(p.i);
            Zipper {
                span_ol,
                span_or,
                middle: Context::empty(),
            }
        }
    }

    pub fn into_kid(self, s: Step) -> (Self, Expr<L>, Self) {
        let (es_l, e, es_r) = extract_from_vec_at_index(self.0, s.0).expect("index to valid");
        (Self(es_l), e, Self(es_r))
    }

    pub fn into_index(self, i: Index) -> (Self, Self) {
        let (es_l, es_r) = split_vec_at_index(self.0, i.0);
        (Self(es_l), Self(es_r))
    }

    pub fn empty() -> Self {
        Self(vec![])
    }

    pub fn assert_step_in_bounds(&self, s: Step) {
        assert!(s.0 < self.0.len(), "step out of bounds");
    }

    pub fn assert_index_in_bounds(&self, i: Index) {
        assert!(i.0 <= self.0.len(), "index out of bounds");
    }

    pub fn at_kid(&self, step: Step) -> &Expr<L> {
        self.0.get(step.0).expect("step in bounds")
    }

    pub fn iter_steps_and_kids(&self) -> impl Iterator<Item = (Step, &Expr<L>)> {
        self.0.iter().enumerate().map(|(i, e)| (Step(i), e))
    }

    pub fn rightmost_index(&self) -> Index {
        Index(self.0.len())
    }

    pub fn leftmost_index(&self) -> Index {
        Index(0)
    }

    pub fn get_kid_mut(&mut self, s: Step) -> &mut Expr<L> {
        self.assert_step_in_bounds(s);
        &mut self.0[s.0]
    }

    pub fn offset(&self) -> Offset {
        Offset(self.0.len())
    }

    pub fn get_subspan<'a>(&'a self, i_l: Index, i_r: Index) -> SpanRef<'a, L> {
        &self.0[i_l.0..i_r.0]
    }

    pub fn concat(mut self, mut other: Self) -> Self {
        self.0.append(&mut other.0);
        self
    }

    fn to_ref(&self) -> SpanRef<'_, L> {
        self.0.as_slice()
    }
}

// -----------------------------------------------------------------------------
// Zipper
// -----------------------------------------------------------------------------

/// A zipper between two [`SpanHandle`]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper<L> {
    pub span_ol: Span<L>,
    pub span_or: Span<L>,
    pub middle: Context<L>,
}

#[macro_export]
macro_rules! zipper {
    ([ $( $e_l:expr ),* $(,)? ], [ $( $th:expr ),* $(,)? ], [ $( $e_r:expr ),* $(,)? ]) => {
        Zipper {
            span_ol: span![$( $e_l ),*],
            span_or: span![$( $e_r ),*],
            middle: context![$( $th ),*],
        }
    };
}

impl<L: Display> Display for Zipper<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "zipper![{}, {}, {}]",
            display_slice(self.span_ol.0.as_slice()),
            display_slice(self.middle.0.as_slice()),
            display_slice(self.span_or.0.as_slice())
        )
    }
}

impl<L: Debug + Display> Zipper<L> {
    pub fn surround(self, span: Span<L>) -> Span<L> {
        match self.middle.surround_span(span) {
            Ok(e) => self.span_ol.concat(Span(vec![e])).concat(self.span_or),
            Err(span) => self.span_ol.concat(span).concat(self.span_or),
        }
    }

    pub fn inner_point(&self) -> Point {
        let s0 = Step(0).add_offset(&self.span_ol.offset());
        let mut path = self.middle.inner_path();
        path.0.insert(0, s0);
        let s = path.0.pop().expect("path to be non-empty");
        Point {
            path,
            i: s.left_index(),
        }
    }

    fn empty() -> Self {
        Self {
            span_ol: Span::empty(),
            span_or: Span::empty(),
            middle: Context::empty(),
        }
    }
}

// -----------------------------------------------------------------------------
// Refs
// -----------------------------------------------------------------------------

// TODO: turn this into: pub struct SpanRef<'a, L>(&'a [Expr<L>]);
pub type SpanRef<'a, L> = &'a [Expr<L>];

pub enum FragmentRef<'a, L> {
    Span(SpanRef<'a, L>),
    Zipper(ZipperRef<'a, L>),
}

impl<'a, L> FragmentRef<'a, L> {
    pub fn to_owned(&self) -> Fragment<L>
    where
        L: Clone,
    {
        match self {
            FragmentRef::Span(span) => Fragment::Span(Span(span.to_vec())),
            FragmentRef::Zipper(zipper) => Fragment::Zipper(zipper.to_owned()),
        }
    }

    pub fn map_to_owned<L2, F>(&self, f: &mut F) -> Fragment<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            FragmentRef::Span(span) => {
                Fragment::Span(Span(span.iter().map(|e| e.map(f)).collect()))
            }
            FragmentRef::Zipper(zipper) => Fragment::Zipper(zipper.map_to_owned(f)),
        }
    }
}

pub struct ToothRef<'a, L> {
    pub label: &'a L,
    pub span_l: SpanRef<'a, L>,
    pub span_r: SpanRef<'a, L>,
}

impl<'a, L> ToothRef<'a, L> {
    pub fn to_owned(&self) -> Tooth<L>
    where
        L: Clone,
    {
        Tooth {
            label: self.label.clone(),
            span_l: Span(self.span_l.to_vec()),
            span_r: Span(self.span_r.to_vec()),
        }
    }

    fn map_to_owned<L2, F>(&self, f: &mut F) -> Tooth<L2>
    where
        F: FnMut(&L) -> L2,
    {
        Tooth {
            label: f(self.label),
            span_l: Span(self.span_l.iter().map(|e| e.map(f)).collect()),
            span_r: Span(self.span_r.iter().map(|e| e.map(f)).collect()),
        }
    }
}

pub struct ContextRef<'a, L>(pub Vec<ToothRef<'a, L>>);

impl<'a, L> ContextRef<'a, L> {
    pub fn to_owned(&self) -> Context<L>
    where
        L: Clone,
    {
        Context(self.0.iter().map(|th| th.to_owned()).collect())
    }
}

pub struct ZipperRef<'a, L> {
    pub span_ol: SpanRef<'a, L>,
    pub span_or: SpanRef<'a, L>,
    pub middle: ContextRef<'a, L>,
}

impl<'a, L> ZipperRef<'a, L> {
    pub fn to_owned(&self) -> Zipper<L>
    where
        L: Clone,
    {
        Zipper {
            span_ol: Span(self.span_ol.to_vec()),
            span_or: Span(self.span_or.to_vec()),
            middle: Context(self.middle.0.iter().map(|th| th.to_owned()).collect()),
        }
    }

    pub fn map_to_owned<L2, F>(&self, f: &mut F) -> Zipper<L2>
    where
        F: FnMut(&L) -> L2,
    {
        Zipper {
            span_ol: Span(self.span_ol.iter().map(|e| e.map(f)).collect()),
            span_or: Span(self.span_or.iter().map(|e| e.map(f)).collect()),
            middle: Context(self.middle.0.iter().map(|th| th.map_to_owned(f)).collect()),
        }
    }
}

// -----------------------------------------------------------------------------
// ExprContext
// -----------------------------------------------------------------------------

/// A context around an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Context<L>(pub Vec<Tooth<L>>);

#[macro_export]
macro_rules! context {
    ( $( $th:expr ),* $(,)? ) => {
        Context(vec![ $( $th ),* ])
    };
}

impl<L: Display> Display for Context<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "context!{}", display_slice(&self.0))
    }
}

impl<L: Debug + Display> Context<L> {
    pub fn empty() -> Self {
        Self(vec![])
    }

    /// Two possibilities:
    /// - If the context is NOT flat (non-empty vector of teeth), then [Ok],
    ///   forms an [Expr].
    /// - If the context is flat (empty vector of teeth), then [Err], forms a
    ///   span by just sandwhiching the input [Span] between the two [Context]s'
    ///   [Span]s
    pub fn surround_span(self, span: Span<L>) -> Result<Expr<L>, Span<L>> {
        let mut ths = self.0;
        if let Some(th0) = ths.pop() {
            let mut e = th0.surround(span);
            while let Some(th) = ths.pop() {
                e = th.surround_expr(e);
            }
            Ok(e)
        } else {
            Err(span)
        }
    }

    fn inner_path(&self) -> Path {
        Path(self.0.iter().map(|th| th.inner_step()).collect())
    }

    fn to_ref(&self) -> ContextRef<'_, L> {
        ContextRef(self.0.iter().map(|th| th.to_ref()).collect())
    }
}

/// A tooth of an [Expr] around a [Step].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub label: L,
    pub span_l: Span<L>,
    pub span_r: Span<L>,
}

#[macro_export]
macro_rules! tooth {
    ($label:expr, [ $( $e_l:expr ),* $(,)? ], [ $( $e_r:expr ),* $(,)? ]) => {
        Tooth {
            label: $label,
            span_l: span![$( $e_l ),*],
            span_r: span![$( $e_r ),*],
        }
    };
}

impl<L: Display> Display for Tooth<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "tooth![{}, {}, {}]",
            self.label,
            display_slice(self.span_l.0.as_slice()),
            display_slice(self.span_r.0.as_slice())
        )
    }
}

impl<L: Debug + Display> Tooth<L> {
    pub fn surround(self, span: Span<L>) -> Expr<L> {
        Expr {
            label: self.label,
            kids: self.span_l.concat(span).concat(self.span_r),
        }
    }

    pub fn surround_expr(self, e: Expr<L>) -> Expr<L> {
        Expr {
            label: self.label,
            kids: self.span_l.concat(Span(vec![e])).concat(self.span_r),
        }
    }

    fn inner_step(&self) -> Step {
        Step(0).add_offset(&self.span_l.offset())
    }

    fn to_ref(&self) -> ToothRef<'_, L> {
        ToothRef {
            label: &self.label,
            span_l: self.span_l.to_ref(),
            span_r: self.span_r.to_ref(),
        }
    }
}

// -----------------------------------------------------------------------------
// Miscellaneous
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct Offset(usize);

impl Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "δ{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MoveError {
    Boundary,
    Undefined,
    Invalid,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum CycleDir {
    Prev,
    Next,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum MoveDir {
    Prev,
    Next,
}

impl Display for MoveDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

// -----------------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn move_ex1() {
        let source = Handle::Zipper(zipper_handle![[], 0, 1, [0], 0, 0, ZipperFocus::InnerLeft]);
        let e = ex!["root", [ex!["a", [ex!["b", []]]]]];
        let target = point![[0, 0], 0];
        let result = source.drag(&e, &target);
        assert_eq!(
            result,
            Some(Handle::Zipper(ZipperHandle {
                path_o: path![],
                i_ol: Index(0),
                i_or: Index(1),
                path_m: path![0, 0],
                i_il: Index(0),
                i_ir: Index(0),
                focus: ZipperFocus::InnerLeft
            }))
        );
    }

    #[test]
    pub fn insert_zipper_ex1() {
        let mut e = ex!["root", [ex!["a", []]]];
        let h = Handle::Point(point![[0], 0]);
        let frag = Fragment::Zipper(zipper![[], [tooth!["b", [], []]], []]);
        let h = e.insert_fragment(h, frag);
        assert_eq!(e, ex!["root", [ex!["a", [ex!["b", []]]]]]);
        assert_eq!(
            h,
            Handle::Zipper(zipper_handle![[0], 0, 1, [0], 0, 0, ZipperFocus::InnerLeft])
        );
    }

    #[test]
    pub fn cut_zipper_ex1() {
        let mut e = ex!["root", [ex!["a", []]]];
        let h = Handle::Zipper(zipper_handle![[], 0, 1, [0], 0, 0, ZipperFocus::InnerLeft]);
        let (frag, h) = e.cut(&h).expect("cut to succeed");
        assert_eq!(e, ex!["root", []]);
        assert_eq!(h, Handle::Span(span_handle![[], 0, 0, SpanFocus::Left]));
        assert_eq!(
            frag,
            Fragment::Zipper(zipper![[], [tooth!["a", [], []]], []])
        );
    }

    #[test]
    pub fn into_zipper_ex1() {
        let e = span![ex!["a", []]];
        let p = point![[0], 0];
        let z = e.into_zipper(&p);
        assert_eq!(z, zipper![[], [tooth!["a", [], []]], []]);
    }

    #[test]
    pub fn insert_zipper_ex2() {
        let mut e = ex!["root", [ex!["a", []]]];
        let h = span_handle![[], 0, 1, SpanFocus::Right];
        let frag = zipper![[], [tooth!["b", [], []]], []];
        let h = e.insert_fragment(Handle::Span(h), Fragment::Zipper(frag));
        assert_eq!(e, ex!["root", [ex!["b", [ex!["a", []]]]]]);
        assert_eq!(
            h,
            Handle::Zipper(zipper_handle![[], 0, 1, [0], 0, 1, ZipperFocus::InnerRight])
        );
    }

    #[test]
    pub fn cut_zipper_ex2() {
        let mut e = ex![
            "root",
            [
                ex!["a", [ex!["b", [ex!["c", []]]], ex!["b", []], ex!["c", []]]],
                ex![
                    "this is a test to see how long a constructor i can make",
                    [ex!["pretty long, it seems!", []]]
                ]
            ]
        ];
        let h = zipper_handle![[], 1, 2, [1], 0, 1, ZipperFocus::InnerLeft];
        let frag = e.cut(&Handle::Zipper(h));
        match frag {
            None => println!("frag = None"),
            Some((frag, h)) => {
                println!("frag = {frag}");
                println!("h = {h}");
            }
        }
        // TODO: specify correct output
    }
}
