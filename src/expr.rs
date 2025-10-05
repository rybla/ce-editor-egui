use std::fmt::Debug;

use crate::utility::{extract_from_vec_at_index, split_vec_at_index};

// -----------------------------------------------------------------------------
// Index
// -----------------------------------------------------------------------------

/// An index between kids, or left the first kid, or right of the last kid of an
/// [Expr].
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Index(pub usize);

impl Index {
    pub fn is_left_of_index(&self, other: &Index) -> bool {
        self.0 < other.0
    }

    pub fn is_left_of_step(&self, s: &Step) -> bool {
        self.0 <= s.0
    }

    pub fn sub_offset(&self, offset: &Offset) -> Index {
        Index(self.0 - offset.0)
    }

    pub fn add_offset(&self, offset: &Offset) -> Index {
        Index(self.0 + offset.0)
    }

    pub fn is_right_of_step(&self, s: &Step) -> bool {
        self.0 > s.0
    }

    pub fn to_offset(&self) -> Offset {
        Offset(self.0)
    }
}

// -----------------------------------------------------------------------------
// Step
// -----------------------------------------------------------------------------

/// A step from an [Expr] to one of its kids.
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(pub usize);

impl Step {
    pub fn left_step(&self) -> Step {
        self.sub_offset(&Offset(1))
    }

    pub fn right_step(&self) -> Step {
        self.add_offset(&Offset(1))
    }

    pub fn left_index(&self) -> Index {
        Index(self.0)
    }

    pub fn right_index(&self) -> Index {
        Index(self.0 + 1)
    }

    pub fn is_left_of_index(&self, i: &Index) -> bool {
        self.0 <= i.0
    }

    pub fn sub_offset(&self, offset: &Offset) -> Step {
        Step(self.0 - offset.0)
    }

    pub fn add_offset(&self, offset: &Offset) -> Step {
        Step(self.0 + offset.0)
    }
}

// -----------------------------------------------------------------------------
// Path
// -----------------------------------------------------------------------------

/// A path from the top [Expr] to an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Path(pub Vec<Step>);

impl Path {
    pub fn add_offset_to_first_step(self, offset: &Offset) -> Option<Self> {
        let mut path = self.0;
        if path.is_empty() {
            return None;
        }
        let s0 = path.remove(0);
        path.insert(0, s0.add_offset(offset));
        Some(Path(path))
    }

    pub fn sub_offset_to_first_step(self, offset: &Offset) -> Option<Self> {
        let mut path = self.0;
        if path.is_empty() {
            return None;
        }
        let s0 = path.remove(0);
        path.insert(0, s0.sub_offset(offset));
        Some(Path(path))
    }

    pub fn empty() -> Self {
        Path(vec![])
    }

    /// If `self` is a sibling of an ancestor of `other`, then is [Option::Some] of:
    ///   - the [Step] into the parent of `self` that goes down towards `other`
    ///   - the path `path` such that `self.path.append(path) = other.path`
    /// Otherwise is [Option::None].
    pub fn is_sibling_of_ancestor_of(&self, other: &Self) -> Option<(Step, Path)> {
        for (i, s0) in self.0.iter().enumerate() {
            let s1 = other.0.get(i)?;
            if !(s0 == s1) {
                return None;
            }
        }
        let mut other_suffix = other.0[self.0.len()..].to_vec();
        let s = other_suffix.remove(0);
        Some((s, Path(other_suffix)))
    }

    pub fn append(self, other: Self) -> Path {
        let mut path0 = self.0;
        let mut path1 = other.0;
        path0.append(&mut path1);
        Path(path0)
    }

    /// If `self` is a prefix of `other`, then return the path `suffix` such
    /// that `self.append(suffix) == other`
    pub fn diff(&self, other: &Self) -> Option<Self> {
        let suffix = other.0.as_slice().strip_suffix(self.0.as_slice())?;
        Some(Path(suffix.to_vec()))
    }

    pub fn is_prefix_of(&self, other: &Self) -> bool {
        other.0.as_slice().strip_suffix(self.0.as_slice()).is_some()
    }
}

// -----------------------------------------------------------------------------
// Point
// -----------------------------------------------------------------------------

/// A point between two [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Point {
    pub path: Path,
    pub i: Index,
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
    pub fn is_left_adjacent_to_point(&self, other: &Point) -> bool {
        self.path == other.path && self.i.is_left_of_index(&other.i)
    }

    pub fn move_dir<L: Debug + Clone>(
        &mut self,
        expr: &Expr<L>,
        dir: &MoveDir,
    ) -> Result<(), MoveError> {
        let sub_expr = expr.at_path(&self.path);
        let leftmost = sub_expr.kids.leftmost_index();
        let rightmost = sub_expr.kids.rightmost_index();
        match dir {
            MoveDir::Prev if leftmost.is_left_of_index(&self.i) => {
                self.i = self.i.sub_offset(&Offset(1));
                Ok(())
            }
            MoveDir::Prev => {
                let step = self.path.0.pop().ok_or(MoveError::Boundary)?;
                self.i = step.left_index();
                Ok(())
            }
            MoveDir::Next if self.i.is_left_of_index(&rightmost) => {
                self.i = self.i.add_offset(&Offset(1));
                Ok(())
            }
            MoveDir::Next => {
                let step = self.path.0.pop().ok_or(MoveError::Boundary)?;
                self.i = step.right_index();
                Ok(())
            }
        }
    }

    pub fn to_empty_span_handle(&self) -> SpanHandle {
        SpanHandle {
            path: self.path.clone(),
            i_l: self.i,
            i_r: self.i,
            focus: SpanFocus::Left,
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
}

// -----------------------------------------------------------------------------
// Handle
// -----------------------------------------------------------------------------

/// A handle for a [Fragment].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Handle {
    Point(Point),
    Span(SpanHandle),
    Zipper(ZipperHandle),
}

impl Default for Handle {
    fn default() -> Self {
        Handle::Point(Default::default())
    }
}

impl Handle {
    /// When a [SpanHandle] is empty (the two endpoints are the same),
    /// then collapse it to a [Point].
    ///
    /// When the middle path of the zipper handle is empty, then collapse
    /// it to a [SpanHandle].
    pub fn norm(self) -> Handle {
        match &self {
            Handle::Span(h) if h.i_l == h.i_r => Handle::Point(h.focus_point()),
            // NOTE: the choice to return h_o rather than h_i here should be
            // arbitrary, but perhaps there might be edge cases where it matters
            // which one, even when the middle [Path] is empty? Nothing really
            // should depend on using a "flat" ZipperHandle, right?
            Handle::Zipper(h) if h.path_m.0.is_empty() => Handle::Span(h.handle_o()),
            _ => self,
        }
    }

    pub fn move_up(&mut self) -> Result<(), MoveError> {
        match self {
            Handle::Point(point) => {
                let mut steps = point.path.0.drain(..).collect::<Vec<_>>();
                let s = steps.pop().ok_or(MoveError::Boundary)?;
                *self = Handle::Span(SpanHandle {
                    path: Path(steps),
                    i_l: s.left_index(),
                    i_r: s.right_index(),
                    focus: SpanFocus::Left,
                });
                Ok(())
            }
            Handle::Span(_) => self.escape(),
            Handle::Zipper(_) => self.escape(),
        }
    }

    pub fn move_dir<L: Debug + Clone>(
        &mut self,
        expr: &Expr<L>,
        dir: &MoveDir,
    ) -> Result<(), MoveError> {
        match self {
            Handle::Point(point) => {
                point.move_dir(expr, dir)?;
                Ok(())
            }
            Handle::Span(h) => match dir {
                MoveDir::Prev => {
                    *self = Handle::Point(h.p_l());
                    Ok(())
                }
                MoveDir::Next => {
                    *self = Handle::Point(h.p_r());
                    Ok(())
                }
            },
            Handle::Zipper(h) => {
                *self = Handle::Point(h.focus_point());
                Ok(())
            }
        }
    }

    pub fn focus_point(&self) -> Point {
        match self {
            Handle::Point(point) => point.clone(),
            Handle::Span(handle) => handle.focus_point(),
            Handle::Zipper(handle) => handle.focus_point(),
        }
    }

    pub fn rotate_focus_dir(&mut self, dir: &MoveDir) {
        match self {
            Handle::Point(_handle) => (),
            Handle::Span(handle) => handle.focus = handle.focus.rotate_dir(dir),
            Handle::Zipper(handle) => handle.focus = handle.focus.rotate_dir(dir),
        }
    }

    pub fn escape(&mut self) -> Result<(), MoveError> {
        match self {
            Handle::Point(_) => Err(MoveError::Undefined),
            Handle::Span(h) => {
                *self = Handle::Point(h.focus_point());
                Ok(())
            }
            Handle::Zipper(h) => {
                *self = Handle::Point(h.focus_point());
                Ok(())
            }
        }
    }

    pub fn drag<L: Debug + Clone>(self, e: &Expr<L>, target: &Point) -> Option<Handle> {
        match self {
            Handle::Point(source) => {
                if target.path == source.path {
                    if target.i.is_left_of_index(&source.i) {
                        Some(Handle::Span(SpanHandle {
                            path: source.path,
                            i_l: target.i,
                            i_r: source.i,
                            focus: SpanFocus::Left,
                        }))
                    } else if target.i == source.i {
                        Some(Handle::Point(source))
                    } else if source.i.is_left_of_index(&target.i) {
                        Some(Handle::Span(SpanHandle {
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
                    #[allow(irrefutable_let_patterns)]
                    if let p_il = &source
                        && let p_ol = &target
                        && let Some(path_m) = p_ol.path.diff(&p_il.path)
                        && let Some(s) = path_m.0.first()
                        && p_ol.i.is_left_of_step(s)
                    {
                        let path_o = p_ol.path.clone();
                        let i_or = s.right_index();
                        let i_ol = p_ol.i;
                        let i_il = p_il.i;
                        let i_ir = e.at_path(&p_il.path).kids.rightmost_index();
                        let focus = ZipperFocus::OuterLeft;
                        Some(Handle::Zipper(ZipperHandle {
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
                        && s.is_left_of_index(&p_or.i)
                    {
                        let path_o = p_or.path.clone();
                        let i_ol = s.left_index();
                        let i_or = p_or.i;
                        let i_il = e.at_path(&p_ir.path).kids.leftmost_index();
                        let i_ir = p_ir.i;
                        let focus = ZipperFocus::OuterRight;
                        Some(Handle::Zipper(ZipperHandle {
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
                        && p_ol.i.is_left_of_step(s)
                    {
                        let path_o = p_ol.path.clone();
                        let i_ol = p_ol.i;
                        let i_or = s.right_index();
                        let i_il = p_il.i;
                        let i_ir = e.at_path(&p_il.path).kids.rightmost_index();
                        let focus = ZipperFocus::InnerLeft;
                        Some(Handle::Zipper(ZipperHandle {
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
                        && s.is_left_of_index(&p_or.i)
                    {
                        let path_o = p_or.path.clone();
                        let i_ol = s.left_index();
                        let i_or = p_or.i;
                        let i_il = e.at_path(&p_ir.path).kids.leftmost_index();
                        let i_ir = p_ir.i;
                        let focus = ZipperFocus::InnerRight;
                        Some(Handle::Zipper(ZipperHandle {
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
            Handle::Span(handle) => Handle::Point(handle.focus_point()).drag(e, target),
            Handle::Zipper(source) => {
                let path_i: Path = source.path_i();

                // adjust i_ol
                if target.path.is_prefix_of(&path_i) {
                    Handle::Point(source.p_il())
                        .drag(e, target)
                        .and_then(|h| match h {
                            Handle::Zipper(h) => {
                                let mut h = h;
                                h.i_or = if target.is_left_adjacent_to_point(&Point {
                                    path: source.path_o,
                                    i: source.path_m.0.first().unwrap().left_index(),
                                }) {
                                    source.i_or
                                } else {
                                    h.i_or
                                };
                                h.i_il = source.i_il;
                                h.i_ir = source.i_ir;
                                Some(Handle::Zipper(h))
                            }
                            _ => Some(h),
                        })
                } else
                // adjust i_or
                if target.path.is_prefix_of(&path_i) {
                    Handle::Point(source.p_ir())
                        .drag(e, target)
                        .and_then(|h| match h {
                            Handle::Zipper(h) => {
                                let mut h = h;
                                h.i_ol = if (Point {
                                    path: source.path_o,
                                    i: source.path_m.0.first().unwrap().right_index(),
                                })
                                .is_left_adjacent_to_point(target)
                                {
                                    source.i_ol
                                } else {
                                    h.i_ol
                                };
                                h.i_il = source.i_il;
                                h.i_ir = source.i_ir;
                                Some(Handle::Zipper(h))
                            }
                            _ => Some(h),
                        })
                } else
                // adjust i_il
                if source.path_o.is_prefix_of(&target.path) {
                    Handle::Point(source.p_ol())
                        .drag(e, target)
                        .and_then(|h| match h {
                            Handle::Zipper(h) => {
                                let mut h = h;
                                h.i_ol = source.i_ol;
                                h.i_or = source.i_or;
                                h.i_ir = if target.is_left_adjacent_to_point(&source.p_ir()) {
                                    source.i_ir
                                } else {
                                    h.i_ir
                                };
                                Some(Handle::Zipper(h))
                            }
                            _ => Some(h),
                        })
                } else
                // adjust i_ir
                if false {
                    Handle::Point(source.p_or())
                        .drag(e, target)
                        .and_then(|h| match h {
                            Handle::Zipper(h) => {
                                let mut h = h;
                                h.i_ol = source.i_ol;
                                h.i_or = source.i_or;
                                h.i_il = if source.p_il().is_left_adjacent_to_point(target) {
                                    source.i_il
                                } else {
                                    h.i_il
                                };
                                Some(Handle::Zipper(h))
                            }
                            _ => Some(h),
                        })
                } else {
                    None
                }
            }
        }
    }

    pub fn contains_point(&self, p: &Point) -> bool {
        match self {
            Handle::Point(_) => false,
            Handle::Span(h) => h.contains_point(p),
            Handle::Zipper(h) => h.contains_point(p),
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        match self {
            Handle::Point(_) => false,
            Handle::Span(h) => h.contains_path(path),
            Handle::Zipper(h) => h.contains_path(path),
        }
    }
}

// -----------------------------------------------------------------------------
// SpanHandle
// -----------------------------------------------------------------------------

/// A handle for a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle {
    pub path: Path,
    pub i_l: Index,
    pub i_r: Index,
    pub focus: SpanFocus,
}

impl SpanHandle {
    pub fn focus_point(&self) -> Point {
        match self.focus {
            SpanFocus::Left => self.p_l(),
            SpanFocus::Right => self.p_r(),
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
        self.path == p.path && self.i_l.is_left_of_index(&p.i) && p.i.is_left_of_index(&self.i_r)
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        &self.path == path
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

impl SpanFocus {
    pub fn rotate_dir(&self, dir: &MoveDir) -> SpanFocus {
        match self {
            SpanFocus::Left => match dir {
                MoveDir::Prev => SpanFocus::Right,
                MoveDir::Next => SpanFocus::Right,
            },
            SpanFocus::Right => match dir {
                MoveDir::Prev => SpanFocus::Left,
                MoveDir::Next => SpanFocus::Left,
            },
        }
    }

    pub fn to_outer_zipper_focus(&self) -> ZipperFocus {
        match self {
            SpanFocus::Left => ZipperFocus::OuterLeft,
            SpanFocus::Right => ZipperFocus::OuterRight,
        }
    }

    pub fn to_inner_zipper_focus(&self) -> ZipperFocus {
        match self {
            SpanFocus::Left => ZipperFocus::InnerLeft,
            SpanFocus::Right => ZipperFocus::InnerRight,
        }
    }
}

// -----------------------------------------------------------------------------
// ZipperHandle
// -----------------------------------------------------------------------------

/// A handle for a [Zipper].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandle {
    pub path_o: Path,
    pub i_ol: Index,
    pub i_or: Index,
    pub path_m: Path,
    pub i_il: Index,
    pub i_ir: Index,
    pub focus: ZipperFocus,
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

    pub fn p_or<'a>(&'a self) -> Point {
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

    /// Outer [SpanHandle].
    pub fn handle_o(&self) -> SpanHandle {
        SpanHandle {
            path: self.path_o.clone(),
            i_l: self.i_ol,
            i_r: self.i_or,
            focus: self.focus.to_span_focus(),
        }
    }

    /// Inner [SpanHandle].
    pub fn handle_i(&self) -> SpanHandle {
        SpanHandle {
            path: self.path_i(),
            i_l: self.i_il,
            i_r: self.i_ir,
            focus: self.focus.to_span_focus(),
        }
    }

    pub fn first_middle_step<'a>(&'a self) -> &'a Step {
        self.path_m
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
impl ZipperFocus {
    pub fn rotate_dir(&self, dir: &MoveDir) -> ZipperFocus {
        match self {
            ZipperFocus::OuterLeft => match dir {
                MoveDir::Prev => ZipperFocus::OuterRight,
                MoveDir::Next => ZipperFocus::InnerLeft,
            },
            ZipperFocus::InnerLeft => match dir {
                MoveDir::Prev => ZipperFocus::OuterLeft,
                MoveDir::Next => ZipperFocus::InnerRight,
            },
            ZipperFocus::InnerRight => match dir {
                MoveDir::Prev => ZipperFocus::InnerLeft,
                MoveDir::Next => ZipperFocus::OuterRight,
            },
            ZipperFocus::OuterRight => match dir {
                MoveDir::Prev => ZipperFocus::InnerRight,
                MoveDir::Next => ZipperFocus::OuterLeft,
            },
        }
    }

    pub fn to_span_focus(&self) -> SpanFocus {
        match self {
            ZipperFocus::OuterLeft => SpanFocus::Left,
            ZipperFocus::InnerLeft => SpanFocus::Left,
            ZipperFocus::InnerRight => SpanFocus::Right,
            ZipperFocus::OuterRight => SpanFocus::Right,
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

impl<L: Debug + Clone> Expr<L> {
    pub fn new(label: L, kids: Span<L>) -> Self {
        Self { label, kids }
    }

    pub fn height(&self) -> u32 {
        self.kids
            .0
            .iter()
            .fold(0, |h, e| std::cmp::max(h, 1 + e.height()))
    }

    pub fn at_handle_cloned(&self, h: &Handle) -> Option<Fragment<L>> {
        match h {
            Handle::Point(_) => None,
            Handle::Span(h) => Some(Fragment::Span(self.at_span_handle(h))),
            Handle::Zipper(h) => Some(Fragment::Zipper(self.at_zipper_handle(h))),
        }
    }

    pub fn insert_fragment_at_handle(&mut self, h: Handle, frag: Fragment<L>) -> Handle {
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

    pub fn at_handle(&self, h: &Handle) -> Option<Fragment<L>> {
        match h {
            Handle::Point(_) => None,
            Handle::Span(h) => Some(Fragment::Span(self.at_span_handle(h))),
            Handle::Zipper(h) => Some(Fragment::Zipper(self.at_zipper_handle(h))),
        }
    }

    /// Replaces the span at the handle with the new span and returns the old
    /// span.
    pub fn replace_span(&mut self, h: &SpanHandle, new_span: Span<L>) -> (Span<L>, SpanHandle) {
        let parent = self.at_path_mut(&h.path);
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
        // take the inner span
        let e_o = self.at_path_mut(&h.path_o);
        let (span_i, new_handle_m) = e_o.replace_span(
            &SpanHandle {
                path: h.path_m.clone(),
                i_l: h.i_il,
                i_r: h.i_ir,
                focus: h.focus.to_span_focus(),
            },
            Span::empty(),
        );

        // wrap the inner span with the zipper
        let new_span_m: Span<L> = new_zipper.surround(span_i);
        let new_span_m_offset = &new_span_m.offset();

        // replace outer span handle of zipper with result
        // let (old_span_m, new_handle_o) = self.replace_span(&h.handle_o(), new_span_m);
        let old_span_m = e_o.kids.replace_sub_span(&h.i_il, &h.i_ir, new_span_m);

        let old_zipper: Zipper<L> = old_span_m.into_zipper(&Point {
            // TODO: handle None case
            path: h
                .path_m
                .clone()
                .sub_offset_to_first_step(&h.i_il.to_offset())
                .unwrap(),
            i: h.i_ol, // TODO: Make sure this is the right index
        });

        (
            old_zipper,
            ZipperHandle {
                path_o: h.path_o.clone(),
                i_ol: h.i_ol,
                i_or: h.i_ol.add_offset(new_span_m_offset),
                path_m: new_handle_m.path,
                i_il: new_handle_m.i_l,
                i_ir: new_handle_m.i_r,
                focus: h.focus,
            },
        )
    }

    pub fn at_path(&self, path: &Path) -> &Self {
        let mut e = self;
        for s in path.0.iter() {
            e = e.at_step(s);
        }
        e
    }

    pub fn at_path_owned(self, path: &Path) -> (Context<L>, Self) {
        let mut ctx: Context<L> = Context::empty();
        let mut e = self;
        for s in path.0.iter() {
            let (th, e_kid) = e.at_step_owned(s);
            ctx.0.push(th);
            e = e_kid;
        }
        (ctx, e)
    }

    pub fn at_step(&self, step: &Step) -> &Self {
        self.kids.at_step(step)
    }

    pub fn at_step_owned(self, s: &Step) -> (Tooth<L>, Expr<L>) {
        let (left, middle, right) = self.kids.at_step_owned(s);
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
        Expr {
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

    pub fn at_path_mut(&mut self, path: &Path) -> &mut Expr<L> {
        let mut expr = self;
        for step in path.0.iter() {
            expr = expr.at_step_mut(step)
        }
        expr
    }

    pub fn at_step_mut(&mut self, step: &Step) -> &mut Expr<L> {
        self.kids.at_step_mut(step)
    }

    pub fn at_span_handle(&self, h: &SpanHandle) -> Span<L> {
        let e = self.at_path(&h.path);
        Span(e.kids.at_index_range(&h.i_l, &h.i_r).to_vec())
    }

    pub fn at_zipper_handle(&self, h: &ZipperHandle) -> Zipper<L> {
        let s0 = h.first_middle_step();
        let mut e = self.at_path(&h.path_o);
        let span_ol = Span(e.kids.at_index_range(&h.i_ol, &s0.left_index()).to_vec());
        let span_or = Span(e.kids.at_index_range(&s0.right_index(), &h.i_or).to_vec());
        let mut ths: Vec<Tooth<L>> = vec![];
        for s in h.path_m.0.iter() {
            ths.push(e.at_tooth(s));
            e = e.at_step(s);
        }
        Zipper {
            span_ol,
            span_or,
            middle: Context(ths),
        }
    }

    pub fn at_tooth(&self, s: &Step) -> Tooth<L> {
        Tooth {
            label: self.label.clone(),
            span_l: Span(
                self.kids
                    .at_index_range(&self.kids.leftmost_index(), &s.left_index())
                    .to_vec(),
            ),
            span_r: Span(
                self.kids
                    .at_index_range(&s.right_index(), &self.kids.rightmost_index())
                    .to_vec(),
            ),
        }
    }

    pub fn into_context(&self, path: &Path) -> Context<L> {
        let mut ctx = Context::empty();
        let mut e = self;
        for s in path.0.iter() {
            let th = e.at_tooth(s);
            ctx.0.push(th);
            e = e.at_step(s);
        }
        ctx
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

// -----------------------------------------------------------------------------
// Span
// -----------------------------------------------------------------------------

/// A span of [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span<L>(pub Vec<Expr<L>>);

impl<L: Debug + Clone> Span<L> {
    pub fn replace_sub_span(&mut self, i_l: &Index, i_r: &Index, new_span: Span<L>) -> Self {
        Span(self.0.splice(i_l.0..i_r.0, new_span.0).collect())
    }

    pub fn into_zipper(self, p: &Point) -> Zipper<L> {
        println!("[into_zipper]\n  - self = {:?}\n  - p = {:?}", &self, p);
        if let Some(s) = p.path.0.first() {
            self.assert_step_in_bounds(s);
            let (span_ol, e, span_or) = self.extract_at_step(s);
            let middle = e.into_context(&Path(p.path.0[1..].to_vec()));
            Zipper {
                span_ol: span_ol,
                span_or: span_or,
                middle,
            }
        } else {
            let (span_ol, span_or) = self.split_at_index(&p.i);
            Zipper {
                span_ol,
                span_or,
                middle: Context::empty(),
            }
        }
    }

    pub fn extract_at_step(self, s: &Step) -> (Span<L>, Expr<L>, Span<L>) {
        let (es_l, e, es_r) = extract_from_vec_at_index(self.0, s.0).unwrap();
        (Span(es_l), e, Span(es_r))
    }

    pub fn split_at_index(self, i: &Index) -> (Span<L>, Span<L>) {
        let (es_l, es_r) = split_vec_at_index(self.0, i.0);
        (Span(es_l), Span(es_r))
    }

    pub fn empty() -> Self {
        Span(vec![])
    }

    pub fn assert_step_in_bounds(&self, s: &Step) {
        assert!(s.0 < self.0.len())
    }

    pub fn assert_index_in_bounds(&self, i: &Index) {
        assert!(i.0 <= self.0.len())
    }

    pub fn at_step(&self, step: &Step) -> &Expr<L> {
        self.0.get(step.0).unwrap()
    }

    pub fn at_step_owned(self, s: &Step) -> (Self, Expr<L>, Self) {
        let (left, middle, right) = extract_from_vec_at_index(self.0, s.0).unwrap();
        (Span(left), middle, Span(right))
    }

    pub fn steps_and_exprs(&self) -> impl Iterator<Item = (Step, &Expr<L>)> {
        self.0.iter().enumerate().map(|(i, e)| (Step(i), e))
    }

    pub fn rightmost_index(&self) -> Index {
        Index(self.0.len())
    }

    pub fn leftmost_index(&self) -> Index {
        Index(0)
    }

    pub fn at_step_mut(&mut self, s: &Step) -> &mut Expr<L> {
        self.assert_step_in_bounds(s);
        self.0.get_mut(s.0).unwrap()
    }

    pub fn offset(&self) -> Offset {
        Offset(self.0.len())
    }

    pub fn at_index_range<'a>(&'a self, i_l: &Index, i_r: &Index) -> &'a [Expr<L>] {
        &self.0[i_l.0..i_r.0]
    }

    pub fn at_sub_span(&self, i_l: &Index, i_r: &Index) -> Span<L> {
        Span(self.at_index_range(i_l, i_r).to_vec())
    }

    pub fn concat(self, other: Self) -> Self {
        Span([self.0, other.0].concat())
    }
}

// -----------------------------------------------------------------------------
// Zipper
// -----------------------------------------------------------------------------

/// A zipper between two [SpanHandle]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper<L> {
    pub span_ol: Span<L>,
    pub span_or: Span<L>,
    pub middle: Context<L>,
}

impl<L: Debug + Clone> Zipper<L> {
    pub fn surround(self, span: Span<L>) -> Span<L> {
        match self.middle.surround_span(span) {
            Ok(e) => self.span_ol.concat(Span(vec![e])).concat(self.span_or),
            Err(span) => self.span_ol.concat(span).concat(self.span_or),
        }
    }
}

// -----------------------------------------------------------------------------
// ExprContext
// -----------------------------------------------------------------------------

/// A context around an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Context<L>(pub Vec<Tooth<L>>);

impl<L: Debug + Clone> Context<L> {
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
            return Err(span);
        }
    }
}

/// A tooth of an [Expr] around a [Step].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub label: L,
    pub span_l: Span<L>,
    pub span_r: Span<L>,
}

impl<L: Debug + Clone> Tooth<L> {
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
}

// -----------------------------------------------------------------------------
// Miscellaneous
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Offset(usize);

#[derive(Debug, Clone, Copy)]
pub enum MoveError {
    Boundary,
    Undefined,
    Invalid,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum CycleDir {
    Prev,
    Next,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum MoveDir {
    Prev,
    Next,
}
