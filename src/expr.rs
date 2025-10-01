use std::fmt::Debug;

use crate::utility::extract_from_vec_at_index;

// -----------------------------------------------------------------------------
// Index
// -----------------------------------------------------------------------------

/// An index between kids, or left the first kid, or right of the last kid of an
/// [Expr].
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Index(pub usize);

impl Index {
    pub fn is_left_of_index(&self, index: Index) -> bool {
        todo!()
    }

    pub fn is_left_of_step(&self, s: &Step) -> bool {
        todo!()
    }

    pub fn sub_offset(&self, offset: Offset) -> Index {
        todo!()
    }

    pub fn add_offset(&self, offset: Offset) -> Index {
        todo!()
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
        todo!()
    }

    pub fn right_step(&self) -> Step {
        todo!()
    }

    pub fn left_index(&self) -> Index {
        todo!()
    }

    pub fn right_index(&self) -> Index {
        todo!()
    }

    pub fn is_left_of_index(&self, index: Index) -> bool {
        todo!()
    }
}

// -----------------------------------------------------------------------------
// Path
// -----------------------------------------------------------------------------

/// A path from the top [Expr] to an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Path(pub Vec<Step>);

impl Path {
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

    pub fn p_ol(&self) -> Point {
        todo!()
    }

    pub fn p_or(&self) -> Point {
        todo!()
    }

    pub fn p_il(&self) -> Point {
        todo!()
    }

    pub fn p_ir(&self) -> Point {
        todo!()
    }
}

// -----------------------------------------------------------------------------
// Point
// -----------------------------------------------------------------------------

/// A point between two [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Point {
    pub path: Path,
    pub index: Index,
}

impl Default for Point {
    fn default() -> Self {
        Self {
            path: Path::empty(),
            index: Index(0),
        }
    }
}

impl Point {
    pub fn is_left_adjacent_to_point(&self, other: &Point) -> bool {
        self.path == other.path && self.index.is_left_of_index(other.index)
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
            MoveDir::Prev if leftmost.is_left_of_index(self.index) => {
                self.index = self.index.sub_offset(Offset(1));
                Ok(())
            }
            MoveDir::Prev => {
                let step = self.path.0.pop().ok_or(MoveError::Boundary)?;
                self.index = step.left_index();
                Ok(())
            }
            MoveDir::Next if self.index.is_left_of_index(rightmost) => {
                self.index = self.index.add_offset(Offset(1));
                Ok(())
            }
            MoveDir::Next => {
                let step = self.path.0.pop().ok_or(MoveError::Boundary)?;
                self.index = step.right_index();
                Ok(())
            }
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
    pub fn move_up(&mut self) -> Result<(), MoveError> {
        match self {
            Handle::Point(point) => {
                let mut steps = point.path.0.drain(..).collect::<Vec<_>>();
                let s = steps.pop().ok_or(MoveError::Boundary)?;
                *self = Handle::Span(SpanHandle {
                    path: Path(steps),
                    left: s.left_index(),
                    right: s.right_index(),
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
        todo!()
    }

    pub fn drag<L: Debug + Clone>(self, e: &Expr<L>, target: &Point) -> Option<Handle> {
        match self {
            Handle::Point(source) => {
                if target.path == source.path {
                    if target.index.is_left_of_index(source.index) {
                        Some(Handle::Span(SpanHandle {
                            path: source.path,
                            left: target.index,
                            right: source.index,
                            focus: SpanFocus::Left,
                        }))
                    } else if target.index == source.index {
                        Some(Handle::Point(source))
                    } else if source.index.is_left_of_index(target.index) {
                        Some(Handle::Span(SpanHandle {
                            path: source.path,
                            left: source.index,
                            right: target.index,
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
                        && p_ol.index.is_left_of_step(s)
                    {
                        let path_o = p_ol.path.clone();
                        let i_or = s.right_index();
                        let i_ol = p_ol.index;
                        let i_il = p_il.index;
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
                        && s.is_left_of_index(p_or.index)
                    {
                        let path_o = p_or.path.clone();
                        let i_ol = s.left_index();
                        let i_or = p_or.index;
                        let i_il = e.at_path(&p_ir.path).kids.leftmost_index();
                        let i_ir = p_ir.index;
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
                        && p_ol.index.is_left_of_step(s)
                    {
                        let path_o = p_ol.path.clone();
                        let i_ol = p_ol.index;
                        let i_or = s.right_index();
                        let i_il = p_il.index;
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
                        && s.is_left_of_index(p_or.index)
                    {
                        let path_o = p_or.path.clone();
                        let i_ol = s.left_index();
                        let i_or = p_or.index;
                        let i_il = e.at_path(&p_ir.path).kids.leftmost_index();
                        let i_ir = p_ir.index;
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
                                    index: source.path_m.0.first().unwrap().left_index(),
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
                                    index: source.path_m.0.first().unwrap().right_index(),
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

    pub fn contains_point(&self, point: &Point) -> bool {
        todo!()
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        todo!()
    }
}

// -----------------------------------------------------------------------------
// SpanHandle
// -----------------------------------------------------------------------------

/// A handle for a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle {
    pub path: Path,
    pub left: Index,
    pub right: Index,
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
        todo!()
    }

    /// Right point of [Span].
    pub fn p_r(&self) -> Point {
        todo!()
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
        todo!()
    }

    pub fn p_il(&self) -> Point {
        todo!()
    }

    pub fn p_ir(&self) -> Point {
        todo!()
    }

    pub fn p_ol(&self) -> Point {
        todo!()
    }

    pub fn p_or(&self) -> Point {
        todo!()
    }

    pub fn focus_point(&self) -> Point {
        match self.focus {
            ZipperFocus::OuterLeft => self.p_ol(),
            ZipperFocus::InnerLeft => self.p_il(),
            ZipperFocus::InnerRight => self.p_ir(),
            ZipperFocus::OuterRight => self.p_or(),
        }
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

    pub fn at_handle_cloned(&self, handle: &Handle) -> Fragment<L> {
        todo!()
    }

    pub fn insert_fragment_at_handle(&mut self, handle: Handle, frag: Fragment<L>) -> Handle {
        todo!()
    }

    pub fn at_handle(&self, handle: &Handle) -> Fragment<L> {
        todo!()
    }

    /// Replaces the span at the handle with the new span and returns the old
    /// span.
    pub fn splice_at_span(&mut self, handle: &SpanHandle, new_span: Span<L>) -> Span<L> {
        todo!()
    }

    /// Replaces the zipper at the handle with the new zipper and returns the
    /// old zipper.
    pub fn splice_at_zipper(&mut self, handle: &ZipperHandle, new_zipper: Zipper<L>) -> Zipper<L> {
        todo!()
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
                left: left,
                right: right,
            },
            middle,
        )
    }

    pub fn example(mk_label: impl FnMut() -> L, width: i32, height: i32) -> Self {
        todo!()
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
    pub fn empty() -> Self {
        Span(vec![])
    }

    pub fn assert_step_in_bounds(&self, s: Step) {
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
        todo!()
    }

    pub fn leftmost_index(&self) -> Index {
        todo!()
    }
}

// -----------------------------------------------------------------------------
// Zipper
// -----------------------------------------------------------------------------

/// A zipper between two [SpanHandle]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper<L> {
    pub outer_left: Span<L>,
    pub outer_right: Span<L>,
    pub middle: Context<L>,
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
}

/// A tooth of an [Expr] around a [Step].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub label: L,
    pub left: Span<L>,
    pub right: Span<L>,
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
