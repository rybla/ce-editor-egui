#![allow(dead_code)]

use std::fmt::Debug;

// -----------------------------------------------------------------------------
// Handle: Point, SpanHandle, ZipperHandle
// -----------------------------------------------------------------------------

/// A handle for a [Fragment].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Handle {
    Point(Point),
    Span(SpanHandleAndFocus),
    Zipper(ZipperHandleAndFocus),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandleAndFocus {
    pub span_handle: SpanHandle,
    pub focus: SpanFocus,
}

impl SpanHandleAndFocus {
    pub fn focus_point(&self) -> Point {
        self.span_handle.focus_point(&self.focus)
    }

    fn origin_point(&self) -> Point {
        self.span_handle.origin_point(&self.focus)
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandleAndFocus {
    pub zipper_handle: ZipperHandle,
    pub focus: ZipperFocus,
    // TODO: Actually, this needs to be removed once I transition to the proper
    // implementation of select_to Where three points are fixed Other than just
    // the two origin and focus.
    pub origin: ZipperFocus,
}

impl ZipperHandleAndFocus {
    pub fn focus_point(&self) -> Point {
        self.zipper_handle.focus_point(&self.focus)
    }

    fn origin_point(&self) -> Point {
        self.zipper_handle.focus_point(&self.origin)
    }
}

impl Handle {
    pub fn move_up<L: Debug>(&mut self, expr: &Expr<L>) -> bool {
        match self {
            Handle::Point(Point { path, index: _ }) => {
                if let Some(step) = path.pop() {
                    *self = Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: path.clone(),
                            left: step.left_index(),
                            right: step.right_index(),
                        },
                        focus: SpanFocus::Left,
                    });
                    true
                } else {
                    false
                }
            }
            Handle::Span(handle) => {
                let kid = expr.at_path(&handle.span_handle.path);
                let (leftmost, rightmost) = kid.kids.extreme_indexes();
                if handle.span_handle.left == leftmost && handle.span_handle.right == rightmost {
                    if let Some(step) = handle.span_handle.path.pop() {
                        handle.span_handle.left = step.left_index();
                        handle.span_handle.right = step.right_index();
                        true
                    } else {
                        false
                    }
                } else {
                    handle.span_handle.left = leftmost;
                    handle.span_handle.right = rightmost;
                    true
                }
            }
            // NOTE: I'm not sure exactly what should happen when you move up at
            // a zipper, but by default for now, nothing happens.
            Handle::Zipper(_) => false,
        }
    }

    pub fn escape(&mut self) {
        match self {
            Handle::Point(_point) => (),
            Handle::Span(handle) => *self = Handle::Point(handle.focus_point()),
            Handle::Zipper(handle) => *self = Handle::Point(handle.focus_point()),
        }
    }

    /// Returns a Boolean indicating whether the move hit a boundary.
    pub fn move_dir<L: Debug>(&mut self, dir: MoveDir, expr: &Expr<L>) -> bool {
        match self {
            Handle::Point(handle) => handle.move_dir(dir, expr),
            // A little bit weirdly, my intuition indicates that in a span, when
            // you press left or right arrow, the cursor should jump to the left
            // or right end of the span and escape the span. But for a zipper, I
            // don't have that intuition. Instead, I think it should just jump
            // to the focus of the handle, regardless of whether the left or
            // right arrow was pressed. I am willing to be convinced otherwise
            // though.
            Handle::Span(handle) => {
                match dir {
                    MoveDir::Prev => *self = Handle::Point(handle.span_handle.left_point()),
                    MoveDir::Next => *self = Handle::Point(handle.span_handle.right_point()),
                }
                true
            }
            Handle::Zipper(handle) => {
                *self = Handle::Point(handle.focus_point());
                true
            }
        }
    }

    pub fn focus_point<'a>(&'a self) -> Point {
        match self {
            Handle::Point(point) => point.clone(),
            Handle::Span(handle) => handle.focus_point(),
            Handle::Zipper(handle) => handle.focus_point(),
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        match self {
            Handle::Point(_point) => false,
            Handle::Span(handle) => path
                .strip_prefix(&handle.span_handle.path)
                .and_then(|steps| {
                    steps.first().and_then(|step| {
                        Some(
                            step.is_right_of_index(&handle.span_handle.left)
                                && step.is_left_of_index(&handle.span_handle.right),
                        )
                    })
                })
                .unwrap_or(false),
            Handle::Zipper(handle) => match path.strip_prefix(&handle.zipper_handle.inner_path()) {
                Some(path_suffix) => path_suffix
                    .first()
                    .and_then(|step| {
                        Some(
                            step.is_right_of_index(&handle.zipper_handle.inner_left)
                                && step.is_left_of_index(&handle.zipper_handle.inner_right),
                        )
                    })
                    .unwrap_or(false),
                None => path
                    .strip_prefix(&handle.zipper_handle.outer_path)
                    .and_then(|steps| {
                        steps.first().and_then(|step| {
                            Some(
                                step.is_right_of_index(&handle.zipper_handle.outer_left)
                                    && step.is_left_of_index(&handle.zipper_handle.outer_right),
                            )
                        })
                    })
                    .unwrap_or(false),
            },
        }
    }

    pub fn contains_point(&self, point: &Point) -> bool {
        match self {
            Handle::Point(handle) => handle == point,
            Handle::Span(handle) => point
                .path
                .strip_prefix(&handle.span_handle.path)
                .and_then(|steps| match steps.first() {
                    Some(step) => Some(
                        step.is_right_of_index(&handle.span_handle.left)
                            && step.is_left_of_index(&handle.span_handle.right),
                    ),
                    None => Some(
                        point.index.is_right_of_index(&handle.span_handle.left)
                            && point.index.is_left_of_index(&handle.span_handle.right),
                    ),
                })
                .unwrap_or(false),
            Handle::Zipper(handle) => {
                match point.path.strip_prefix(&handle.zipper_handle.inner_path()) {
                    Some(path_suffix) => path_suffix
                        .first()
                        .and_then(|step| {
                            Some(
                                step.is_right_of_index(&handle.zipper_handle.inner_left)
                                    && step.is_left_of_index(&handle.zipper_handle.inner_right),
                            )
                        })
                        .unwrap_or_else(|| {
                            point
                                .index
                                .is_right_of_index(&handle.zipper_handle.inner_left)
                                && point
                                    .index
                                    .is_left_of_index(&handle.zipper_handle.inner_right)
                        }),
                    None => point
                        .path
                        .strip_prefix(&handle.zipper_handle.outer_path)
                        .and_then(|steps| {
                            steps.first().and_then(|step| {
                                Some(
                                    step.is_right_of_index(&handle.zipper_handle.outer_left)
                                        && step.is_left_of_index(&handle.zipper_handle.outer_right),
                                )
                            })
                        })
                        .unwrap_or(false),
                }
            }
        }
    }

    pub fn rotate_focus_dir(&mut self, dir: MoveDir) {
        match self {
            Handle::Point(_handle) => (),
            Handle::Span(handle) => handle.focus = handle.focus.rotate_dir(dir),
            Handle::Zipper(handle) => handle.focus = handle.focus.rotate_dir(dir),
        }
    }

    pub fn norm(&mut self) {
        todo!(
            "Normalize as a handle by collapsing empty SpanHandles and empty sections of ZipperHandle"
        )
    }

    pub fn origin_point(&self) -> Point {
        match self {
            Handle::Point(handle) => handle.clone(),
            Handle::Span(handle) => handle.origin_point(),
            Handle::Zipper(handle) => handle.origin_point(),
        }
    }

    pub fn move_select_dir<L: Debug>(&mut self, dir: MoveDir, expr: &Expr<L>) -> bool {
        let origin = self.origin_point();
        let mut target = self.focus_point();
        let moved = target.move_dir(dir, expr);
        if moved {
            match origin.select_to(&target, expr) {
                Some(handle) => {
                    *self = handle;
                    true
                }
                None => false,
            }
        } else {
            false
        }
    }
}

impl Default for Handle {
    fn default() -> Self {
        Self::Point(Point::default())
    }
}

/// A point between two [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Point {
    pub path: Path,
    pub index: Index,
}

impl Point {
    /// Return a boolean indicating if the move hit a boundary.
    pub fn move_dir<L: Debug>(&mut self, dir: MoveDir, expr: &Expr<L>) -> bool {
        let subexpr = expr.at_path(&self.path);
        let (leftmost, rightmost) = subexpr.kids.extreme_indexes();
        let is_at_local_boundary = match dir {
            MoveDir::Prev => self.index == leftmost,
            MoveDir::Next => self.index == rightmost,
        };
        if is_at_local_boundary {
            if let Some(step) = self.path.pop() {
                self.index = match dir {
                    MoveDir::Prev => step.left_index(),
                    MoveDir::Next => step.right_index(),
                };
                return true;
            }
            false
        } else {
            let step = match dir {
                MoveDir::Prev => self.index.left_step(),
                MoveDir::Next => self.index.right_step(),
            };
            let kid = subexpr.kids.at_step(&step);
            let kid_boundary = {
                let (kid_leftmost, kid_rightmost) = kid.kids.extreme_indexes();
                match dir {
                    MoveDir::Prev => kid_rightmost,
                    MoveDir::Next => kid_leftmost,
                }
            };
            self.path.push(step);
            self.index = kid_boundary;
            true
        }
    }

    pub fn select_from_outer_to_inner<L: Debug>(
        outer: &Point,
        inner: &Point,
        inner_suffix: &[Step],
        expr: &Expr<L>,
    ) -> Option<Handle> {
        // outer.path is a prefix of inner.path
        println!("outer.path is a prefix of inner.path");
        println!("inner_suffix = {:#?}", Path(inner_suffix.to_vec()));
        if let Some(step) = inner_suffix.first() {
            // inner.path has an extra step beyond outer.path
            let kid = expr.at_path(&inner.path);

            if outer.index.is_right_of_step(step) {
                Some(Handle::Zipper(ZipperHandleAndFocus {
                    zipper_handle: ZipperHandle {
                        outer_path: outer.path.clone(),
                        outer_left: outer.index.clone(),
                        outer_right: step.right_index(),
                        middle_path: Path(inner_suffix.to_vec()),
                        inner_left: kid.kids.extreme_indexes().0,
                        inner_right: inner.index.clone(),
                    },
                    focus: ZipperFocus::InnerRight,
                    origin: ZipperFocus::OuterRight,
                }))
            } else {
                Some(Handle::Zipper(ZipperHandleAndFocus {
                    zipper_handle: ZipperHandle {
                        outer_path: outer.path.clone(),
                        outer_left: outer.index.clone(),
                        outer_right: step.right_index(),
                        middle_path: Path(inner_suffix.to_vec()),
                        inner_left: inner.index.clone(),
                        inner_right: kid.kids.extreme_indexes().1,
                    },
                    focus: ZipperFocus::InnerLeft,
                    origin: ZipperFocus::OuterLeft,
                }))
            }
        } else {
            // outer.path == inner.path

            if outer.index.is_right_of_index(&inner.index) {
                Some(Handle::Span(SpanHandleAndFocus {
                    span_handle: SpanHandle {
                        path: outer.path.clone(),
                        left: inner.index.clone(),
                        right: outer.index.clone(),
                    },
                    focus: SpanFocus::Left,
                }))
            } else {
                Some(Handle::Span(SpanHandleAndFocus {
                    span_handle: SpanHandle {
                        path: outer.path.clone(),
                        left: outer.index.clone(),
                        right: inner.index.clone(),
                    },
                    focus: SpanFocus::Right,
                }))
            }
        }
    }

    /// Calculates the selection from self to target.
    pub fn select_to<L: Debug>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
        println!("select_to");
        println!("self   = {:#?}", self);
        println!("target = {:#?}", target);

        if let Some(target_suffix) = target.path.strip_prefix(&self.path) {
            // self.path is a prefix of target.path
            Self::select_from_outer_to_inner(self, target, target_suffix, expr)
        } else if let Some(self_suffix) = self.path.strip_prefix(&self.path) {
            // target.path is a prefix of self.path
            Self::select_from_outer_to_inner(target, self, self_suffix, expr)
        } else {
            // NOTE: Perhaps want to calculate the smallest span that contains both self and target.
            None
        }
    }
}

/// A path from the top [Expr] to an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Path(pub Vec<Step>);

impl Path {
    pub fn pop(&mut self) -> Option<Step> {
        self.0.pop()
    }

    pub fn push(&mut self, step: Step) {
        self.0.push(step);
    }

    pub fn starts_with(&self, path: &Path) -> bool {
        self.0.starts_with(&path.0)
    }

    pub fn strip_prefix(&self, path: &Path) -> Option<&[Step]> {
        self.0.strip_prefix(path.0.as_slice())
    }
}

/// A step from an [Expr] to one of its kids.
#[derive(Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(pub usize);

impl Debug for Step {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Step {
    pub fn left_step(&self) -> Step {
        Step(self.0 - 1)
    }

    pub fn right_step(&self) -> Step {
        Step(self.0 + 1)
    }

    pub fn left_index(&self) -> Index {
        Index(self.0)
    }

    pub fn right_index(&self) -> Index {
        Index(self.0 + 1)
    }

    pub fn is_left_of_step(&self, step: &Step) -> bool {
        self.0 < step.0
    }

    pub fn is_right_of_step(&self, step: &Step) -> bool {
        self.0 > step.0
    }

    pub fn is_left_of_index(&self, index: &Index) -> bool {
        self.0 < index.0
    }

    pub fn is_right_of_index(&self, index: &Index) -> bool {
        self.0 + 1 > index.0
    }
}

/// An index between kids, or left the first kid, or right of the last kid of an
/// [Expr].
#[derive(Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Index(pub usize);

impl Debug for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Index {
    pub fn left_index(&self) -> Index {
        Index(self.0 - 1)
    }

    pub fn right_index(&self) -> Index {
        Index(self.0 + 1)
    }

    pub fn left_step(&self) -> Step {
        Step(self.0 - 1)
    }

    pub fn right_step(&self) -> Step {
        Step(self.0)
    }

    pub fn is_left_of_index(&self, index: &Index) -> bool {
        self.0 < index.0
    }

    pub fn is_right_of_index(&self, index: &Index) -> bool {
        self.0 > index.0
    }

    fn is_right_of_step(&self, step: &Step) -> bool {
        step.is_left_of_index(self)
    }

    fn is_left_of_step(&self, step: &Step) -> bool {
        step.is_right_of_index(self)
    }
}

/// A handle for a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle {
    pub path: Path,
    pub left: Index,
    pub right: Index,
}

impl SpanHandle {
    pub fn left_point(&self) -> Point {
        Point {
            path: self.path.clone(),
            index: self.left.clone(),
        }
    }

    pub fn right_point<'a>(&'a self) -> Point {
        Point {
            path: self.path.clone(),
            index: self.right.clone(),
        }
    }

    pub fn focus_point(&self, focus: &SpanFocus) -> Point {
        match focus {
            SpanFocus::Left => self.left_point(),
            SpanFocus::Right => self.right_point(),
        }
    }

    fn origin_point(&self, focus: &SpanFocus) -> Point {
        match focus {
            SpanFocus::Left => self.right_point(),
            SpanFocus::Right => self.left_point(),
        }
    }
}

/// A focus of a [SpanHandle].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum SpanFocus {
    Left,
    Right,
}

impl SpanFocus {
    pub fn rotate_prev(&self) -> Self {
        match self {
            SpanFocus::Left => SpanFocus::Right,
            SpanFocus::Right => SpanFocus::Left,
        }
    }

    pub fn rotate_next(&self) -> Self {
        match self {
            SpanFocus::Left => SpanFocus::Right,
            SpanFocus::Right => SpanFocus::Left,
        }
    }

    fn rotate_dir(&self, dir: MoveDir) -> SpanFocus {
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

/// A hanle for a [Zipper].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandle {
    pub outer_path: Path,
    pub outer_left: Index,
    pub outer_right: Index,
    pub middle_path: Path,
    pub inner_left: Index,
    pub inner_right: Index,
}

impl ZipperHandle {
    pub fn inner_path(&self) -> Path {
        Path([self.outer_path.0.clone(), self.middle_path.0.clone()].concat())
    }

    pub fn outer_left_point(&self) -> Point {
        Point {
            path: self.outer_path.clone(),
            index: self.outer_left.clone(),
        }
    }

    pub fn outer_right_point(&self) -> Point {
        Point {
            path: self.outer_path.clone(),
            index: self.outer_right.clone(),
        }
    }

    pub fn inner_left_point(&self) -> Point {
        Point {
            path: self.inner_path(),
            index: self.inner_left.clone(),
        }
    }

    pub fn inner_right_point(&self) -> Point {
        Point {
            path: self.inner_path(),
            index: self.inner_right.clone(),
        }
    }

    fn focus_point(&self, focus: &ZipperFocus) -> Point {
        match focus {
            ZipperFocus::OuterLeft => self.outer_left_point(),
            ZipperFocus::InnerLeft => self.inner_left_point(),
            ZipperFocus::InnerRight => self.inner_right_point(),
            ZipperFocus::OuterRight => self.outer_right_point(),
        }
    }
}

/// A focus fro a [ZipperHandle].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum ZipperFocus {
    OuterLeft,
    InnerLeft,
    OuterRight,
    InnerRight,
}

impl ZipperFocus {
    fn rotate_dir(&self, dir: MoveDir) -> ZipperFocus {
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
// Fragment: Expr, Span, Zipper
// -----------------------------------------------------------------------------

/// A fragment of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Fragment<L> {
    Span(Span<L>),
    Zipper(Expr<L>),
}

/// An expression of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Expr<L> {
    pub label: L,
    pub kids: Span<L>,
}

impl<L: Debug> Expr<L> {
    pub fn height(&self) -> u32 {
        self.kids
            .0
            .iter()
            .fold(0, |h, e| std::cmp::max(h, 1 + e.height()))
    }

    pub fn at_path<'a>(&'a self, path: &Path) -> &'a Expr<L> {
        path.0.iter().fold(self, |e, step| e.kids.at_step(step))
    }

    pub fn kids_and_steps<'a>(
        &'a self,
    ) -> std::iter::Map<
        std::iter::Enumerate<std::slice::Iter<'a, Expr<L>>>,
        impl FnMut((usize, &'a Expr<L>)) -> (Step, &'a Expr<L>),
    > {
        self.kids
            .0
            .iter()
            .enumerate()
            .map(|(i, kid)| (Step(i), kid))
    }

    pub fn example<F>(mk_label: &mut F, width: usize, depth: usize) -> Self
    where
        F: FnMut() -> L,
    {
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
}

/// A span of [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span<L>(pub Vec<Expr<L>>);

impl<L: Debug> Span<L> {
    pub fn at_step<'a>(&'a self, step: &Step) -> &'a Expr<L> {
        self.0
            .get(step.0)
            .unwrap_or_else(|| panic!("step out of bounds: span = {self:?}; step = {step:?}"))
    }

    pub fn extreme_indexes(&self) -> (Index, Index) {
        (Index(0), Index(self.0.len()))
    }
}

/// A tooth of an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub left: Vec<Expr<L>>,
    pub right: Vec<Expr<L>>,
}

/// A context around an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ExprContext<L>(pub Vec<Tooth<L>>);

/// A context around a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanContext<L> {
    pub outer: ExprContext<L>,
    pub inner: Tooth<L>,
}

/// A zipper between two [SpanHandle]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper<L> {
    pub outer_left: Vec<Expr<L>>,
    pub outer_right: Vec<Expr<L>>,
    pub inner: SpanContext<L>,
}

// -----------------------------------------------------------------------------
// Miscellaneous
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum MoveDir {
    Prev,
    Next,
}
