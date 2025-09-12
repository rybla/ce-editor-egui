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
    pub fn move_up<L: Debug>(&mut self, expr: &Expr<L>) {
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
                }
            }
            Handle::Span(handle) => {
                let kid = expr.at_path(&handle.span_handle.path);
                let (leftmost, rightmost) = kid.kids.extreme_indexes();
                if handle.span_handle.left == leftmost && handle.span_handle.right == rightmost {
                    if let Some(step) = handle.span_handle.path.pop() {
                        handle.span_handle.left = step.left_index();
                        handle.span_handle.right = step.right_index();
                    }
                } else {
                    handle.span_handle.left = leftmost;
                    handle.span_handle.right = rightmost;
                }
            }
            // NOTE: I'm not sure exactly what should happen when you move up at
            // a zipper, but by default for now, nothing happens.
            Handle::Zipper(_) => (),
        }
    }

    pub fn escape(&mut self) {
        match self {
            Handle::Point(_point) => (),
            Handle::Span(handle) => *self = Handle::Point(handle.focus_point()),
            Handle::Zipper(handle) => *self = Handle::Point(handle.focus_point()),
        }
    }

    /// Returns a boolean indicating if the move hit a boundary.
    pub fn move_prev<L: Debug>(&mut self, expr: &Expr<L>) -> bool {
        match self {
            Handle::Point(handle) => handle.move_prev(expr),
            // A little bit weirdly, my intuition indicates that in a span, when
            // you press left or right arrow, the cursor should jump to the left
            // or right end of the span and escape the span. But for a zipper, I
            // don't have that intuition. Instead, I think it should just jump
            // to the focus of the handle, regardless of whether the left or
            // right arrow was pressed. I am willing to be convinced otherwise
            // though.
            Handle::Span(handle) => {
                *self = Handle::Point(handle.span_handle.left_point());
                true
            }
            Handle::Zipper(handle) => {
                *self = Handle::Point(handle.focus_point());
                true
            }
        }
    }

    /// Returns a boolean indicating if the move hit a boundary.
    pub fn move_next<L: Debug>(&mut self, expr: &Expr<L>) -> bool {
        match self {
            Handle::Point(handle) => handle.move_next(expr),
            // Same note as in [move_prev]
            Handle::Span(handle) => {
                *self = Handle::Point(handle.span_handle.right_point());
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
            Handle::Zipper(_handle) => todo!(),
        }
    }

    pub(crate) fn contains_point(&self, point: &Point) -> bool {
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
            Handle::Zipper(_handle) => todo!(),
        }
    }

    pub fn rotate_focus_prev(&mut self) {
        match self {
            Handle::Point(_handle) => (),
            Handle::Span(handle) => handle.focus = handle.focus.rotate_prev(),
            Handle::Zipper(handle) => handle.focus = handle.focus.rotate_prev(),
        }
    }

    pub fn rotate_focus_next(&mut self) {
        match self {
            Handle::Point(_handle) => (),
            Handle::Span(handle) => handle.focus = handle.focus.rotate_next(),
            Handle::Zipper(handle) => handle.focus = handle.focus.rotate_next(),
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

    pub fn select_prev<L: Debug>(&mut self, expr: &Expr<L>) {
        let origin = self.origin_point();
        let mut target = self.focus_point();
        target.move_prev(expr);
        *self = origin.select_to(&target, expr);
    }

    pub fn select_next<L: Debug>(&mut self, expr: &Expr<L>) {
        let origin = self.origin_point();
        let mut target = self.focus_point();
        target.move_next(expr);
        *self = origin.select_to(&target, expr);
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
    pub fn move_prev<L: Debug>(&mut self, expr: &Expr<L>) -> bool {
        let subexpr = expr.at_path(&self.path);
        let (leftmost, _rightmost) = subexpr.kids.extreme_indexes();
        if self.index == leftmost {
            if let Some(step) = self.path.pop() {
                self.index = step.left_index();
                return true;
            }
            false
        } else {
            let step = self.index.left_step();
            let kid = subexpr.kids.at_step(&step);
            let (_kid_leftmost, kid_rightmost) = kid.kids.extreme_indexes();
            self.path.push(step);
            self.index = kid_rightmost;
            true
        }
    }

    /// Return a boolean indicating if the move hit a boundary.
    pub fn move_next<L: Debug>(&mut self, expr: &Expr<L>) -> bool {
        let subexpr = expr.at_path(&self.path);
        let (_leftmost, rightmost) = subexpr.kids.extreme_indexes();
        if self.index == rightmost {
            if let Some(step) = self.path.pop() {
                self.index = step.right_index();
                return true;
            }
            false
        } else {
            let step = self.index.right_step();
            let kid = subexpr.kids.at_step(&step);
            let (kid_leftmost, _kid_rightmost) = kid.kids.extreme_indexes();
            self.path.push(step);
            self.index = kid_leftmost;
            true
        }
    }

    /// Calculates the selection from self to target.
    pub fn select_to<L: Debug>(&self, target: &Point, expr: &Expr<L>) -> Handle {
        if let Some(target_suffix) = target.path.strip_prefix(&self.path) {
            // self.path is a prefix of target.path

            let mut target_suffix = target_suffix.into_iter();
            if let Some(step) = target_suffix.next() {
                // target.path has some more steps than self.path
                let kid = expr.at_path(&target.path);

                if self.index.is_right_of_step(step) {
                    Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: self.path.clone(),
                            outer_left: self.index.clone(),
                            outer_right: step.right_index(),
                            middle_path: Path(target_suffix.cloned().collect()).clone(),
                            inner_left: target.index.clone(),
                            inner_right: kid.kids.extreme_indexes().1,
                        },
                        focus: ZipperFocus::InnerLeft,
                        origin: ZipperFocus::OuterLeft,
                    })
                } else {
                    todo!()
                }
            } else {
                // self.path == target.path

                if self.index.is_right_of_index(&target.index) {
                    Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: self.path.clone(),
                            left: target.index.clone(),
                            right: self.index.clone(),
                        },
                        focus: SpanFocus::Left,
                    })
                } else {
                    Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: self.path.clone(),
                            left: self.index.clone(),
                            right: target.index.clone(),
                        },
                        focus: SpanFocus::Right,
                    })
                }
            }
        } else {
            todo!()
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
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(pub usize);

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
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Index(pub usize);

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
    pub fn rotate_prev(&self) -> Self {
        match self {
            ZipperFocus::OuterLeft => ZipperFocus::OuterRight,
            ZipperFocus::InnerLeft => ZipperFocus::OuterLeft,
            ZipperFocus::InnerRight => ZipperFocus::InnerLeft,
            ZipperFocus::OuterRight => ZipperFocus::InnerRight,
        }
    }

    pub fn rotate_next(&self) -> Self {
        match self {
            ZipperFocus::OuterLeft => ZipperFocus::InnerLeft,
            ZipperFocus::InnerLeft => ZipperFocus::InnerRight,
            ZipperFocus::InnerRight => ZipperFocus::OuterRight,
            ZipperFocus::OuterRight => ZipperFocus::OuterLeft,
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
