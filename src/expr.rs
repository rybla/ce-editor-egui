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

pub type SpanHandleAndFocus = (SpanHandle, SpanFocus);

pub type ZipperHandleAndFocus = (ZipperHandle, ZipperFocus);

impl Handle {
    pub fn move_up<L: Debug>(&mut self, expr: &Expr<L>) {
        match self {
            Handle::Point(Point { path, index: _ }) => {
                if let Some(step) = path.pop() {
                    *self = Handle::Span((
                        SpanHandle {
                            path: path.clone(),
                            left: step.left_index(),
                            right: step.right_index(),
                        },
                        SpanFocus::Left,
                    ));
                }
            }
            Handle::Span((SpanHandle { path, left, right }, _focus)) => {
                let kid = expr.at_path(path);
                let (leftmost, rightmost) = kid.kids.extreme_indexes();
                if *left == leftmost && *right == rightmost {
                    if let Some(step) = path.pop() {
                        *left = step.left_index();
                        *right = step.right_index();
                    }
                } else {
                    *left = leftmost;
                    *right = rightmost;
                }
            }
            Handle::Zipper(_) => todo!("move_up zipper"),
        }
    }

    pub fn focus_point<'a>(&'a self) -> Point {
        match self {
            Handle::Point(point) => point.clone(),
            Handle::Span((span, focus)) => span.focus_point(focus),
            Handle::Zipper((zipper, focus)) => zipper.focus_point(focus),
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        match self {
            Handle::Point(_point) => false,
            Handle::Span((handle, _focus)) => path
                .strip_prefix(&handle.path)
                .and_then(|steps| {
                    steps.first().and_then(|step| {
                        Some(
                            step.is_right_of_index(&handle.left)
                                && step.is_left_of_index(&handle.right),
                        )
                    })
                })
                .unwrap_or(false),
            Handle::Zipper((_handle, _focus)) => todo!(),
        }
    }

    pub(crate) fn contains_point(&self, point: &Point) -> bool {
        match self {
            Handle::Point(handle) => handle == point,
            Handle::Span((handle, _focus)) => point
                .path
                .strip_prefix(&handle.path)
                .and_then(|steps| match steps.first() {
                    Some(step) => Some(
                        step.is_right_of_index(&handle.left)
                            && step.is_left_of_index(&handle.right),
                    ),
                    None => Some(
                        point.index.is_right_of_index(&handle.left)
                            && point.index.is_left_of_index(&handle.right),
                    ),
                })
                .unwrap_or(false),
            Handle::Zipper((_handle, _focus)) => todo!(),
        }
    }

    pub fn rotate_focus_prev(&mut self) {
        match self {
            Handle::Point(_handle) => (),
            Handle::Span((_handle, focus)) => *focus = focus.rotate_prev(),
            Handle::Zipper((_handle, focus)) => *focus = focus.rotate_prev(),
        }
    }

    pub fn rotate_focus_next(&mut self) {
        match self {
            Handle::Point(_handle) => (),
            Handle::Span((_handle, focus)) => *focus = focus.rotate_next(),
            Handle::Zipper((_handle, focus)) => *focus = focus.rotate_next(),
        }
    }

    pub fn norm(&mut self) {
        todo!(
            "Normalize as a handle by collapsing empty SpanHandles and empty sections of ZipperHandle"
        )
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
    pub fn move_prev<L: Debug>(&mut self, expr: &Expr<L>) {
        let subexpr = expr.at_path(&self.path);
        let (leftmost, _rightmost) = subexpr.kids.extreme_indexes();
        if self.index == leftmost {
            if let Some(step) = self.path.pop() {
                self.index = step.left_index();
            }
        } else {
            let step = self.index.left_step();
            let kid = subexpr.kids.at_step(&step);
            let (_kid_leftmost, kid_rightmost) = kid.kids.extreme_indexes();
            self.path.push(step);
            self.index = kid_rightmost;
        }
    }

    pub fn move_next<L: Debug>(&mut self, expr: &Expr<L>) {
        let subexpr = expr.at_path(&self.path);
        let (_leftmost, rightmost) = subexpr.kids.extreme_indexes();
        if self.index == rightmost {
            if let Some(step) = self.path.pop() {
                self.index = step.right_index();
            }
        } else {
            let step = self.index.right_step();
            let kid = subexpr.kids.at_step(&step);
            let (kid_leftmost, _kid_rightmost) = kid.kids.extreme_indexes();
            self.path.push(step);
            self.index = kid_leftmost;
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
