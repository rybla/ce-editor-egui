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

impl Default for Handle {
    fn default() -> Self {
        Self::Point(Point::default())
    }
}

/// A point between two [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Point(pub Path, pub Index);

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
}

/// A step from an [Expr] to one of its kids.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(pub usize);

impl Step {
    pub fn left_index(&self) -> Index {
        Index(self.0)
    }

    pub fn right_index(&self) -> Index {
        Index(self.0 + 1)
    }
}

/// An index between kids, or before the first kid, or after the last kid of an
/// [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Index(pub usize);

/// A handle for a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle {
    pub path: Path,
    pub left: Index,
    pub right: Index,
}

impl SpanHandle {
    pub fn left_point(&self) -> Point {
        Point(self.path.clone(), self.left.clone())
    }

    pub fn right_point(&self) -> Point {
        Point(self.path.clone(), self.right.clone())
    }
}

/// A focus of a [SpanHandle].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum SpanFocus {
    Left,
    Right,
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
        Point(self.outer_path.clone(), self.outer_left.clone())
    }

    pub fn outer_right_point(&self) -> Point {
        Point(self.outer_path.clone(), self.outer_right.clone())
    }

    pub fn inner_left_point(&self) -> Point {
        Point(self.inner_path(), self.inner_left.clone())
    }

    pub fn inner_right_point(&self) -> Point {
        Point(self.inner_path(), self.inner_right.clone())
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

    pub fn at_step<'a>(&'a self, step: &Step) -> &'a Expr<L> {
        self.kids.at_step(step)
    }

    pub fn at_path<'a>(&'a self, path: &Path) -> &'a Expr<L> {
        path.0.iter().fold(self, |e, step| e.at_step(step))
    }

    pub fn kids_and_steps<'a>(
        &'a self,
    ) -> std::iter::Map<
        std::iter::Enumerate<std::slice::Iter<'a, Expr<L>>>,
        impl FnMut((usize, &'a Expr<L>)) -> (Step, bool, &'a Expr<L>),
    > {
        self.kids
            .0
            .iter()
            .enumerate()
            .map(|(i, kid)| (Step(i), i != 0 && i == self.kids.0.len() - 1, kid))
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
