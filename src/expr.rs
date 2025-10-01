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
    fn is_left_of_index(&self, index: Index) -> bool {
        todo!()
    }

    fn is_left_of_step(&self, s: &Step) -> bool {
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

    fn is_left_of_index(&self, index: Index) -> bool {
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

    /// If `self` is an ancestor of `other`, then return the path `suffix` such
    /// that `path.append(suffix) == other`
    pub fn diff(&self, other: &Self) -> Option<Path> {
        let suffix = self.0.as_slice().strip_suffix(other.0.as_slice())?;
        Some(Path(suffix.to_vec()))
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

impl Point {}

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

impl Handle {
    pub fn drag<L: Debug + Clone>(self, target: &Point, e: &Expr<L>) -> Option<Handle> {
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
                        let i_ir = e.at_path(&p_il.path).rightmost_index();
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
                        let i_il = e.at_path(&p_ir.path).leftmost_index();
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
                        let i_ir = e.at_path(&p_il.path).rightmost_index();
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
                        let i_il = e.at_path(&p_ir.path).leftmost_index();
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
                        todo!()
                    }
                }
            }
            Handle::Span(handle) => todo!(),
            Handle::Zipper(handle) => todo!(),
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
    pub left: Index,
    pub right: Index,
    pub focus: SpanFocus,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum SpanFocus {
    Left,
    Right,
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

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum ZipperFocus {
    OuterLeft,
    InnerLeft,
    InnerRight,
    OuterRight,
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

    fn at_step(&self, step: &Step) -> &Self {
        self.kids.at_step(step)
    }

    fn at_step_owned(self, s: &Step) -> (Tooth<L>, Expr<L>) {
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

    fn rightmost_index(&self) -> Index {
        todo!()
    }

    fn leftmost_index(&self) -> Index {
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
    pub inner_left: Span<L>,
    pub inner_right: Span<L>,
}

// -----------------------------------------------------------------------------
// ExprContext
// -----------------------------------------------------------------------------

/// A context around an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Context<L>(pub Vec<Tooth<L>>);

impl<L: Debug + Clone> Context<L> {
    fn empty() -> Self {
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
