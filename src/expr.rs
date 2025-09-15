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

    fn select_to<L: Debug>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
        println!("[select]");
        println!("self = {self:#?}");
        println!("target = {target:#?}");
        if let Some(target_suffix) = target
            .path
            .0
            .strip_prefix(self.span_handle.path.0.as_slice())
        {
            println!("[select] target is inside or beside self");
            if let Some(step) = target_suffix.first() {
                println!("[select] target.path is inside self.path");
                let target_kid = expr.at_path(&target.path);
                if step.is_left_of_index(&self.span_handle.left) {
                    println!("[select] target step is left of span's left");
                    Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: self.span_handle.path.clone(),
                            outer_left: step.left_index(),
                            outer_right: match self.focus {
                                SpanFocus::Left => self.span_handle.right.clone(),
                                SpanFocus::Right => self.span_handle.left.clone(),
                            },
                            middle_path: Path(target_suffix.to_vec()),
                            inner_left: target_kid.leftmost_index(),
                            inner_right: target.index.clone(),
                        },
                        focus: ZipperFocus::InnerRight,
                    }))
                } else if step.is_left_of_index(&self.span_handle.right) {
                    println!("[select] target step is inside span");
                    match self.focus {
                        SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.span_handle.path.clone(),
                                outer_left: step.left_index(),
                                outer_right: self.span_handle.right.clone(),
                                middle_path: Path(target_suffix.to_vec()),
                                inner_left: target.index.clone(),
                                inner_right: target_kid.rightmost_index(),
                            },
                            focus: ZipperFocus::InnerLeft,
                        })),
                        SpanFocus::Right => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.span_handle.path.clone(),
                                outer_left: self.span_handle.left.clone(),
                                outer_right: step.right_index(),
                                middle_path: Path(target_suffix.to_vec()),
                                inner_left: target_kid.leftmost_index(),
                                inner_right: target.index.clone(),
                            },
                            focus: ZipperFocus::InnerRight,
                        })),
                    }
                } else {
                    println!("[select] target step is right of span's right");
                    Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: self.span_handle.path.clone(),
                            outer_left: match self.focus {
                                SpanFocus::Left => self.span_handle.right.clone(),
                                SpanFocus::Right => self.span_handle.left.clone(),
                            },
                            outer_right: step.right_index(),
                            middle_path: Path(target_suffix.to_vec()),
                            inner_left: target.index.clone(),
                            inner_right: target_kid.rightmost_index(),
                        },
                        focus: ZipperFocus::InnerRight,
                    }))
                }
            } else {
                println!("[select] target is beside self");
                if target.index.is_left_of_index(&self.span_handle.left) {
                    println!("[select] target is beside self to the left");
                    Some(Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: self.span_handle.path.clone(),
                            left: target.index.clone(),
                            right: match self.focus {
                                SpanFocus::Left => self.span_handle.right.clone(),
                                SpanFocus::Right => self.span_handle.left.clone(),
                            },
                        },
                        focus: SpanFocus::Left,
                    }))
                } else if target.index.is_left_of_index(&self.span_handle.right) {
                    println!("[select] target is beside self to the middle");
                    match self.focus {
                        SpanFocus::Left => Some(Handle::Span(SpanHandleAndFocus {
                            span_handle: SpanHandle {
                                path: self.span_handle.path.clone(),
                                left: target.index.clone(),
                                right: self.span_handle.right.clone(),
                            },
                            focus: SpanFocus::Left,
                        })),
                        SpanFocus::Right => Some(Handle::Span(SpanHandleAndFocus {
                            span_handle: SpanHandle {
                                path: self.span_handle.path.clone(),
                                left: self.span_handle.left.clone(),
                                right: target.index.clone(),
                            },
                            focus: SpanFocus::Right,
                        })),
                    }
                } else {
                    println!("[select] target is beside self to the right");
                    Some(Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: self.span_handle.path.clone(),
                            left: match self.focus {
                                SpanFocus::Left => self.span_handle.right.clone(),
                                SpanFocus::Right => self.span_handle.left.clone(),
                            },
                            right: target.index.clone(),
                        },
                        focus: SpanFocus::Right,
                    }))
                }
            }
        } else if let Some(self_suffix) = self
            .span_handle
            .path
            .0
            .strip_prefix(target.path.0.as_slice())
        {
            println!("[select] self.path is strictly inside target.path");
            let step = self_suffix.first().unwrap_or_else(|| {
                panic!("impossible since then would have matched previous strip_suffix pattern")
            });
            if target.index.is_left_of_step(step) {
                println!("[select] self is inside target; target is to the left");
                let self_kid = expr.at_path(&self.span_handle.path);
                match self.focus {
                    SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: target.index.clone(),
                            outer_right: step.right_index(),
                            middle_path: Path(self_suffix.to_vec()),
                            inner_left: self_kid.leftmost_index(),
                            inner_right: self.span_handle.right.clone(),
                        },
                        focus: ZipperFocus::OuterLeft,
                    })),
                    SpanFocus::Right => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: step.left_index(),
                            outer_right: target.index.clone(),
                            middle_path: Path(self_suffix.to_vec()),
                            inner_left: self.span_handle.left.clone(),
                            inner_right: self_kid.rightmost_index(),
                        },
                        focus: ZipperFocus::OuterLeft,
                    })),
                }
            } else {
                println!("[select] self is inside target; target is to the right");
                let self_kid = expr.at_path(&self.span_handle.path);
                match self.focus {
                    SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: step.left_index(),
                            outer_right: target.index.clone(),
                            middle_path: Path(self_suffix.to_vec()),
                            inner_left: self_kid.leftmost_index(),
                            inner_right: self.span_handle.right.clone(),
                        },
                        focus: ZipperFocus::OuterRight,
                    })),
                    SpanFocus::Right => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: step.left_index(),
                            outer_right: target.index.clone(),
                            middle_path: Path(self_suffix.to_vec()),
                            inner_left: self.span_handle.left.clone(),
                            inner_right: self_kid.rightmost_index(),
                        },
                        focus: ZipperFocus::OuterRight,
                    })),
                }
            }
        } else {
            // NOTE: Perhaps want to calculate the smallest span that contains both self and target
            None
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandleAndFocus {
    pub zipper_handle: ZipperHandle,
    pub focus: ZipperFocus,
}

impl ZipperHandleAndFocus {
    pub fn focus_point(&self) -> Point {
        self.zipper_handle.focus_point(&self.focus)
    }

    fn select_to<L: Debug>(&self, target: &Point, _expr: &Expr<L>) -> Option<Handle> {
        println!("[select]");
        println!("self = {self:#?}");
        println!("target = {target:#?}");
        todo!()
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
            Handle::Span(handle) => handle.span_handle.contains_path(path),
            Handle::Zipper(handle) => handle.zipper_handle.contains_path(path),
        }
    }

    pub fn contains_point(&self, point: &Point) -> bool {
        match self {
            Handle::Point(handle) => handle == point,
            Handle::Span(handle) => handle.span_handle.contains_point(point),
            Handle::Zipper(handle) => handle.zipper_handle.contains_point(point),
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
        match self {
            Handle::Point(_handle) => {}
            Handle::Span(handle) => {
                if handle.span_handle.left == handle.span_handle.right {
                    *self = Handle::Point(handle.focus_point())
                }
            }
            // We could have considered handling the case when the middle span
            // is empty here, but that actually is a bug if it ever happens. So
            // instead, we're just never going to consider that case.
            Handle::Zipper(_handle) => {}
        }
    }

    pub fn move_select_dir<L: Debug>(&mut self, dir: MoveDir, expr: &Expr<L>) -> bool {
        match self {
            Handle::Point(handle) => {
                let mut target = handle.clone();
                let moved = target.move_dir(dir, expr);
                if moved {
                    match handle.select_to(&target, expr) {
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
            Handle::Span(handle) => {
                let mut target = handle.focus_point();
                let moved = target.move_dir(dir, expr);
                if moved {
                    match handle.select_to(&target, expr) {
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
            Handle::Zipper(handle) => {
                let mut target = handle.focus_point();
                let moved = target.move_dir(dir, expr);
                if moved {
                    match handle.select_to(&target, expr) {
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
    pub fn new(path: Path, index: Index) -> Self {
        Point { path, index }
    }

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
        inner_suffix: &[Step],
        inner: &Point,
        expr: &Expr<L>,
        inner_is_focus: bool,
    ) -> Option<Handle> {
        if let Some(step) = inner_suffix.first() {
            println!("[select] inner is beside-down outer");
            let inner_expr = expr.at_path(&inner.path);
            if step.is_left_of_index(&outer.index) {
                println!("[select] inner is to the left of outer");
                Some(Handle::Zipper(ZipperHandleAndFocus {
                    zipper_handle: ZipperHandle {
                        outer_path: outer.path.clone(),
                        outer_left: step.left_index(),
                        outer_right: outer.index.clone(),
                        middle_path: Path(inner_suffix.to_vec()),
                        inner_left: inner_expr.leftmost_index(),
                        inner_right: inner.index.clone(),
                    },
                    focus: if inner_is_focus {
                        ZipperFocus::InnerRight
                    } else {
                        ZipperFocus::OuterRight
                    },
                }))
            } else {
                println!("[select] inner is to the right of outer");
                Some(Handle::Zipper(ZipperHandleAndFocus {
                    zipper_handle: ZipperHandle {
                        outer_path: outer.path.clone(),
                        outer_left: outer.index.clone(),
                        outer_right: step.right_index(),
                        middle_path: Path(inner_suffix.to_vec()),
                        inner_left: inner.index.clone(),
                        inner_right: inner_expr.rightmost_index(),
                    },
                    focus: if inner_is_focus {
                        ZipperFocus::InnerLeft
                    } else {
                        ZipperFocus::OuterLeft
                    },
                }))
            }
        } else {
            println!("[select] inner is at or beside self");
            if inner.index.is_left_of_index(&outer.index) {
                println!("[select] inner is beside outer to the left");
                Some(Handle::Span(SpanHandleAndFocus {
                    span_handle: SpanHandle {
                        path: outer.path.clone(),
                        left: inner.index.clone(),
                        right: outer.index.clone(),
                    },
                    focus: if inner_is_focus {
                        SpanFocus::Left
                    } else {
                        SpanFocus::Right
                    },
                }))
            } else if inner.index.is_right_of_index(&outer.index) {
                println!("[select] inner is beside outer to the right");
                Some(Handle::Span(SpanHandleAndFocus {
                    span_handle: SpanHandle {
                        path: outer.path.clone(),
                        left: outer.index.clone(),
                        right: inner.index.clone(),
                    },
                    focus: if inner_is_focus {
                        SpanFocus::Right
                    } else {
                        SpanFocus::Left
                    },
                }))
            } else {
                println!("[select] inner is at outer");
                Some(Handle::Point(outer.clone()))
            }
        }
    }

    /// Calculates the selection from self to target.
    pub fn select_to<L: Debug>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
        println!("[select]");
        println!("self = {self:#?}");
        println!("target = {target:#?}");
        if let Some(target_suffix) = target.path.0.strip_prefix(self.path.0.as_slice()) {
            println!("[select] target is inside or beside self");
            Self::select_from_outer_to_inner(self, target_suffix, target, expr, true)
        } else if let Some(self_suffix) = self.path.0.strip_prefix(target.path.0.as_slice()) {
            println!("[select] self is inside or beside target");
            Self::select_from_outer_to_inner(target, self_suffix, self, expr, false)
        } else {
            // NOTE: Perhaps want to calculate the smallest span that contains both self and target
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
        self.is_left_of_step(&index.right_step())
    }

    pub fn is_right_of_index(&self, index: &Index) -> bool {
        // compare right step and right index so don't accidentally go negative
        self.right_step()
            .is_right_of_step(&index.right_index().left_step())
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
        index.0 < self.0
    }

    pub fn is_right_of_step(&self, step: &Step) -> bool {
        // compare right index and right step to avoid going negative
        self.right_index()
            .is_right_of_index(&step.right_step().left_index())
    }

    pub fn is_left_of_step(&self, step: &Step) -> bool {
        self.is_left_of_index(&step.right_index())
    }

    pub fn leftmost<'a>(i0: &'a Index, i1: &'a Index) -> &'a Index {
        if i0.is_left_of_index(i1) { i0 } else { i1 }
    }

    pub fn rightmost<'a>(i0: &'a Index, i1: &'a Index) -> &'a Index {
        if i0.is_right_of_index(i1) { i0 } else { i1 }
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

    pub fn origin_point(&self, focus: &SpanFocus) -> Point {
        match focus {
            SpanFocus::Left => self.right_point(),
            SpanFocus::Right => self.left_point(),
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        path.0
            .strip_prefix(self.path.0.as_slice())
            .and_then(|steps| {
                steps.first().and_then(|step| {
                    Some(step.is_right_of_index(&self.left) && step.is_left_of_index(&self.right))
                })
            })
            .unwrap_or(false)
    }

    pub fn contains_point(&self, point: &Point) -> bool {
        point
            .path
            .0
            .strip_prefix(self.path.0.as_slice())
            .and_then(|steps| match steps.first() {
                Some(step) => {
                    Some(step.is_right_of_index(&self.left) && step.is_left_of_index(&self.right))
                }
                None => Some(
                    point.index.is_right_of_index(&self.left)
                        && point.index.is_left_of_index(&self.right),
                ),
            })
            .unwrap_or(false)
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

    pub fn focus_point(&self, focus: &ZipperFocus) -> Point {
        match focus {
            ZipperFocus::OuterLeft => self.outer_left_point(),
            ZipperFocus::InnerLeft => self.inner_left_point(),
            ZipperFocus::InnerRight => self.inner_right_point(),
            ZipperFocus::OuterRight => self.outer_right_point(),
        }
    }

    pub fn outer_span_handle(&self) -> SpanHandle {
        SpanHandle {
            path: self.outer_path.clone(),
            left: self.outer_left.clone(),
            right: self.outer_right.clone(),
        }
    }

    pub fn inner_span_handle(&self) -> SpanHandle {
        SpanHandle {
            path: self.inner_path(),
            left: self.inner_left.clone(),
            right: self.inner_right.clone(),
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        self.outer_span_handle().contains_path(path)
            && !self.inner_span_handle().contains_path(path)
    }

    pub fn contains_point(&self, point: &Point) -> bool {
        self.outer_span_handle().contains_point(point)
            && !self.inner_span_handle().contains_point(point)
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

    pub fn leftmost_step(&self) -> Option<Step> {
        if self.kids.0.is_empty() {
            None
        } else {
            Some(Step(0))
        }
    }

    pub fn rightmost_step(&self) -> Option<Step> {
        if self.kids.0.is_empty() {
            None
        } else {
            Some(Step(self.kids.0.len() - 1))
        }
    }

    pub fn leftmost_index(&self) -> Index {
        Index(0)
    }

    pub fn rightmost_index(&self) -> Index {
        Index(self.kids.0.len())
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

#[cfg(test)]
mod tests {
    use super::*;

    type Expr = super::Expr<String>;

    mod span_handle_contains {
        use super::*;

        lazy_static::lazy_static! {
            static ref handle: Handle = Handle::Span(SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: Path(vec![(Step(2))]),
                    left: Index(1),
                    right: Index(4),
                },
                focus: SpanFocus::Left,
            });
        }

        mod path {
            use super::*;

            fn assert_handle_contains_path(path: &Path, result: bool) {
                assert_eq!(
                    handle.contains_path(path),
                    result,
                    "expected\n{:#?}\nto {}contain\n{:#?}",
                    *handle,
                    if result { "" } else { "not " },
                    path
                );
            }

            mod depth0 {
                use super::*;

                #[test]
                fn test() {
                    assert_handle_contains_path(&Path(vec![]), false)
                }
            }

            mod depth1 {
                use super::*;

                #[test]
                fn test_step0() {
                    assert_handle_contains_path(&Path(vec![Step(0)]), false)
                }
                #[test]
                fn test_step1() {
                    assert_handle_contains_path(&Path(vec![Step(1)]), false)
                }
                #[test]
                fn test_step2() {
                    assert_handle_contains_path(&Path(vec![Step(2)]), false)
                }
                #[test]
                fn test_step3() {
                    assert_handle_contains_path(&Path(vec![Step(3)]), false)
                }
                #[test]
                fn test_step4() {
                    assert_handle_contains_path(&Path(vec![Step(4)]), false)
                }
            }

            mod depth2 {
                use super::*;

                #[test]
                fn test_step0() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(0)]), false)
                }
                #[test]
                fn test_step1() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(1)]), true)
                }
                #[test]
                fn test_step2() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2)]), true)
                }
                #[test]
                fn test_step3() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(3)]), true)
                }
                #[test]
                fn test_step4() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(4)]), false)
                }
            }

            mod depth3 {
                use super::*;

                #[test]
                fn test_step0() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(0)]), true)
                }
                #[test]
                fn test_step1() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(1)]), true)
                }
                #[test]
                fn test_step2() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(2)]), true)
                }
                #[test]
                fn test_step3() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(3)]), true)
                }
                #[test]
                fn test_step4() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(4)]), true)
                }
            }
        }

        mod point {
            use super::*;

            fn assert_handle_contains_point(point: &Point, result: bool) {
                assert_eq!(
                    handle.contains_point(point),
                    result,
                    "expected\n{:#?}\nto {}contain\n{:#?}",
                    *handle,
                    if result { "" } else { "not " },
                    point
                );
            }

            mod depth1 {
                use super::*;

                #[test]
                fn test_index0() {
                    assert_handle_contains_point(&Point::new(Path(vec![]), Index(0)), false);
                }
                #[test]
                fn test_index1() {
                    assert_handle_contains_point(&Point::new(Path(vec![]), Index(1)), false);
                }
                #[test]
                fn test_index2() {
                    assert_handle_contains_point(&Point::new(Path(vec![]), Index(2)), false);
                }
                #[test]
                fn test_index3() {
                    assert_handle_contains_point(&Point::new(Path(vec![]), Index(3)), false);
                }
                #[test]
                fn test_index4() {
                    assert_handle_contains_point(&Point::new(Path(vec![]), Index(4)), false);
                }
                #[test]
                fn test_index5() {
                    assert_handle_contains_point(&Point::new(Path(vec![]), Index(5)), false);
                }
            }

            mod depth2 {
                use super::*;

                #[test]
                fn test_index0() {
                    assert_handle_contains_point(&Point::new(Path(vec![Step(2)]), Index(0)), false);
                }
                #[test]
                fn test_index1() {
                    assert_handle_contains_point(&Point::new(Path(vec![Step(2)]), Index(1)), false);
                }
                #[test]
                fn test_index2() {
                    assert_handle_contains_point(&Point::new(Path(vec![Step(2)]), Index(2)), true);
                }
                #[test]
                fn test_index3() {
                    assert_handle_contains_point(&Point::new(Path(vec![Step(2)]), Index(3)), true);
                }
                #[test]
                fn test_index4() {
                    assert_handle_contains_point(&Point::new(Path(vec![Step(2)]), Index(4)), false);
                }
                #[test]
                fn test_index5() {
                    assert_handle_contains_point(&Point::new(Path(vec![Step(2)]), Index(5)), false);
                }
            }
        }
    }

    mod zipper_handle_contains {
        use super::*;

        lazy_static::lazy_static! {
            static ref handle: Handle = Handle::Zipper(ZipperHandleAndFocus {
                zipper_handle: ZipperHandle {
                    outer_path: Path(vec![(Step(2))]),
                    outer_left: Index(1),
                    outer_right: Index(4),
                    middle_path: Path(vec![Step(2)]),
                    inner_left: Index(1),
                    inner_right: Index(4),
                },
                focus: ZipperFocus::OuterLeft,
            });
        }

        fn assert_handle_contains_path(path: &Path, result: bool) {
            assert_eq!(
                handle.contains_path(path),
                result,
                "expected\n{:#?}\nto {}contain\n{:#?}",
                *handle,
                if result { "" } else { "not " },
                path
            );
        }

        mod path {
            use super::*;

            mod depth0 {
                use super::*;

                #[test]
                fn test() {
                    assert_handle_contains_path(&Path(vec![]), false);
                }
            }

            mod depth1 {
                use super::*;

                #[test]
                fn test_step0() {
                    assert_handle_contains_path(&Path(vec![Step(0)]), false);
                }
                #[test]
                fn test_step1() {
                    assert_handle_contains_path(&Path(vec![Step(1)]), false);
                }
                #[test]
                fn test_step2() {
                    assert_handle_contains_path(&Path(vec![Step(2)]), false);
                }
                #[test]
                fn test_step3() {
                    assert_handle_contains_path(&Path(vec![Step(3)]), false);
                }
                #[test]
                fn test_step4() {
                    assert_handle_contains_path(&Path(vec![Step(4)]), false);
                }
            }

            mod depth2 {
                use super::*;

                #[test]
                fn test_step0() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(0)]), false);
                }
                #[test]
                fn test_step1() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(1)]), true);
                }
                #[test]
                fn test_step2() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2)]), true);
                }
                #[test]
                fn test_step3() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(3)]), true);
                }
                #[test]
                fn test_step4() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(4)]), false);
                }
            }

            mod depth3 {
                use super::*;

                #[test]
                fn test_step0() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(0)]), true);
                }
                #[test]
                fn test_step1() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(1)]), false);
                }
                #[test]
                fn test_step2() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(2)]), false);
                }
                #[test]
                fn test_step3() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(3)]), false);
                }
                #[test]
                fn test_step4() {
                    assert_handle_contains_path(&Path(vec![Step(2), Step(2), Step(4)]), true);
                }
            }
        }
    }
}
