#![allow(dead_code)]

use std::fmt::Debug;

use crate::utility::split_vec_at_index;

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
    pub fn to_ref<'a>(&'a self) -> SpanHandleAndFocusRef<'a> {
        SpanHandleAndFocusRef {
            span_handle: self.span_handle.to_ref(),
            focus: self.focus,
        }
    }

    pub fn focus_point<'a>(&'a self) -> PointRef<'a> {
        self.span_handle.focus_point(&self.focus)
    }

    pub fn origin_point<'a>(&'a self) -> PointRef<'a> {
        self.span_handle.origin_point(&self.focus)
    }

    // TODO: Could make self mut here so that can update in place.
    pub fn select_to<L: Debug + Clone>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
        println!("[select]");
        if let Some(target_suffix) = target
            .path
            .0
            .strip_prefix(self.span_handle.path.0.as_slice())
        {
            println!("zone: A, B");
            match target_suffix.first() {
                Some(target_suffix_first_step) => {
                    if target_suffix_first_step.is_right_of_index(self.span_handle.left)
                        && target_suffix_first_step.is_left_of_index(self.span_handle.right)
                    {
                        println!("zone: A");
                        let subexpr = expr.at_expr(target.path.to_ref()).1;
                        match self.focus {
                            SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.span_handle.path.clone(),
                                    outer_left: self.span_handle.left.clone(),
                                    outer_right: self.span_handle.right.clone(),
                                    middle_path: Path(target_suffix.to_vec()),
                                    inner_left: target.index,
                                    inner_right: subexpr.rightmost_index(),
                                },
                                focus: ZipperFocus::InnerLeft,
                            })),
                            SpanFocus::Right => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.span_handle.path.clone(),
                                    outer_left: self.span_handle.left.clone(),
                                    outer_right: self.span_handle.right.clone(),
                                    middle_path: Path(target_suffix.to_vec()),
                                    inner_left: subexpr.leftmost_index(),
                                    inner_right: target.index,
                                },
                                focus: ZipperFocus::InnerLeft,
                            })),
                        }
                    } else {
                        None
                    }
                }
                None => {
                    println!("zone: B");
                    if target.index.is_left_of_index(self.span_handle.left) {
                        println!("zone: B_left");
                        match self.focus {
                            SpanFocus::Left => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.span_handle.path.clone(),
                                    left: target.index,
                                    right: self.span_handle.right.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            SpanFocus::Right => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.span_handle.path.clone(),
                                    left: target.index,
                                    right: self.span_handle.left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                        }
                    } else if target.index.is_left_of_index(self.span_handle.right) {
                        println!("zone: B_middle");
                        match self.focus {
                            SpanFocus::Left => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.span_handle.path.clone(),
                                    left: target.index,
                                    right: self.span_handle.right.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            SpanFocus::Right => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.span_handle.path.clone(),
                                    left: self.span_handle.left.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Right,
                            })),
                        }
                    } else {
                        println!("zone: B_right");
                        match self.focus {
                            SpanFocus::Left => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.span_handle.path.clone(),
                                    left: self.span_handle.right.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Right,
                            })),
                            SpanFocus::Right => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.span_handle.path.clone(),
                                    left: self.span_handle.left.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Right,
                            })),
                        }
                    }
                }
            }
        } else if let Some(self_prefix) = self
            .span_handle
            .path
            .0
            .strip_prefix(target.path.0.as_slice())
        {
            println!("zone: C");
            let self_prefix_first_step = self_prefix
                .first()
                .unwrap_or_else(|| panic!("impossible since then would have matched previous self.span_handle.path.0.strip_prefix pattern"));
            if target.index.is_left_of_step(*self_prefix_first_step) {
                println!("zone: C_left");
                match self.focus {
                    SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: target.index,
                            outer_right: self_prefix_first_step.right_index(),
                            middle_path: Path(self_prefix.to_vec()),
                            inner_left: self.span_handle.left.clone(),
                            inner_right: self.span_handle.right.clone(),
                        },
                        focus: ZipperFocus::OuterLeft,
                    })),
                    SpanFocus::Right => {
                        let subexpr = expr.at_expr(self.span_handle.path.to_ref()).1;
                        Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: target.path.clone(),
                                outer_left: target.index,
                                outer_right: self_prefix_first_step.right_index(),
                                middle_path: Path(self_prefix.to_vec()),
                                inner_left: self.span_handle.left.clone(),
                                inner_right: subexpr.rightmost_index(),
                            },
                            focus: ZipperFocus::OuterLeft,
                        }))
                    }
                }
            } else {
                println!("zone: C_right");
                match self.focus {
                    SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: self_prefix_first_step.left_index(),
                            outer_right: target.index,
                            middle_path: Path(self_prefix.to_vec()),
                            inner_left: self.span_handle.left.clone(),
                            inner_right: self.span_handle.right.clone(),
                        },
                        focus: ZipperFocus::OuterRight,
                    })),
                    SpanFocus::Right => {
                        let subexpr = expr.at_expr(self.span_handle.path.to_ref()).1;
                        Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: target.path.clone(),
                                outer_left: self_prefix_first_step.left_index(),
                                outer_right: target.index,
                                middle_path: Path(self_prefix.to_vec()),
                                inner_left: self.span_handle.left.clone(),
                                inner_right: subexpr.rightmost_index(),
                            },
                            focus: ZipperFocus::OuterRight,
                        }))
                    }
                }
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpanHandleAndFocusRef<'a> {
    span_handle: SpanHandleRef<'a>,
    focus: SpanFocus,
}

impl<'a> SpanHandleAndFocusRef<'a> {
    pub fn cloned(self) -> SpanHandleAndFocus {
        SpanHandleAndFocus {
            span_handle: self.span_handle.cloned(),
            focus: self.focus.clone(),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandleAndFocus {
    pub zipper_handle: ZipperHandle,
    pub focus: ZipperFocus,
}

impl ZipperHandleAndFocus {
    pub fn focus_point<'a>(&'a self) -> PointRef<'a> {
        self.zipper_handle.focus_point(&self.focus)
    }

    // TODO: Could make self mut here so that can update in place.
    pub fn select_to<L: Debug + Clone>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
        println!("[select]");
        println!("self = {self:#?}");
        println!("target = {target:#?}");

        let prefix = self.zipper_handle.inner_path().cloned();
        if let Some(target_suffix) = target.path.0.strip_prefix(prefix.0.as_slice()) {
            println!("zone: A, B");
            match target_suffix.is_empty() {
                false => {
                    println!("zone: A");
                    let subexpr = expr.at_expr(target.path.to_ref()).1;

                    match self.focus {
                        ZipperFocus::InnerLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.zipper_handle.outer_path.clone(),
                                outer_left: self.zipper_handle.outer_left.clone(),
                                outer_right: self.zipper_handle.outer_right.clone(),
                                middle_path: Path(
                                    [&self.zipper_handle.middle_path.0.as_slice(), target_suffix]
                                        .concat(),
                                ),
                                inner_left: target.index,
                                inner_right: subexpr.rightmost_index(),
                            },
                            focus: ZipperFocus::InnerLeft,
                        })),
                        ZipperFocus::InnerRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.zipper_handle.outer_path.clone(),
                                outer_left: self.zipper_handle.outer_left.clone(),
                                outer_right: self.zipper_handle.outer_right.clone(),
                                middle_path: Path(
                                    [&self.zipper_handle.inner_path().cloned().0, target_suffix]
                                        .concat(),
                                ),
                                inner_left: subexpr.leftmost_index(),
                                inner_right: target.index,
                            },
                            focus: ZipperFocus::InnerLeft,
                        })),
                        ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.zipper_handle.inner_path().cloned(),
                                outer_left: self.zipper_handle.inner_left.clone(),
                                outer_right: self.zipper_handle.inner_right.clone(),
                                middle_path: Path(target_suffix.to_vec()),
                                inner_left: target.index,
                                inner_right: subexpr.rightmost_index(),
                            },
                            focus: ZipperFocus::InnerLeft,
                        })),
                        ZipperFocus::OuterRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.zipper_handle.inner_path().cloned(),
                                outer_left: self.zipper_handle.inner_left.clone(),
                                outer_right: self.zipper_handle.inner_right.clone(),
                                middle_path: Path(target_suffix.to_vec()),
                                inner_left: subexpr.leftmost_index(),
                                inner_right: target.index,
                            },
                            focus: ZipperFocus::InnerRight,
                        })),
                    }
                }
                true => {
                    println!("zone: B");
                    if target.index.is_left_of_index(self.zipper_handle.inner_left) {
                        println!("zone: B_left");
                        match self.focus {
                            ZipperFocus::InnerLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: target.index,
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::InnerLeft,
                            })),
                            ZipperFocus::InnerRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: target.index,
                                },
                                focus: ZipperFocus::InnerRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path().cloned(),
                                    left: target.index,
                                    right: self.zipper_handle.inner_left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path().cloned(),
                                    left: self.zipper_handle.inner_right.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Left,
                            })),
                        }
                    } else if target
                        .index
                        .is_left_of_index(self.zipper_handle.inner_right)
                    {
                        println!("zone: B_middle");
                        match self.focus {
                            ZipperFocus::InnerLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: target.index,
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::InnerLeft,
                            })),
                            ZipperFocus::InnerRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: target.index,
                                },
                                focus: ZipperFocus::InnerRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path().cloned(),
                                    left: self.zipper_handle.inner_left.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path().cloned(),
                                    left: target.index,
                                    right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: SpanFocus::Right,
                            })),
                        }
                    } else {
                        println!("zone: B_right");
                        // turns out this is the same as B_left
                        match self.focus {
                            ZipperFocus::InnerLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: target.index,
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::InnerLeft,
                            })),
                            ZipperFocus::InnerRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: target.index,
                                },
                                focus: ZipperFocus::InnerRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path().cloned(),
                                    left: target.index,
                                    right: self.zipper_handle.inner_left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path().cloned(),
                                    left: self.zipper_handle.inner_right.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Left,
                            })),
                        }
                    }
                }
            }
        } else if let Some(target_suffix) = target
            .path
            .0
            .strip_prefix(self.zipper_handle.outer_path.0.as_slice())
        {
            println!("zone: C, D");
            match target_suffix.is_empty() {
                false => {
                    if let Some(self_middle_path_suffix) = self
                        .zipper_handle
                        .middle_path
                        .0
                        .strip_prefix(target.path.0.as_slice())
                    {
                        println!("zone: C");
                        let subexpr = expr.at_expr(target.path.to_ref()).1;
                        match self.focus {
                            ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: Path(
                                        [
                                            self.zipper_handle.outer_path.0.clone(),
                                            target_suffix.to_vec(),
                                        ]
                                        .concat(),
                                    ),
                                    outer_left: target.index,
                                    outer_right: subexpr.rightmost_index(),
                                    middle_path: Path(self_middle_path_suffix.to_vec()),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::OuterLeft,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: Path(
                                        [
                                            self.zipper_handle.outer_path.0.clone(),
                                            target_suffix.to_vec(),
                                        ]
                                        .concat(),
                                    ),
                                    outer_left: subexpr.leftmost_index(),
                                    outer_right: target.index,
                                    middle_path: Path(self_middle_path_suffix.to_vec()),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::OuterRight,
                            })),
                            ZipperFocus::InnerLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: Path(
                                        [
                                            self.zipper_handle.outer_path.0.clone(),
                                            target_suffix.to_vec(),
                                        ]
                                        .concat(),
                                    ),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: Path(target_suffix.to_vec()),
                                    inner_left: target.index,
                                    inner_right: subexpr.rightmost_index(),
                                },
                                focus: ZipperFocus::InnerLeft,
                            })),
                            ZipperFocus::InnerRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: Path(
                                        [
                                            self.zipper_handle.outer_path.0.clone(),
                                            target_suffix.to_vec(),
                                        ]
                                        .concat(),
                                    ),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: Path(target_suffix.to_vec()),
                                    inner_left: subexpr.leftmost_index(),
                                    inner_right: target.index,
                                },
                                focus: ZipperFocus::InnerRight,
                            })),
                        }
                    } else {
                        // stepped along a different path from self.zipper_handle.middle_path
                        None
                    }
                }
                true => {
                    println!("zone: D");
                    if target.index.is_left_of_index(self.zipper_handle.outer_left) {
                        println!("zone: D_outer_left");
                        match self.focus {
                            ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: target.index,
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::OuterLeft,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.outer_path.clone(),
                                    left: target.index,
                                    right: self.zipper_handle.outer_left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: target.index,
                                        right: self.zipper_handle.outer_right.clone(),
                                    },
                                    focus: SpanFocus::Left,
                                }))
                            }
                        }
                    } else if target
                        .index
                        .is_left_of_step(*self.zipper_handle.middle_path.0.first().unwrap())
                    {
                        println!("zone: D_inner_left");
                        match self.focus {
                            ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: target.index,
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::OuterLeft,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.outer_path.clone(),
                                    left: self.zipper_handle.outer_left.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: target.index,
                                        right: self.zipper_handle.outer_right.clone(),
                                    },
                                    focus: SpanFocus::Left,
                                }))
                            }
                        }
                    } else if target
                        .index
                        .is_left_of_index(self.zipper_handle.outer_right)
                    {
                        println!("zone: D_inner_right");
                        match self.focus {
                            ZipperFocus::OuterRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: target.index,
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::OuterRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.outer_path.clone(),
                                    left: target.index,
                                    right: self.zipper_handle.outer_right.clone(),
                                },
                                focus: SpanFocus::Right,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: self.zipper_handle.outer_left.clone(),
                                        right: target.index,
                                    },
                                    focus: SpanFocus::Right,
                                }))
                            }
                        }
                    } else {
                        println!("zone: D_outer_right");
                        match self.focus {
                            ZipperFocus::OuterRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: target.index,
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::OuterRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.outer_path.clone(),
                                    left: self.zipper_handle.outer_right.clone(),
                                    right: target.index,
                                },
                                focus: SpanFocus::Right,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: self.zipper_handle.outer_left.clone(),
                                        right: target.index,
                                    },
                                    focus: SpanFocus::Right,
                                }))
                            }
                        }
                    }
                }
            }
        } else if let Some(self_outer_suffix) = self
            .zipper_handle
            .outer_path
            .0
            .strip_prefix(target.path.0.as_slice())
        {
            println!("zone: E");
            let self_outer_suffix_first_step = self_outer_suffix.first().unwrap_or_else(|| panic!("impossible since then would have matched previous self.zipper_handle.outer_path.0.strip_prefix pattern"));
            if target.index.is_left_of_step(*self_outer_suffix_first_step) {
                println!("zone: E_left");
                match self.focus {
                    ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: target.index,
                            outer_right: self_outer_suffix_first_step.right_index(),
                            middle_path: Path(
                                [
                                    self_outer_suffix.to_vec(),
                                    self.zipper_handle.middle_path.0.clone(),
                                ]
                                .concat(),
                            ),
                            inner_left: self.zipper_handle.inner_left.clone(),
                            inner_right: self.zipper_handle.inner_right.clone(),
                        },
                        focus: ZipperFocus::OuterLeft,
                    })),
                    ZipperFocus::OuterRight | ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                        Some(Handle::Point(target.clone()))
                    }
                }
            } else {
                println!("zone: E_right");
                match self.focus {
                    ZipperFocus::OuterRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: self_outer_suffix_first_step.left_index(),
                            outer_right: target.index,
                            middle_path: Path(
                                [
                                    self_outer_suffix.to_vec(),
                                    self.zipper_handle.middle_path.0.clone(),
                                ]
                                .concat(),
                            ),
                            inner_left: self.zipper_handle.inner_left.clone(),
                            inner_right: self.zipper_handle.inner_right.clone(),
                        },
                        focus: ZipperFocus::OuterLeft,
                    })),
                    ZipperFocus::OuterLeft | ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                        Some(Handle::Point(target.clone()))
                    }
                }
            }
        } else {
            // stepped along a different path from self.zipper_handle.outer_path
            None
        }
    }

    pub fn outer_span_handle_and_focus<'a>(&'a self) -> SpanHandleAndFocusRef<'a> {
        SpanHandleAndFocusRef {
            span_handle: self.zipper_handle.outer_span_handle(),
            focus: self.focus.to_span_focus(),
        }
    }

    pub fn outer_span_handle_and_focus_owned(self) -> SpanHandleAndFocus {
        SpanHandleAndFocus {
            span_handle: self.zipper_handle.outer_span_handle_owned(),
            focus: self.focus.to_span_focus(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ZipperHandleAndFocusRef<'a> {
    pub zipper_handle: ZipperHandleRef<'a>,
    pub focus: ZipperFocus,
}

impl<'a> ZipperHandleAndFocusRef<'a> {
    pub fn cloned(self) -> ZipperHandleAndFocus {
        ZipperHandleAndFocus {
            zipper_handle: self.zipper_handle.cloned(),
            focus: self.focus,
        }
    }
}

pub type MoveStatus = Result<(), MoveError>;

#[derive(Debug, Clone)]
pub enum MoveError {
    Boundary,
    Undefined,
    Invalid,
}

impl Handle {
    pub fn move_up<L: Debug + Clone>(&mut self, expr: &Expr<L>) -> MoveStatus {
        match self {
            Handle::Point(handle) => {
                if let Some(step) = handle.path.pop() {
                    *self = Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: handle.path.clone(),
                            left: step.left_index(),
                            right: step.right_index(),
                        },
                        focus: SpanFocus::Left,
                    });
                    MoveStatus::Ok(())
                } else {
                    MoveStatus::Err(MoveError::Boundary)
                }
            }
            Handle::Span(handle) => {
                let kid = expr.at_expr(handle.span_handle.path.to_ref()).1;
                let (leftmost, rightmost) = kid.kids.extreme_indexes();
                if handle.span_handle.left.is_left_of_index(leftmost)
                    || handle.span_handle.right.is_right_of_index(rightmost)
                {
                    // expand the span to stretch between the leftmost and rightmost indices
                    handle.span_handle.left = leftmost;
                    handle.span_handle.right = rightmost;
                    MoveStatus::Ok(())
                } else {
                    // span is already stretching between the leftmost and rightmost indices
                    if let Some(step) = handle.span_handle.path.pop() {
                        // move span to be around parent
                        handle.span_handle.left = step.left_index();
                        handle.span_handle.right = step.right_index();
                        MoveStatus::Ok(())
                    } else {
                        // alredy stretching around entire program
                        MoveStatus::Err(MoveError::Boundary)
                    }
                }
            }
            // NOTE: I'm not sure exactly what should happen when you move up at
            // a zipper, but by default for now, nothing happens.
            Handle::Zipper(_) => MoveStatus::Err(MoveError::Undefined),
        }
    }

    pub fn escape(&mut self) -> MoveStatus {
        match self {
            Handle::Point(_point) => MoveStatus::Err(MoveError::Invalid),
            Handle::Span(handle) => {
                *self = Handle::Point(handle.focus_point().cloned());
                MoveStatus::Ok(())
            }
            Handle::Zipper(handle) => {
                *self = Handle::Point(handle.focus_point().cloned());
                MoveStatus::Ok(())
            }
        }
    }

    pub fn move_dir<L: Debug + Clone>(&mut self, dir: MoveDir, expr: &Expr<L>) -> MoveStatus {
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
                    MoveDir::Prev => {
                        *self = Handle::Point(handle.span_handle.left_point().cloned())
                    }
                    MoveDir::Next => {
                        *self = Handle::Point(handle.span_handle.right_point().cloned())
                    }
                }
                MoveStatus::Ok(())
            }
            Handle::Zipper(handle) => {
                *self = Handle::Point(handle.focus_point().cloned());
                MoveStatus::Ok(())
            }
        }
    }

    pub fn focus_point<'a>(&'a self) -> PointRef<'a> {
        match self {
            Handle::Point(point) => point.to_ref(),
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
                    *self = Handle::Point(handle.focus_point().cloned())
                }
            }
            // We could have considered handling the case when the middle span
            // is empty here, but that actually is a bug if it ever happens. So
            // instead, we're just never going to consider that case.
            Handle::Zipper(_handle) => {}
        }
    }

    pub fn select_to<L: Debug + Clone>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
        match self {
            Handle::Point(point) => point.select_to(target, expr),
            Handle::Span(handle) => handle.select_to(target, expr),
            Handle::Zipper(handle) => handle.select_to(target, expr),
        }
    }

    pub fn move_select_dir<L: Debug + Clone>(&mut self, dir: MoveDir, expr: &Expr<L>) -> bool {
        match self {
            Handle::Point(handle) => {
                let mut target = handle.clone();
                let move_status = target.move_dir(dir, expr);
                if move_status.is_ok() {
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
                let mut target: Point = handle.focus_point().cloned();
                let move_status = target.move_dir(dir, expr);
                if move_status.is_ok() {
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
                let mut target: Point = handle.focus_point().cloned();
                let move_status = target.move_dir(dir, expr);
                if move_status.is_ok() {
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
    pub fn to_ref<'a>(&'a self) -> PointRef<'a> {
        PointRef {
            path: self.path.to_ref(),
            index: self.index,
        }
    }

    pub fn to_mut_ref<'a>(&'a mut self) -> PointMutRef<'a> {
        PointMutRef {
            path: &mut self.path,
            index: &mut self.index,
        }
    }

    pub fn new(path: Path, index: Index) -> Self {
        Point { path, index }
    }

    pub fn move_dir<L: Debug + Clone>(&mut self, dir: MoveDir, expr: &Expr<L>) -> MoveStatus {
        let subexpr = expr.at_expr(self.path.to_ref()).1;
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
                return MoveStatus::Ok(());
            }
            MoveStatus::Err(MoveError::Boundary)
        } else {
            let step = match dir {
                MoveDir::Prev => self.index.left_step(),
                MoveDir::Next => self.index.right_step(),
            };
            let kid = subexpr.kids.at_step(step);
            let kid_boundary = {
                let (kid_leftmost, kid_rightmost) = kid.kids.extreme_indexes();
                match dir {
                    MoveDir::Prev => kid_rightmost,
                    MoveDir::Next => kid_leftmost,
                }
            };
            self.path.push(step);
            self.index = kid_boundary;
            MoveStatus::Ok(())
        }
    }

    pub fn select_from_outer_to_inner<L: Debug + Clone>(
        outer: &Point,
        inner_suffix: &[Step],
        inner: &Point,
        expr: &Expr<L>,
        inner_is_focus: bool,
    ) -> Option<Handle> {
        if let Some(step) = inner_suffix.first() {
            println!("[select] inner is beside-down outer");
            let inner_expr = expr.at_expr(inner.path.to_ref()).1;
            if step.is_left_of_index(outer.index) {
                println!("[select] inner is to the left of outer");
                Some(Handle::Zipper(ZipperHandleAndFocus {
                    zipper_handle: ZipperHandle {
                        outer_path: outer.path.clone(),
                        outer_left: step.left_index(),
                        outer_right: outer.index,
                        middle_path: Path(inner_suffix.to_vec()),
                        inner_left: inner_expr.leftmost_index(),
                        inner_right: inner.index,
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
                        outer_left: outer.index,
                        outer_right: step.right_index(),
                        middle_path: Path(inner_suffix.to_vec()),
                        inner_left: inner.index,
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
            if inner.index.is_left_of_index(outer.index) {
                println!("[select] inner is beside outer to the left");
                Some(Handle::Span(SpanHandleAndFocus {
                    span_handle: SpanHandle {
                        path: outer.path.clone(),
                        left: inner.index,
                        right: outer.index,
                    },
                    focus: if inner_is_focus {
                        SpanFocus::Left
                    } else {
                        SpanFocus::Right
                    },
                }))
            } else if inner.index.is_right_of_index(outer.index) {
                println!("[select] inner is beside outer to the right");
                Some(Handle::Span(SpanHandleAndFocus {
                    span_handle: SpanHandle {
                        path: outer.path.clone(),
                        left: outer.index,
                        right: inner.index,
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
    // TODO: Could make self mut here so that can update in place.
    pub fn select_to<L: Debug + Clone>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
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

    pub fn add_offset(self, offset: Offset) -> Self {
        let mut point = self;
        if !point.path.0.is_empty() {
            point.path = point.path.add_offset(offset);
        } else {
            point.index = point.index.add_offset(offset);
        }
        point
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PointRef<'a> {
    pub path: PathRef<'a>,
    pub index: Index,
}

impl<'a> PointRef<'a> {
    pub fn cloned(self) -> Point {
        Point {
            path: self.path.cloned(),
            index: self.index,
        }
    }
}

pub struct PointMutRef<'a> {
    pub path: &'a mut Path,
    pub index: &'a mut Index,
}

/// A path from the top [Expr] to an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Path(pub Vec<Step>);

impl Path {
    pub fn to_ref<'a>(&'a self) -> PathRef<'a> {
        PathRef::Single(&self)
    }

    pub fn pop(&mut self) -> Option<Step> {
        self.0.pop()
    }

    pub fn push(&mut self, step: Step) {
        self.0.push(step);
    }

    pub fn starts_with(&self, path: &Path) -> bool {
        self.0.starts_with(&path.0)
    }

    pub fn concat(self, other: Self) -> Self {
        Path([self.0, other.0].concat())
    }

    pub fn add_offset(self, offset: Offset) -> Self {
        let mut path = self;
        if let Some(step) = path.0.first_mut() {
            *step = step.add_offset(offset);
        }
        path
    }
}

#[derive(Debug, Clone)]
pub enum PathRef<'a> {
    Single(&'a Path),
    Concat(Box<PathRef<'a>>, Box<PathRef<'a>>),
}

impl<'a> PartialEq for PathRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.to_vec().eq(&other.to_vec())
    }
}

impl<'a> PathRef<'a> {
    fn to_vec_helper(&self, steps: &mut Vec<Step>) {
        match self {
            PathRef::Single(path) => {
                for step in path.0.iter() {
                    steps.push(*step);
                }
            }
            PathRef::Concat(left, right) => {
                left.to_vec_helper(steps);
                right.to_vec_helper(steps);
            }
        }
    }

    pub fn to_vec(&self) -> Vec<Step> {
        let mut steps: Vec<Step> = vec![];
        self.to_vec_helper(&mut steps);
        steps
    }

    pub fn cloned(self) -> Path {
        let mut steps: Vec<Step> = vec![];
        for step in self.to_vec() {
            steps.push(step);
        }
        Path(steps)
    }

    pub fn chain(self, other: PathRef<'a>) -> PathRef<'a> {
        PathRef::Concat(Box::new(self), Box::new(other))
    }
}

/// An offset among the kids of an [Expr], which can by applied to either
/// [Index]s or [Step]s.
#[derive(Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Offset(pub usize);

impl Debug for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A step from an [Expr] to one of its kids.
#[derive(Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
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

    pub fn is_left_of_step(&self, step: Step) -> bool {
        self.0 < step.0
    }

    pub fn is_right_of_step(&self, step: Step) -> bool {
        self.0 > step.0
    }

    pub fn is_left_of_index(&self, index: Index) -> bool {
        self.is_left_of_step(index.right_step())
    }

    pub fn is_right_of_index(&self, index: Index) -> bool {
        // compare right step and right index so don't accidentally go negative
        self.right_step()
            .is_right_of_step(index.right_index().left_step())
    }

    fn add_offset(self, offset: Offset) -> Self {
        Step(self.0 + offset.0)
    }

    fn sub_offset(self, offset: Offset) -> Self {
        if offset.0 > self.0 {
            panic!("Offset is greater than the step:\n  - self: {self:?}\n  - offset: {offset:?}")
        }
        Step(self.0 - offset.0)
    }
}

/// An index between kids, or left the first kid, or right of the last kid of an
/// [Expr].
#[derive(Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Default)]
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

    pub fn is_left_of_index(&self, index: Index) -> bool {
        self.0 < index.0
    }

    pub fn is_right_of_index(&self, index: Index) -> bool {
        index.0 < self.0
    }

    pub fn is_right_of_step(&self, step: Step) -> bool {
        // compare right index and right step to avoid going negative
        self.right_index()
            .is_right_of_index(step.right_step().left_index())
    }

    pub fn is_left_of_step(&self, step: Step) -> bool {
        self.is_left_of_index(step.right_index())
    }

    pub fn leftmost(i0: Index, i1: Index) -> Index {
        if i0.is_left_of_index(i1) { i0 } else { i1 }
    }

    pub fn rightmost(i0: Index, i1: Index) -> Index {
        if i0.is_right_of_index(i1) { i0 } else { i1 }
    }

    pub fn add_offset(&self, offset: Offset) -> Index {
        Index(self.0 + offset.0)
    }

    pub fn sub_offset(&self, offset: Offset) -> Index {
        Index(self.0 - offset.0)
    }

    fn to_offset(&self) -> Offset {
        Offset(self.0)
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
    pub fn to_ref<'a>(&'a self) -> SpanHandleRef<'a> {
        SpanHandleRef {
            path: self.path.to_ref(),
            left: self.left,
            right: self.right,
        }
    }

    pub fn left_point<'a>(&'a self) -> PointRef<'a> {
        PointRef {
            path: self.path.to_ref(),
            index: self.left,
        }
    }

    pub fn right_point<'a>(&'a self) -> PointRef<'a> {
        PointRef {
            path: self.path.to_ref(),
            index: self.right,
        }
    }

    pub fn focus_point<'a>(&'a self, focus: &SpanFocus) -> PointRef<'a> {
        match focus {
            SpanFocus::Left => self.left_point(),
            SpanFocus::Right => self.right_point(),
        }
    }

    pub fn origin_point<'a>(&'a self, focus: &SpanFocus) -> PointRef<'a> {
        match focus {
            SpanFocus::Left => self.right_point(),
            SpanFocus::Right => self.left_point(),
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        self.to_ref().contains_path(path.to_ref())
    }

    pub fn contains_point(&self, point: &Point) -> bool {
        self.to_ref().contains_point(point.to_ref())
    }

    fn left_offset(&self) -> Offset {
        self.left.to_offset()
    }

    fn right_offset(&self) -> Offset {
        self.right.to_offset()
    }
}

#[derive(Debug, Clone)]
pub struct SpanHandleRef<'a> {
    pub path: PathRef<'a>,
    pub left: Index,
    pub right: Index,
}

impl<'a> SpanHandleRef<'a> {
    fn cloned(self) -> SpanHandle {
        SpanHandle {
            path: self.path.cloned(),
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }

    fn contains_path(&self, path: PathRef<'_>) -> bool {
        path.to_vec()
            .strip_prefix(self.path.to_vec().as_slice())
            .and_then(|steps| {
                steps.first().and_then(|step| {
                    Some(step.is_right_of_index(self.left) && step.is_left_of_index(self.right))
                })
            })
            .unwrap_or(false)
    }

    fn contains_point(&self, point: PointRef<'_>) -> bool {
        point
            .path
            .to_vec()
            .strip_prefix(self.path.to_vec().as_slice())
            .and_then(|steps| match steps.first() {
                Some(step) => {
                    Some(step.is_right_of_index(self.left) && step.is_left_of_index(self.right))
                }
                None => Some(
                    point.index.is_right_of_index(self.left)
                        && point.index.is_left_of_index(self.right),
                ),
            })
            .unwrap_or(false)
    }
}

/// A focus of a [SpanHandle].
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
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

    pub fn rotate_dir(&self, dir: MoveDir) -> SpanFocus {
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

/// A handle for a [Zipper].
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
    pub fn to_ref<'a>(&'a self) -> ZipperHandleRef<'a> {
        ZipperHandleRef {
            outer_path: self.outer_path.to_ref(),
            outer_left: self.outer_left,
            outer_right: self.outer_right,
            middle_path: self.middle_path.to_ref(),
            inner_left: self.inner_left,
            inner_right: self.inner_right,
        }
    }

    pub fn middle_path_first_step(&self) -> Step {
        *self
            .middle_path
            .0
            .first()
            .unwrap_or_else(|| panic!("ZipperHandle's middle_path must be nonempty"))
    }

    pub fn inner_path<'a>(&'a self) -> PathRef<'a> {
        self.outer_path.to_ref().chain(self.middle_path.to_ref())
    }

    pub fn outer_left_point<'a>(&'a self) -> PointRef<'a> {
        PointRef {
            path: self.outer_path.to_ref(),
            index: self.outer_left,
        }
    }

    pub fn outer_right_point<'a>(&'a self) -> PointRef<'a> {
        PointRef {
            path: self.outer_path.to_ref(),
            index: self.outer_right,
        }
    }

    pub fn inner_left_point<'a>(&'a self) -> PointRef<'a> {
        PointRef {
            path: self.inner_path(),
            index: self.inner_left,
        }
    }

    pub fn inner_right_point<'a>(&'a self) -> PointRef<'a> {
        PointRef {
            path: self.inner_path(),
            index: self.inner_right,
        }
    }

    pub fn focus_point<'a>(&'a self, focus: &ZipperFocus) -> PointRef<'a> {
        match focus {
            ZipperFocus::OuterLeft => self.outer_left_point(),
            ZipperFocus::InnerLeft => self.inner_left_point(),
            ZipperFocus::InnerRight => self.inner_right_point(),
            ZipperFocus::OuterRight => self.outer_right_point(),
        }
    }

    pub fn outer_span_handle<'a>(&'a self) -> SpanHandleRef<'a> {
        SpanHandleRef {
            path: self.outer_path.to_ref(),
            left: self.outer_left,
            right: self.outer_right,
        }
    }

    fn outer_span_handle_owned(self) -> SpanHandle {
        SpanHandle {
            path: self.outer_path,
            left: self.outer_left,
            right: self.outer_right,
        }
    }

    pub fn contains_path(&self, path: &Path) -> bool {
        self.outer_span_handle().contains_path(path.to_ref())
            && !self.inner_span_handle().contains_path(path.to_ref())
    }

    pub fn contains_point(&self, point: &Point) -> bool {
        self.outer_span_handle().contains_point(point.to_ref())
            && !self.inner_span_handle().contains_point(point.to_ref())
    }

    fn middle_span_handle<'a>(&'a self) -> SpanHandleRef<'a> {
        SpanHandleRef {
            path: self.middle_path.to_ref(),
            left: self.inner_left,
            right: self.inner_right,
        }
    }

    fn inner_span_handle<'a>(&'a self) -> SpanHandleRef<'a> {
        SpanHandleRef {
            path: self.inner_path(),
            left: self.inner_left,
            right: self.inner_right,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ZipperHandleRef<'a> {
    pub outer_path: PathRef<'a>,
    pub outer_left: Index,
    pub outer_right: Index,
    pub middle_path: PathRef<'a>,
    pub inner_left: Index,
    pub inner_right: Index,
}

impl<'a> ZipperHandleRef<'a> {
    pub fn cloned(self) -> ZipperHandle {
        ZipperHandle {
            outer_path: self.outer_path.cloned(),
            outer_left: self.outer_left,
            outer_right: self.outer_right,
            middle_path: self.middle_path.cloned(),
            inner_left: self.inner_left,
            inner_right: self.inner_right,
        }
    }

    fn inner_path(self) -> PathRef<'a> {
        self.outer_path.chain(self.middle_path)
    }

    fn outer_span_handle(self) -> SpanHandleRef<'a> {
        SpanHandleRef {
            path: self.outer_path,
            left: self.outer_left,
            right: self.outer_right,
        }
    }
}

/// A focus fro a [ZipperHandle].
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum ZipperFocus {
    OuterLeft,
    InnerLeft,
    OuterRight,
    InnerRight,
}

impl ZipperFocus {
    pub fn rotate_dir(&self, dir: MoveDir) -> ZipperFocus {
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

    fn to_span_focus(&self) -> SpanFocus {
        match self {
            ZipperFocus::OuterLeft => SpanFocus::Left,
            ZipperFocus::InnerLeft => SpanFocus::Left,
            ZipperFocus::OuterRight => SpanFocus::Right,
            ZipperFocus::InnerRight => SpanFocus::Right,
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
    Zipper(Zipper<L>),
}

/// An expression of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Expr<L> {
    pub label: L,
    pub kids: Span<L>,
}

impl<L: Debug + Clone> Expr<L> {
    pub fn new(label: L, kids: Vec<Self>) -> Self {
        Expr {
            label,
            kids: Span(kids),
        }
    }

    pub fn height(&self) -> u32 {
        self.kids
            .0
            .iter()
            .fold(0, |h, e| std::cmp::max(h, 1 + e.height()))
    }

    pub fn at_expr(&self, path: PathRef<'_>) -> (ExprContext<L>, Expr<L>) {
        let mut expr = self.clone();
        let mut ctx = ExprContext(Vec::default());
        for step in path.to_vec() {
            let (tooth, sub_expr) = expr.at_tooth(step);
            expr = sub_expr;
            ctx.0.push(tooth);
        }
        (ctx, expr.clone())
    }

    pub fn at_tooth(self, step: Step) -> (Tooth<L>, Expr<L>) {
        let (left, middle, right) = self.kids.split_at_step(step);
        (
            Tooth {
                label: self.label.clone(),
                left: left,
                right: right,
            },
            middle,
        )
    }

    pub fn at_span_tooth(self, left: Index, right: Index) -> (SpanTooth<L>, Span<L>) {
        let (left, middle, right) = self.kids.split_at_index_range(left, right);
        (
            SpanTooth {
                label: self.label.clone(),
                left,
                right,
            },
            middle,
        )
    }

    pub fn at_span(&self, handle: SpanHandleRef<'_>) -> (SpanContext<L>, Span<L>) {
        let (sub_expr_ctx, sub_expr) = self.at_expr(handle.path);
        let (tooth, inner) = sub_expr.at_span_tooth(handle.left, handle.right);
        (
            SpanContext {
                outer: sub_expr_ctx,
                inner: tooth,
            },
            inner,
        )
    }

    pub fn at_zipper(self, handle: &ZipperHandle) -> (SpanContext<L>, Zipper<L>, Span<L>) {
        let (outer_ctx, span) = self.at_span(handle.outer_span_handle());
        let (left, expr, right) = span.split_at_step(*handle.middle_path.0.first().unwrap());
        let (span_ctx, inner_span) = expr.at_span(handle.middle_span_handle());
        (
            outer_ctx,
            Zipper {
                outer_left: left,
                outer_right: right,
                inner: span_ctx,
            },
            inner_span,
        )
    }

    pub fn get_fragment_at_handle(self, handle: &Handle) -> Option<Fragment<L>> {
        match handle {
            Handle::Point(_point) => None,
            Handle::Span(handle) => {
                Some(Fragment::Span(self.at_span(handle.span_handle.to_ref()).1))
            }
            Handle::Zipper(handle) => {
                Some(Fragment::Zipper(self.at_zipper(&handle.zipper_handle).1))
            }
        }
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

    // TODO: could make all these insert/delete method be over mutable references to self so that dont have to clone as much

    pub fn get_expr_at_path_mut<'a>(&'a mut self, path: PathRef<'_>) -> &'a mut Expr<L> {
        let mut expr = self;
        for step in path.to_vec() {
            expr = expr.get_expr_at_step_mut(step);
        }
        expr
    }

    pub fn insert_span_at_point(&mut self, point: Point, span: Span<L>) -> SpanHandleAndFocus {
        self.insert_span_at_span_handle(
            SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: point.path,
                    left: point.index,
                    right: point.index,
                },
                focus: SpanFocus::Left,
            },
            span,
        )
    }

    pub fn insert_span_at_span_handle(
        &mut self,
        handle: SpanHandleAndFocus,
        span: Span<L>,
    ) -> SpanHandleAndFocus {
        let span_offset = span.offset();
        let _ = self.splice_span(handle.span_handle.to_ref(), span);
        SpanHandleAndFocus {
            span_handle: SpanHandle {
                path: handle.span_handle.path,
                left: handle.span_handle.left,
                right: handle.span_handle.right.add_offset(span_offset),
            },
            focus: handle.focus,
        }
    }

    pub fn insert_zipper_at_point(
        &mut self,
        point: Point,
        zipper: Zipper<L>,
    ) -> SpanHandleAndFocus {
        self.insert_zipper_at_span_handle(
            SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: point.path,
                    left: point.index,
                    right: point.index,
                },
                focus: SpanFocus::Left,
            },
            zipper,
        )
    }

    pub fn insert_zipper_at_span_handle(
        &mut self,
        handle: SpanHandleAndFocus,
        zipper: Zipper<L>,
    ) -> SpanHandleAndFocus {
        let span = self.splice_span(handle.span_handle.to_ref(), Span::empty());
        let span_offset = span.offset();
        let (zipper_point, span) = zipper.unwrap(span);
        let span_handle_ref = handle.span_handle.to_ref();
        self.splice_span(
            SpanHandleRef {
                path: span_handle_ref.path,
                left: span_handle_ref.left,
                right: span_handle_ref.left,
            },
            span,
        );
        let handle_left_offset = handle.span_handle.left_offset();
        SpanHandleAndFocus {
            span_handle: SpanHandle {
                path: handle
                    .span_handle
                    .path
                    .concat(zipper_point.path.add_offset(handle_left_offset)),
                left: zipper_point.index,
                right: zipper_point.index.add_offset(span_offset),
            },
            focus: handle.focus,
        }
    }

    pub fn insert_zipper_at_zipper_handle(
        &mut self,
        handle: ZipperHandleAndFocus,
        zipper: Zipper<L>,
    ) -> SpanHandleAndFocus {
        let zipper_outer_offset = zipper.outer_offset();
        let path = handle.zipper_handle.outer_path.clone();
        let left = handle.zipper_handle.outer_left;
        let right = handle
            .zipper_handle
            .outer_left
            .add_offset(zipper_outer_offset);
        let _ = self.splice_zipper(handle.zipper_handle.to_ref(), zipper);
        SpanHandleAndFocus {
            span_handle: SpanHandle { path, left, right },
            focus: handle.focus.to_span_focus(),
        }
    }

    pub fn insert_span_at_zipper_handle(
        &mut self,
        handle: ZipperHandleAndFocus,
        span: Span<L>,
    ) -> SpanHandleAndFocus {
        self.insert_span_at_span_handle(handle.outer_span_handle_and_focus_owned(), span)
    }

    pub fn insert_span_at_handle(&mut self, handle: Handle, span: Span<L>) -> Handle {
        match handle {
            Handle::Point(point) => Handle::Span(self.insert_span_at_point(point, span)),
            Handle::Span(span_handle) => {
                Handle::Span(self.insert_span_at_span_handle(span_handle, span))
            }
            Handle::Zipper(zipper_handle) => {
                Handle::Span(self.insert_span_at_zipper_handle(zipper_handle, span))
            }
        }
    }

    pub fn insert_zipper_at_handle(&mut self, handle: Handle, zipper: Zipper<L>) -> Handle {
        match handle {
            Handle::Point(point) => Handle::Span(self.insert_zipper_at_point(point, zipper)),
            Handle::Span(handle) => Handle::Span(self.insert_zipper_at_span_handle(handle, zipper)),
            Handle::Zipper(handle) => {
                Handle::Span(self.insert_zipper_at_zipper_handle(handle, zipper))
            }
        }
    }

    pub fn insert_fragment_at_handle(&mut self, handle: Handle, frag: Fragment<L>) -> Handle {
        match frag {
            Fragment::Span(span) => self.insert_span_at_handle(handle, span),
            Fragment::Zipper(zipper) => self.insert_zipper_at_handle(handle, zipper),
        }
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

    pub fn get_expr_at_step_mut<'a>(&'a mut self, step: Step) -> &'a mut Expr<L> {
        self.kids.get_expr_at_step_mut(step)
    }

    pub fn splice_span(&mut self, handle: SpanHandleRef<'_>, new_span: Span<L>) -> Span<L> {
        let expr = self.get_expr_at_path_mut(handle.path);
        expr.kids
            .splice_span_at_index_range(handle.left, handle.right, new_span)
    }

    pub fn splice_zipper(
        &mut self,
        handle: ZipperHandleRef<'_>,
        new_zipper: Zipper<L>,
    ) -> Zipper<L> {
        let result = new_zipper.clone();

        let inner_expr = self.get_expr_at_path_mut(handle.clone().inner_path());
        let inner_span = inner_expr.kids.splice_span_at_index_range(
            handle.inner_left,
            handle.inner_right,
            Span::empty(),
        );
        let (_, outer_span) = new_zipper.unwrap(inner_span);
        let outer_expr = self.get_expr_at_path_mut(handle.outer_path.clone());
        outer_expr.splice_span(handle.outer_span_handle(), outer_span);

        result
    }
}

/// A span of [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span<L>(pub Vec<Expr<L>>);

impl<L: Debug + Clone> Span<L> {
    pub fn concat(self, other: Self) -> Self {
        Span([self.0, other.0].concat())
    }

    pub fn at_step<'a>(&'a self, step: Step) -> &'a Expr<L> {
        self.0
            .get(step.0)
            .unwrap_or_else(|| panic!("step out of bounds: span = {self:?}; step = {step:?}"))
    }

    pub fn extreme_indexes(&self) -> (Index, Index) {
        (Index(0), Index(self.0.len()))
    }

    pub fn split_at_index_range(self, left: Index, right: Index) -> (Self, Self, Self) {
        assert!(left.0 < right.0);
        assert!(self.is_index_in_range(left));
        assert!(self.is_index_in_range(right));
        let (es_l, es_mr) = split_vec_at_index(self.0, left.0);
        let (es_m, es_r) = split_vec_at_index(es_mr, right.0 - left.0);
        (Span(es_l), Span(es_m), Span(es_r))
    }

    pub fn split_at_step(self, step: Step) -> (Span<L>, Expr<L>, Span<L>) {
        let (left, middle, right) =
            self.split_at_index_range(step.left_index(), step.right_index());
        (left, middle.0.into_iter().nth(0).unwrap(), right)
    }

    pub fn offset(&self) -> Offset {
        Offset(self.0.len())
    }

    pub fn is_index_in_range(&self, index: Index) -> bool {
        index.0 < self.0.len()
    }

    pub fn insert_span_at_index(&mut self, index: Index, span: Span<L>) {
        assert!(self.is_index_in_range(index));
        self.0.splice(index.0..index.0, span.0);
    }

    pub fn splice_span_at_index_range(
        &mut self,
        start_index: Index,
        end_index: Index,
        span: Span<L>,
    ) -> Span<L> {
        assert!(self.is_index_in_range(start_index));
        assert!(self.is_index_in_range(end_index));
        assert!(start_index.0 <= end_index.0);
        Span(
            self.0
                .splice(start_index.0..end_index.0, span.0)
                .collect::<Vec<_>>(),
        )
    }

    fn get_expr_at_step_mut<'a>(&'a mut self, step: Step) -> &'a mut Expr<L> {
        self.0
            .get_mut(step.0)
            .unwrap_or_else(|| panic!("Step is out of bounds in Span"))
    }

    fn empty() -> Span<L> {
        Span(vec![])
    }
}

/// A tooth of an [Expr] around a [Step].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub label: L,
    pub left: Span<L>,
    pub right: Span<L>,
}

impl<L: Debug + Clone> Tooth<L> {
    pub fn unwrap(&self, sub_expr: Expr<L>) -> Expr<L> {
        Expr {
            label: self.label.clone(),
            kids: Span([self.left.0.as_slice(), &[sub_expr], self.right.0.as_slice()].concat()),
        }
    }

    pub fn step(&self) -> Step {
        Step(self.left.0.len())
    }
}

/// A tooth of an [Expr] around a range of [Index]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanTooth<L> {
    pub label: L,
    pub left: Span<L>,
    pub right: Span<L>,
}

impl<L: Debug + Clone> SpanTooth<L> {
    pub fn unwrap(&self, span: Span<L>) -> Expr<L> {
        Expr {
            label: self.label.clone(),
            kids: Span(
                [
                    self.left.0.as_slice(),
                    span.0.as_slice(),
                    self.right.0.as_slice(),
                ]
                .concat(),
            ),
        }
    }

    pub fn inner_right_offset(&self) -> Offset {
        Offset(self.left.0.len())
    }

    pub fn inner_left_offset(&self) -> Offset {
        Offset(self.right.0.len())
    }
}

/// A context around an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ExprContext<L>(pub Vec<Tooth<L>>);

impl<L: Debug + Clone> ExprContext<L> {
    pub fn unwrap(self, sub_expr: Expr<L>) -> Expr<L> {
        self.0
            .iter()
            .rfold(sub_expr, |sub_expr, tooth| tooth.unwrap(sub_expr))
    }

    pub fn path(&self) -> Path {
        Path(self.0.iter().map(|tooth| tooth.step()).collect())
    }
}

/// A context around a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanContext<L> {
    pub outer: ExprContext<L>,
    pub inner: SpanTooth<L>,
}

impl<L: Debug + Clone> SpanContext<L> {
    pub fn unwrap(self, span: Span<L>) -> Expr<L> {
        self.outer.unwrap(self.inner.unwrap(span))
    }

    pub fn left_offset(&self) -> Offset {
        self.inner.inner_left_offset()
    }

    pub fn right_offset(&self) -> Offset {
        self.inner.inner_right_offset()
    }

    pub fn path(&self) -> Path {
        self.outer.path()
    }
}

/// A zipper between two [SpanHandle]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper<L> {
    pub outer_left: Span<L>,
    pub outer_right: Span<L>,
    pub inner: SpanContext<L>,
}

impl<L: Debug + Clone> Zipper<L> {
    pub fn unwrap(self, span: Span<L>) -> (Point, Span<L>) {
        (
            Point {
                path: self.inner.path(),
                index: Index(0).add_offset(self.inner_left_offset()),
            },
            Span(
                [
                    self.outer_left.0.as_slice(),
                    &[self.inner.unwrap(span)],
                    self.outer_right.0.as_slice(),
                ]
                .concat(),
            ),
        )
    }

    pub fn path(&self) -> Path {
        self.inner.path()
    }

    /// The offset at the innermost of the zipper from the leftmost index to the
    /// zipper's inner left index.
    pub fn inner_left_offset(&self) -> Offset {
        self.inner.left_offset()
    }

    /// The offset at the innermost of the zipper from the rightmost index to
    /// the zipper's inner right index.
    pub fn inner_right_offset(&self) -> Offset {
        self.inner.right_offset()
    }

    fn outer_left_offset(&self) -> Offset {
        self.outer_left.offset()
    }

    fn outer_right_offset(&self) -> Offset {
        self.outer_right.offset()
    }

    fn outer_offset(&self) -> Offset {
        self.outer_left_offset() + self.outer_right_offset()
    }
}

impl std::ops::Add for Offset {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Offset(self.0 + other.0)
    }
}

impl std::ops::Sub for Offset {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Offset(self.0 - other.0)
    }
}

// -----------------------------------------------------------------------------
// Miscellaneous
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum MoveDir {
    Prev,
    Next,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum CycleDir {
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
