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

    pub fn origin_point(&self) -> Point {
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
                    if target_suffix_first_step.is_right_of_index(&self.span_handle.left)
                        && target_suffix_first_step.is_left_of_index(&self.span_handle.right)
                    {
                        println!("zone: A");
                        let subexpr = expr.at_expr(&target.path).1;
                        match self.focus {
                            SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.span_handle.path.clone(),
                                    outer_left: self.span_handle.left.clone(),
                                    outer_right: self.span_handle.right.clone(),
                                    middle_path: Path(target_suffix.to_vec()),
                                    inner_left: target.index.clone(),
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
                                    inner_right: target.index.clone(),
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
                    if target.index.is_left_of_index(&self.span_handle.left) {
                        println!("zone: B_left");
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
                                    left: target.index.clone(),
                                    right: self.span_handle.left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                        }
                    } else if target.index.is_left_of_index(&self.span_handle.right) {
                        println!("zone: B_middle");
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
                        println!("zone: B_right");
                        match self.focus {
                            SpanFocus::Left => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.span_handle.path.clone(),
                                    left: self.span_handle.right.clone(),
                                    right: target.index.clone(),
                                },
                                focus: SpanFocus::Right,
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
            if target.index.is_left_of_step(self_prefix_first_step) {
                println!("zone: C_left");
                match self.focus {
                    SpanFocus::Left => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: target.index.clone(),
                            outer_right: self_prefix_first_step.right_index(),
                            middle_path: Path(self_prefix.to_vec()),
                            inner_left: self.span_handle.left.clone(),
                            inner_right: self.span_handle.right.clone(),
                        },
                        focus: ZipperFocus::OuterLeft,
                    })),
                    SpanFocus::Right => {
                        let subexpr = expr.at_expr(&self.span_handle.path).1;
                        Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: target.path.clone(),
                                outer_left: target.index.clone(),
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
                            outer_right: target.index.clone(),
                            middle_path: Path(self_prefix.to_vec()),
                            inner_left: self.span_handle.left.clone(),
                            inner_right: self.span_handle.right.clone(),
                        },
                        focus: ZipperFocus::OuterRight,
                    })),
                    SpanFocus::Right => {
                        let subexpr = expr.at_expr(&self.span_handle.path).1;
                        Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: target.path.clone(),
                                outer_left: self_prefix_first_step.left_index(),
                                outer_right: target.index.clone(),
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

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandleAndFocus {
    pub zipper_handle: ZipperHandle,
    pub focus: ZipperFocus,
}

impl ZipperHandleAndFocus {
    pub fn focus_point(&self) -> Point {
        self.zipper_handle.focus_point(&self.focus)
    }

    // TODO: Could make self mut here so that can update in place.
    pub fn select_to<L: Debug + Clone>(&self, target: &Point, expr: &Expr<L>) -> Option<Handle> {
        println!("[select]");
        println!("self = {self:#?}");
        println!("target = {target:#?}");

        if let Some(target_suffix) = target
            .path
            .0
            .strip_prefix(self.zipper_handle.inner_path().0.as_slice())
        {
            println!("zone: A, B");
            match target_suffix.is_empty() {
                false => {
                    println!("zone: A");
                    let subexpr = expr.at_expr(&target.path).1;

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
                                inner_left: target.index.clone(),
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
                                    [&self.zipper_handle.inner_path().0.as_slice(), target_suffix]
                                        .concat(),
                                ),
                                inner_left: subexpr.leftmost_index(),
                                inner_right: target.index.clone(),
                            },
                            focus: ZipperFocus::InnerLeft,
                        })),
                        ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.zipper_handle.inner_path(),
                                outer_left: self.zipper_handle.inner_left.clone(),
                                outer_right: self.zipper_handle.inner_right.clone(),
                                middle_path: Path(target_suffix.to_vec()),
                                inner_left: target.index.clone(),
                                inner_right: subexpr.rightmost_index(),
                            },
                            focus: ZipperFocus::InnerLeft,
                        })),
                        ZipperFocus::OuterRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                            zipper_handle: ZipperHandle {
                                outer_path: self.zipper_handle.inner_path(),
                                outer_left: self.zipper_handle.inner_left.clone(),
                                outer_right: self.zipper_handle.inner_right.clone(),
                                middle_path: Path(target_suffix.to_vec()),
                                inner_left: subexpr.leftmost_index(),
                                inner_right: target.index.clone(),
                            },
                            focus: ZipperFocus::InnerRight,
                        })),
                    }
                }
                true => {
                    println!("zone: B");
                    if target
                        .index
                        .is_left_of_index(&self.zipper_handle.inner_left)
                    {
                        println!("zone: B_left");
                        match self.focus {
                            ZipperFocus::InnerLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: target.index.clone(),
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
                                    inner_right: target.index.clone(),
                                },
                                focus: ZipperFocus::InnerRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path(),
                                    left: target.index.clone(),
                                    right: self.zipper_handle.inner_left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path(),
                                    left: self.zipper_handle.inner_right.clone(),
                                    right: target.index.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                        }
                    } else if target
                        .index
                        .is_left_of_index(&self.zipper_handle.inner_right)
                    {
                        println!("zone: B_middle");
                        match self.focus {
                            ZipperFocus::InnerLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: self.zipper_handle.outer_right.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: target.index.clone(),
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
                                    inner_right: target.index.clone(),
                                },
                                focus: ZipperFocus::InnerRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path(),
                                    left: self.zipper_handle.inner_left.clone(),
                                    right: target.index.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path(),
                                    left: target.index.clone(),
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
                                    inner_left: target.index.clone(),
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
                                    inner_right: target.index.clone(),
                                },
                                focus: ZipperFocus::InnerRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path(),
                                    left: target.index.clone(),
                                    right: self.zipper_handle.inner_left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::OuterRight => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.inner_path(),
                                    left: self.zipper_handle.inner_right.clone(),
                                    right: target.index.clone(),
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
                        let subexpr = expr.at_expr(&target.path).1;
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
                                    outer_left: target.index.clone(),
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
                                    outer_right: target.index.clone(),
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
                                    inner_left: target.index.clone(),
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
                                    inner_right: target.index.clone(),
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
                    if target
                        .index
                        .is_left_of_index(&self.zipper_handle.outer_left)
                    {
                        println!("zone: D_outer_left");
                        match self.focus {
                            ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: target.index.clone(),
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
                                    left: target.index.clone(),
                                    right: self.zipper_handle.outer_left.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: target.index.clone(),
                                        right: self.zipper_handle.outer_right.clone(),
                                    },
                                    focus: SpanFocus::Left,
                                }))
                            }
                        }
                    } else if target
                        .index
                        .is_left_of_step(self.zipper_handle.middle_path.0.first().unwrap())
                    {
                        println!("zone: D_inner_left");
                        match self.focus {
                            ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: target.index.clone(),
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
                                    right: target.index.clone(),
                                },
                                focus: SpanFocus::Left,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: target.index.clone(),
                                        right: self.zipper_handle.outer_right.clone(),
                                    },
                                    focus: SpanFocus::Left,
                                }))
                            }
                        }
                    } else if target
                        .index
                        .is_left_of_index(&self.zipper_handle.outer_right)
                    {
                        println!("zone: D_inner_right");
                        match self.focus {
                            ZipperFocus::OuterRight => Some(Handle::Zipper(ZipperHandleAndFocus {
                                zipper_handle: ZipperHandle {
                                    outer_path: self.zipper_handle.outer_path.clone(),
                                    outer_left: self.zipper_handle.outer_left.clone(),
                                    outer_right: target.index.clone(),
                                    middle_path: self.zipper_handle.middle_path.clone(),
                                    inner_left: self.zipper_handle.inner_left.clone(),
                                    inner_right: self.zipper_handle.inner_right.clone(),
                                },
                                focus: ZipperFocus::OuterRight,
                            })),
                            ZipperFocus::OuterLeft => Some(Handle::Span(SpanHandleAndFocus {
                                span_handle: SpanHandle {
                                    path: self.zipper_handle.outer_path.clone(),
                                    left: target.index.clone(),
                                    right: self.zipper_handle.outer_right.clone(),
                                },
                                focus: SpanFocus::Right,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: self.zipper_handle.outer_left.clone(),
                                        right: target.index.clone(),
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
                                    outer_right: target.index.clone(),
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
                                    right: target.index.clone(),
                                },
                                focus: SpanFocus::Right,
                            })),
                            ZipperFocus::InnerLeft | ZipperFocus::InnerRight => {
                                Some(Handle::Span(SpanHandleAndFocus {
                                    span_handle: SpanHandle {
                                        path: self.zipper_handle.outer_path.clone(),
                                        left: self.zipper_handle.outer_left.clone(),
                                        right: target.index.clone(),
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
            if target.index.is_left_of_step(self_outer_suffix_first_step) {
                println!("zone: E_left");
                match self.focus {
                    ZipperFocus::OuterLeft => Some(Handle::Zipper(ZipperHandleAndFocus {
                        zipper_handle: ZipperHandle {
                            outer_path: target.path.clone(),
                            outer_left: target.index.clone(),
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
                            outer_right: target.index.clone(),
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
}

impl Handle {
    pub fn move_up<L: Debug + Clone>(&mut self, expr: &Expr<L>) -> bool {
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
                let kid = expr.at_expr(&handle.span_handle.path).1;
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
    pub fn move_dir<L: Debug + Clone>(&mut self, dir: MoveDir, expr: &Expr<L>) -> bool {
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
    pub fn move_dir<L: Debug + Clone>(&mut self, dir: MoveDir, expr: &Expr<L>) -> bool {
        let subexpr = expr.at_expr(&self.path).1;
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

    pub fn select_from_outer_to_inner<L: Debug + Clone>(
        outer: &Point,
        inner_suffix: &[Step],
        inner: &Point,
        expr: &Expr<L>,
        inner_is_focus: bool,
    ) -> Option<Handle> {
        if let Some(step) = inner_suffix.first() {
            println!("[select] inner is beside-down outer");
            let inner_expr = expr.at_expr(&inner.path).1;
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

/// An offset among the kids of an [Expr], which can by applied to either
/// [Index]s or [Step]s.
#[derive(Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Offset(pub usize);

impl Debug for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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

    pub fn add_offset(&self, offset: Offset) -> Index {
        Index(self.0 + offset.0)
    }

    pub fn sub_offset(&self, offset: Offset) -> Index {
        Index(self.0 - offset.0)
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

    fn middle_span_handle(&self) -> SpanHandle {
        SpanHandle {
            path: self.middle_path.clone(),
            left: self.inner_left.clone(),
            right: self.inner_right.clone(),
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
    pub fn height(&self) -> u32 {
        self.kids
            .0
            .iter()
            .fold(0, |h, e| std::cmp::max(h, 1 + e.height()))
    }

    pub fn at_expr(&self, path: &Path) -> (ExprContext<L>, Expr<L>) {
        let mut expr = self.clone();
        let mut ctx = ExprContext(Vec::default());
        for step in path.0.iter() {
            let (tooth, sub_expr) = expr.at_tooth(step);
            expr = sub_expr;
            ctx.0.push(tooth);
        }
        (ctx, expr.clone())
    }

    pub fn at_tooth(&self, step: &Step) -> (Tooth<L>, Expr<L>) {
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

    pub fn at_span_tooth(&self, left: &Index, right: &Index) -> (SpanTooth<L>, Span<L>) {
        let (left, middle, right) = self.kids.between(left, right);
        (
            SpanTooth {
                label: self.label.clone(),
                left,
                right,
            },
            middle,
        )
    }

    pub fn at_span(&self, handle: &SpanHandle) -> (SpanContext<L>, Span<L>) {
        let (sub_expr_ctx, sub_expr) = self.at_expr(&handle.path);
        let (tooth, inner) = sub_expr.at_span_tooth(&handle.left, &handle.right);
        (
            SpanContext {
                outer: sub_expr_ctx,
                inner: tooth,
            },
            inner,
        )
    }

    pub fn at_zipper(&self, handle: &ZipperHandle) -> (SpanContext<L>, Zipper<L>, Span<L>) {
        let (outer_ctx, span) = self.at_span(&handle.outer_span_handle());
        let (left, expr, right) = span.split_at_step(handle.middle_path.0.first().unwrap());
        let (span_ctx, inner_span) = expr.at_span(&handle.middle_span_handle());
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

    pub fn get_fragment_at_handle(&self, handle: &Handle) -> Option<Fragment<L>> {
        match handle {
            Handle::Point(_point) => None,
            Handle::Span(handle) => Some(Fragment::Span(self.at_span(&handle.span_handle).1)),
            Handle::Zipper(handle) => {
                Some(Fragment::Zipper(self.at_zipper(&handle.zipper_handle).1))
            }
        }
    }

    // pub fn insert_span_at_point(&self, point: &Point, span: Span<L>) -> Expr<L> {
    //     let (expr_ctx, sub_expr) = self.at_expr(&point.path);
    //     let sub_expr = sub_expr.insert_span_at_index(&point.index, span);
    //     expr_ctx.unwrap(sub_expr)
    // }

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

    pub fn insert_span_at_point(
        self,
        span: Span<L>,
        point: Point,
    ) -> (SpanHandleAndFocus, Expr<L>) {
        self.insert_span_at_span_handle(
            span,
            SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: point.path.clone(),
                    left: point.index.clone(),
                    right: point.index.clone(),
                },
                focus: SpanFocus::Left,
            },
        )
    }

    pub fn insert_zipper_at_point(
        self,
        zipper: Zipper<L>,
        point: Point,
    ) -> (SpanHandleAndFocus, Expr<L>) {
        self.insert_zipper_at_span_handle(
            zipper,
            SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: point.path,
                    left: point.index.clone(),
                    right: point.index,
                },
                focus: SpanFocus::Left,
            },
        )
    }

    pub fn insert_span_at_span_handle(
        self,
        span: Span<L>,
        handle: SpanHandleAndFocus,
    ) -> (SpanHandleAndFocus, Expr<L>) {
        // replace the selected span with the new span
        let (span_ctx, _) = self.at_span(&handle.span_handle);
        (
            SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: handle.span_handle.path,
                    // out of order in order to avoid clone
                    right: handle.span_handle.left.add_offset(span.offset()),
                    left: handle.span_handle.left,
                },
                focus: handle.focus,
            },
            span_ctx.unwrap(span),
        )
    }

    pub fn insert_zipper_at_span_handle(
        self,
        zipper: Zipper<L>,
        handle: SpanHandleAndFocus,
    ) -> (SpanHandleAndFocus, Expr<L>) {
        let (span_ctx, expr) = self.at_span(&handle.span_handle);
        let expr = zipper.unwrap(expr);
        let middle_path: Path = zipper.path();
        let inner_left_offset: Offset = zipper.inner_left_offset();
        let inner_right_offset: Offset = zipper.inner_right_offset();
        let left = Index(0).add_offset(inner_left_offset);
        let right = left.add_offset(inner_right_offset);
        (
            SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: Path(
                        [
                            handle.span_handle.path.0.as_slice(),
                            middle_path.0.as_slice(),
                        ]
                        .concat(),
                    ),
                    left: left,
                    right: right,
                },
                focus: handle.focus,
            },
            span_ctx.unwrap(zipper.unwrap(expr)),
        )
    }

    pub fn insert_span_at_zipper_handle(
        self,
        span: Span<L>,
        handle: ZipperHandleAndFocus,
    ) -> (SpanHandleAndFocus, Expr<L>) {
        // replaces the selected outer span with the new span
        self.insert_span_at_span_handle(
            span,
            SpanHandleAndFocus {
                span_handle: SpanHandle {
                    path: handle.zipper_handle.outer_path,
                    left: handle.zipper_handle.outer_left,
                    right: handle.zipper_handle.outer_right,
                },
                focus: match handle.focus {
                    ZipperFocus::OuterLeft | ZipperFocus::InnerLeft => SpanFocus::Left,
                    ZipperFocus::OuterRight | ZipperFocus::InnerRight => SpanFocus::Right,
                },
            },
        )
    }

    pub fn insert_zipper_at_zipper_handle(
        self,
        zipper: Zipper<L>,
        handle: ZipperHandleAndFocus,
    ) -> (ZipperHandleAndFocus, Expr<L>) {
        // replaces selected zipper with new zipper
        let (outer_ctx, span) = self.at_span(&handle.zipper_handle.outer_span_handle());
        let (outer_left, outer_expr, outer_right) =
            span.split_at_step(handle.zipper_handle.middle_path.0.first().unwrap());
        let (_, inner_span) = outer_expr.at_span(&handle.zipper_handle.middle_span_handle());
        (
            ZipperHandleAndFocus {
                zipper_handle: ZipperHandle {
                    outer_path: outer_ctx.path(),
                    outer_left: Index(0).add_offset(outer_ctx.left_offset()),
                    outer_right: Index(0)
                        .add_offset(outer_ctx.left_offset())
                        .add_offset(zipper.outer_left_offset())
                        .add_offset(Offset(1))
                        .add_offset(zipper.outer_right_offset()),
                    middle_path: zipper.path(),
                    inner_left: Index(0).add_offset(zipper.inner_left_offset()),
                    inner_right: Index(0)
                        .add_offset(zipper.inner_left_offset())
                        .add_offset(span.offset()),
                },
                focus: handle.focus,
            },
            outer_ctx.unwrap(Span(
                [outer_left.0, zipper.unwrap(inner_span).0, outer_right.0].concat(),
            )),
        )
    }

    pub fn insert_span_at_handle(self, span: Span<L>, handle: Handle) -> (Handle, Expr<L>) {
        match handle {
            Handle::Point(point) => {
                let (handle, expr) = self.insert_span_at_point(span, point);
                (Handle::Span(handle), expr)
            }
            Handle::Span(handle) => {
                let (handle, expr) = self.insert_span_at_span_handle(span, handle);
                (Handle::Span(handle), expr)
            }
            Handle::Zipper(handle) => {
                let (handle, expr) = self.insert_span_at_zipper_handle(span, handle);
                (Handle::Span(handle), expr)
            }
        }
    }

    pub fn insert_zipper_at_handle(self, zipper: Zipper<L>, handle: Handle) -> (Handle, Expr<L>) {
        match handle {
            Handle::Point(point) => {
                let (handle, expr) = self.insert_zipper_at_point(zipper, point);
                (Handle::Span(handle), expr)
            }
            Handle::Span(handle) => {
                let (handle, expr) = self.insert_zipper_at_span_handle(zipper, handle);
                (Handle::Span(handle), expr)
            }
            Handle::Zipper(handle) => {
                let (handle, expr) = self.insert_zipper_at_zipper_handle(zipper, handle);
                (Handle::Zipper(handle), expr)
            }
        }
    }

    pub fn insert_fragment_at_handle(self, frag: Fragment<L>, handle: Handle) -> (Handle, Expr<L>) {
        match frag {
            Fragment::Span(span) => self.insert_span_at_handle(span, handle),
            Fragment::Zipper(zipper) => self.insert_zipper_at_handle(zipper, handle),
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
}

/// A span of [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span<L>(pub Vec<Expr<L>>);

impl<L: Debug + Clone> Span<L> {
    pub fn at_step<'a>(&'a self, step: &Step) -> &'a Expr<L> {
        self.0
            .get(step.0)
            .unwrap_or_else(|| panic!("step out of bounds: span = {self:?}; step = {step:?}"))
    }

    pub fn extreme_indexes(&self) -> (Index, Index) {
        (Index(0), Index(self.0.len()))
    }

    pub fn between(&self, left: &Index, right: &Index) -> (Span<L>, Span<L>, Span<L>) {
        (
            Span(self.0[..left.0].to_vec()),
            Span(self.0[left.0..right.0].to_vec()),
            Span(self.0[right.0..].to_vec()),
        )
    }

    pub fn split_at_step(&self, step: &Step) -> (Span<L>, Expr<L>, Span<L>) {
        let (left, middle_and_right) = self.0.split_at(step.0);
        let (middle, right) = middle_and_right.split_first().unwrap();
        (Span(left.to_vec()), middle.clone(), Span(right.to_vec()))
    }

    fn offset(&self) -> Offset {
        Offset(self.0.len())
    }

    // fn insert_span_at_index(&self, index: &Index, span: Span<L>) -> Span<L> {
    //     let (left, right) = self.0.split_at(index.0);
    //     Span([left, span.0.as_slice(), right].concat())
    // }
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
    pub fn unwrap(&self, sub_expr: Expr<L>) -> Expr<L> {
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
    pub fn unwrap(&self, span: Span<L>) -> Expr<L> {
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
    pub fn unwrap(&self, span: Span<L>) -> Span<L> {
        Span(
            [
                self.outer_left.0.as_slice(),
                &[self.inner.unwrap(span)],
                self.outer_right.0.as_slice(),
            ]
            .concat(),
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
