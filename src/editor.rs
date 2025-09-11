use crate::expr::*;
use egui::Frame;
use std::fmt::Debug;

pub const MAX_EXPR_HEIGHT_FOR_HORIZONTAL: u32 = 2;

pub const NORMAL_BORDER_COLOR: egui::Color32 = egui::Color32::BLACK;
pub const NORMAL_TEXT_COLOR: egui::Color32 = egui::Color32::BLACK;
pub const NORMAL_BACKGROUND_COLOR: egui::Color32 = egui::Color32::TRANSPARENT;
pub const ACTIVE_TEXT_COLOR: egui::Color32 = egui::Color32::WHITE;
pub const ACTIVE_BACKGROUND_COLOR: egui::Color32 = egui::Color32::BLUE;
pub const HIGHLIGHT_BACKGROUND_COLOR: egui::Color32 = egui::Color32::LIGHT_BLUE;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLabel<Constructor, Diagnostic> {
    pub constructor: Constructor,
    pub diagnostic: Diagnostic,
}

#[derive(Debug)]
pub struct EditorState<Constructor, Diagnostic> {
    pub expr: Expr<ExprLabel<Constructor, Diagnostic>>,
    pub handle: Handle,
}

#[derive(Debug, Default)]
pub struct EditMenu {
    // TODO
}

pub trait EditorSpec {
    type Constructor: Debug + Clone;
    type Diagnostic: Debug + Clone;

    fn name() -> String;

    fn initial_state() -> EditorState<Self::Constructor, Self::Diagnostic>;

    fn get_edit_menu(state: EditorState<Self::Constructor, Self::Diagnostic>) -> EditMenu;

    fn get_diagnostics(
        state: EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> Vec<Self::Diagnostic>;

    fn render_label(
        ui: &mut egui::Ui,
        label: &ExprLabel<Self::Constructor, Self::Diagnostic>,
    ) -> egui::Response;

    fn update(state: &mut EditorState<Self::Constructor, Self::Diagnostic>, ctx: &egui::Context) {
        let mut focus_point = state.handle.focus_point();
        if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            state.handle.move_up(&state.expr);
        } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowLeft)) {
            focus_point.move_prev(&state.expr);
            state.handle = Handle::Point(focus_point);
        } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowRight)) {
            focus_point.move_next(&state.expr);
            state.handle = Handle::Point(focus_point);
        }
    }

    fn render(state: &mut EditorState<Self::Constructor, Self::Diagnostic>, ui: &mut egui::Ui) {
        Self::render_expr(state, ui, &state.expr.clone(), &Path::default());
    }

    fn render_point(
        state: &mut EditorState<Self::Constructor, Self::Diagnostic>,
        ui: &mut egui::Ui,
        point: &Point,
    ) {
        let is_handle = match &state.handle {
            Handle::Point(handle) => point == handle,
            Handle::Span((handle, _)) => {
                *point == handle.left_point() || *point == handle.right_point()
            }
            Handle::Zipper((handle, _)) => {
                *point == handle.outer_left_point()
                    || *point == handle.outer_right_point()
                    || *point == handle.inner_left_point()
                    || *point == handle.inner_right_point()
            }
        };

        let frame = Frame::new()
            .outer_margin(0)
            .inner_margin(egui::Margin {
                left: 4,
                right: 4,
                top: 0,
                bottom: 0,
            })
            .fill(if is_handle {
                ACTIVE_BACKGROUND_COLOR
            } else {
                NORMAL_BACKGROUND_COLOR
            });

        frame.show(ui, |ui| {
            let label = ui.label(egui::RichText::new(format!("•")).color(if is_handle {
                ACTIVE_TEXT_COLOR
            } else {
                ACTIVE_BACKGROUND_COLOR
            }));
            if label.clicked() {
                state.handle = Handle::Point(point.clone())
            }
        });
    }

    fn render_expr_contents(
        state: &mut EditorState<Self::Constructor, Self::Diagnostic>,
        ui: &mut egui::Ui,
        expr: &Expr<ExprLabel<Self::Constructor, Self::Diagnostic>>,
        path: &Path,
    ) {
        let in_handle = match &state.handle {
            Handle::Point(_point) => false,
            Handle::Span((handle, _focus)) => path.starts_with(&handle.path),
            Handle::Zipper((handle, focus)) => todo!(),
        };
        let frame = Frame::new()
            .outer_margin(0)
            .inner_margin(4)
            .fill(if in_handle {
                HIGHLIGHT_BACKGROUND_COLOR
            } else {
                NORMAL_BACKGROUND_COLOR
            })
            .stroke(egui::Stroke::new(1.0, NORMAL_BORDER_COLOR));

        frame.show(ui, |ui| {
            let label = Self::render_label(ui, &expr.label);
            if label.clicked() {
                let mut path = path.clone();
                if let Some(step_parent) = path.pop() {
                    state.handle = Handle::Span((
                        SpanHandle {
                            path: path,
                            left: step_parent.left_index(),
                            right: step_parent.right_index(),
                        },
                        SpanFocus::Left,
                    ));
                }
            }

            for (step, kid) in expr.kids_and_steps() {
                // render left point
                Self::render_point(
                    state,
                    ui,
                    &Point {
                        path: path.clone(),
                        index: step.left_index(),
                    },
                );

                // render kid
                let mut kid_path = path.clone();
                kid_path.push(step);
                Self::render_expr(state, ui, kid, &kid_path);
            }

            // render last point
            Self::render_point(
                state,
                ui,
                &Point {
                    path: path.clone(),
                    index: expr.kids.extreme_indexes().1,
                },
            );
        });

        ui.set_max_size(ui.min_size());
    }

    fn render_expr(
        state: &mut EditorState<Self::Constructor, Self::Diagnostic>,
        ui: &mut egui::Ui,
        expr: &Expr<ExprLabel<Self::Constructor, Self::Diagnostic>>,
        path: &Path,
    ) {
        if expr.height() <= MAX_EXPR_HEIGHT_FOR_HORIZONTAL {
            ui.horizontal_top(|ui| Self::render_expr_contents(state, ui, expr, path));
        } else {
            ui.vertical(|ui| Self::render_expr_contents(state, ui, expr, path));
        }
    }
}
