use crate::expr::*;
use egui::Frame;
use std::fmt::Debug;

pub const COLOR_NORMAL_TEXT: egui::Color32 = egui::Color32::BLACK;
pub const COLOR_NORMAL_BACKGROUND: egui::Color32 = egui::Color32::WHITE;
pub const COLOR_ACTIVE_TEXT: egui::Color32 = egui::Color32::WHITE;
pub const COLOR_ACTIVE_BACKGROUND: egui::Color32 = egui::Color32::BLUE;

pub const MAX_EXPR_HEIGHT_FOR_HORIZONTAL: u32 = 1;

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
            .inner_margin(1)
            .outer_margin(1)
            .fill(if is_handle {
                COLOR_ACTIVE_BACKGROUND
            } else {
                COLOR_NORMAL_BACKGROUND
            });

        frame.show(ui, |ui| {
            let label = ui.label(egui::RichText::new(format!("|")).color(if is_handle {
                COLOR_ACTIVE_TEXT
            } else {
                COLOR_ACTIVE_BACKGROUND
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
        let frame = Frame::new()
            .inner_margin(4)
            .outer_margin(4)
            .fill(egui::Color32::WHITE)
            .stroke(egui::Stroke::new(1.0, egui::Color32::BLACK));

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

            for (step, is_last_step, kid) in expr.kids_and_steps() {
                // render left point
                Self::render_point(state, ui, &Point(path.clone(), step.left_index()));
                let right_index = if is_last_step {
                    Some(step.right_index())
                } else {
                    None
                };

                // render kid
                let mut kid_path = path.clone();
                kid_path.push(step);
                Self::render_expr(state, ui, kid, &kid_path);

                // render right (last) point if at end
                if let Some(right_index) = right_index {
                    Self::render_point(state, ui, &Point(path.clone(), right_index));
                }
            }
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
            ui.horizontal(|ui| Self::render_expr_contents(state, ui, expr, path));
        } else {
            ui.vertical(|ui| Self::render_expr_contents(state, ui, expr, path));
        }
    }
}
