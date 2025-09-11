use crate::expr::*;
use egui::Frame;
use std::fmt::Debug;

#[derive(Debug)]
pub struct ExprLabel<Constructor, Diagnostic> {
    pub constructor: Constructor,
    pub diagnostic: Diagnostic,
}

#[derive(Debug)]
pub struct EditorState<Constructor, Diagnostic> {
    pub expr: Expr<ExprLabel<Constructor, Diagnostic>>,
    pub handle: Handle,
}

#[derive(Debug)]
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

    fn render_label(ui: &mut egui::Ui, label: ExprLabel<Self::Constructor, Self::Diagnostic>);

    fn render_expr(
        state: &mut EditorState<Self::Constructor, Self::Diagnostic>,
        ui: &mut egui::Ui,
        expr: &Expr<ExprLabel<Self::Constructor, Self::Diagnostic>>,
        path: &Path,
    ) {
        ui.vertical(|ui| {
            let frame = Frame::new()
                .inner_margin(12)
                .outer_margin(12)
                .corner_radius(22)
                .fill(egui::Color32::BLUE)
                .stroke(match &state.handle {
                    Handle::Point(point) => {
                        if point.0 == *path {
                            egui::Stroke::new(2.0, egui::Color32::BLUE)
                        } else {
                            egui::Stroke::new(2.0, egui::Color32::BLACK)
                        }
                    }
                    Handle::Span((_span_handle, _span_focus)) => {
                        // TODO: highlight focuses
                        egui::Stroke::new(2.0, egui::Color32::BLACK)
                    }
                    Handle::Zipper((_zipper_handle, _zipper_focus)) => {
                        // TODO: highlight focuses
                        egui::Stroke::new(2.0, egui::Color32::BLACK)
                    }
                });

            frame.show(ui, |ui| {
                let label = ui.button(egui::RichText::new(format!("{:?}", expr.label)));
                if label.clicked() {
                    state.handle = Handle::Point(Point(path.clone(), Index(0)));
                }

                for (i, kid) in expr.kids.0.iter().enumerate() {
                    let step = Step(i);
                    let mut kid_path = path.clone();
                    kid_path.0.push(step);
                    Self::render_expr(state, ui, kid, path);
                }
            });

            ui.set_max_size(ui.min_size());
        });
    }
}
