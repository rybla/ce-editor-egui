use egui::Frame;

use crate::{
    editor::{self, *},
    expr::*,
};

type C = String;
type D = String;

pub struct Ce {}

impl Ce {}

impl EditorSpec for Ce {
    type Constructor = C;
    type Diagnostic = D;

    fn name() -> String {
        format!("ce")
    }

    fn initial_state() -> CoreEditorState<Self> {
        CoreEditorState::new(
            Expr::new(
                ExprLabel {
                    constructor: format!("root"),
                    diagnostic: Default::default(),
                },
                Span::empty(),
            ),
            Default::default(),
        )
    }

    fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption<Self>> {
        vec![
            EditMenuOption::new(
                EditMenuPattern::Dynamic(format!("<name> (constructor)"), |query| {
                    Some(format!("{} (constructor)", query))
                }),
                |query, state| {
                    println!("insert_node: {query}");

                    let mut state = state;

                    let handle = state.expr.insert(
                        state.handle,
                        Fragment::Zipper(Zipper {
                            span_ol: Span::empty(),
                            span_or: Span::empty(),
                            middle: Context(vec![Tooth {
                                label: ExprLabel::new(query.clone(), String::new()),
                                span_l: Span::empty(),
                                span_r: Span::empty(),
                            }]),
                        }),
                    );

                    state.handle = handle;

                    Some(state)
                },
            ),
            EditMenuOption::new(EditMenuPattern::Static(format!("copy")), |_query, state| {
                println!("copy");
                state
                    .expr
                    .at_handle(&state.handle)
                    .map(|frag| CoreEditorState {
                        expr: state.expr.clone(),
                        handle: state.handle.clone(),
                        clipboard: Some(frag),
                    })
            }),
        ]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Self::Diagnostic> {
        Default::default()
    }

    fn is_valid_handle(_handle: &Handle, _expr: &EditorExpr<Self>) -> bool {
        true
    }

    fn render_label(ui: &mut egui::Ui, label: &ExprLabel<Self>) -> egui::Response {
        ui.add(egui::Button::new(egui::RichText::new(
            label.constructor.clone(),
        )))
    }

    fn assemble_rendered_expr(
        state: &mut EditorState<Self>,
        ui: &mut egui::Ui,
        path: &Path,
        expr: &EditorExpr<Self>,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_, Self>>)>,
    ) -> egui::Response {
        let color_scheme = editor::EditorState::<Ce>::color_scheme(ui);
        ui.style_mut().spacing.item_spacing.x = 0f32;
        ui.style_mut().spacing.item_spacing.y = 0f32;
        let frame = Frame::new()
            .outer_margin(0)
            .inner_margin(egui::Margin {
                left: 0,
                right: 0,
                top: 0,
                bottom: 0,
            })
            .fill(if state.core.handle.contains_path(path) {
                color_scheme.highlight_background
            } else {
                color_scheme.normal_background
            })
            // .stroke(egui::Stroke::new(1.0, color_scheme.normal_border));
            .stroke(egui::Stroke::new(0.0, color_scheme.normal_border));

        frame
            .show(ui, |ui| {
                ui.add(egui::Label::new("("));
                ui.add(
                    egui::Label::new(egui::RichText::new(expr.label.constructor.clone()))
                        .sense(egui::Sense::hover()),
                );
                for (step, kid) in render_steps_and_kids.iter() {
                    step.render(state, ui);
                    if let Some(kid) = kid {
                        kid.render(state, ui);
                    }
                }
                ui.add(egui::Label::new(")"));
            })
            .response
    }
}
