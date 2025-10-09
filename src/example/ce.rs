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

    fn assemble_rendered_expr(
        state: &mut EditorState<Self>,
        ui: &mut egui::Ui,
        path: &Path,
        expr: &EditorExpr<Self>,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_, Self>>)>,
    ) {
        let color_scheme = editor::EditorState::<Ce>::color_scheme(ui);

        let selected = state.core.handle.contains_path(path);
        let fill_color = if selected {
            color_scheme.highlight_background
        } else {
            color_scheme.normal_background
        };
        let text_color = if selected {
            color_scheme.active_text
        } else {
            color_scheme.normal_text
        };

        // TODO: generic way of handling newlines; does this need to be in ce directly, or would it be in a language built on top of ce?
        if expr.label.constructor == format!("newline") {
            ui.end_row();
        } else {
            egui::Frame::new().fill(fill_color).show(ui, |ui| {
                ui.add(egui::Label::new(egui::RichText::new("(").color(text_color)));
            });

            egui::Frame::new().fill(fill_color).show(ui, |ui| {
                ui.add(egui::Label::new(
                    egui::RichText::new(format!("{}", expr.label.constructor)).color(text_color),
                ));
            });

            for (step, kid) in render_steps_and_kids.iter() {
                step.render(state, ui);
                if let Some(kid) = kid {
                    kid.render(state, ui);
                }
            }

            egui::Frame::new().fill(fill_color).show(ui, |ui| {
                ui.add(egui::Label::new(egui::RichText::new(")").color(text_color)));
            });
        }
    }
}
