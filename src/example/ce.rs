use crate::{
    editor::{self, *},
    expr::*,
};

pub struct Ce {}

impl Ce {}

impl EditorSpec for Ce {
    fn name() -> String {
        format!("ce")
    }

    fn initial_state() -> CoreEditorState {
        CoreEditorState::new(
            Expr::new(
                ExprLabel {
                    constructor: Constructor::Root,
                    diagnostic: Default::default(),
                },
                Span::empty(),
            ),
            Default::default(),
        )
    }

    fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption> {
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
                                label: ExprLabel::new(
                                    Constructor::Literal(query.clone()),
                                    Vec::new(),
                                ),
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

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Diagnostic> {
        Default::default()
    }

    fn is_valid_handle(_h: &Handle, _e: &EditorExpr) -> bool {
        // TODO: there's a bug in movement that enters infinite loop
        // match h {
        //     Handle::Point(p) => {
        //         let e = e.at_path(&p.path);
        //         e.label.constructor != Constructor::Newline
        //     }
        //     Handle::Span(h) => {
        //         let e = e.at_path(&h.path);
        //         e.label.constructor != Constructor::Newline
        //     }
        //     Handle::Zipper(h) => {
        //         let e = e.at_path(&h.path_o);
        //         if !(e.label.constructor != Constructor::Newline) {
        //             return false;
        //         }
        //         let e = e.at_path(&h.path_i());
        //         e.label.constructor != Constructor::Newline
        //     }
        // }
        true
    }

    fn assemble_rendered_expr(
        state: &mut EditorState<Self>,
        ui: &mut egui::Ui,
        path: &Path,
        _expr: &EditorExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        label: &String,
    ) {
        let color_scheme = editor::EditorState::<Ce>::color_scheme(ui);

        let selected = state.core.handle.contains_path(path);
        let fill_color = if selected {
            color_scheme.highlight_background
        } else {
            color_scheme.normal_background
        };
        let text_color = color_scheme.normal_text;

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(egui::Label::new(egui::RichText::new("(").color(text_color)).selectable(false));
        });

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(
                egui::Label::new(egui::RichText::new(format!("{}", label)).color(text_color))
                    .selectable(false),
            );
        });

        for (step, kid) in render_steps_and_kids.iter() {
            step.render(state, ui);
            if let Some(kid) = kid {
                kid.render(state, ui);
            }
        }

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(egui::Label::new(egui::RichText::new(")").color(text_color)).selectable(false));
        });
    }
}
