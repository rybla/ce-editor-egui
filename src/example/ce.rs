use crate::{editor::*, expr_v1::*};

type C = String;
type D = Vec<String>;

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
                Default::default(),
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

                    let handle = state.expr.insert_fragment_at_handle(
                        state.handle,
                        Fragment::Zipper(Zipper {
                            outer_left: Span(vec![]),
                            outer_right: Span(vec![]),
                            inner: SpanContext {
                                outer: ExprContext(vec![]),
                                inner: SpanTooth {
                                    label: ExprLabel::new(query.clone(), vec![]),
                                    left: Span(vec![]),
                                    right: Span(vec![]),
                                },
                            },
                        }),
                    );

                    state.handle = handle;

                    Some(state)
                },
            ),
            EditMenuOption::new(EditMenuPattern::Static(format!("copy")), |_query, state| {
                println!("copy");

                // TODO: Eventually I can implement a different type of getter
                // that uses a reference to the expert and then just clones the
                // part that needs to be gotten. But for now, I'm content just
                // to clone the entire top-level expr first.
                let frag = state.expr.clone().get_fragment_at_handle(&state.handle)?;
                Some(CoreEditorState {
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
        ui.label(egui::RichText::new(label.constructor.clone()))
    }
}
