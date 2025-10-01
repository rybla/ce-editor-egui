use crate::{editor::*, expr::*};

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

                    let handle = state.expr.insert_fragment_at_handle(
                        state.handle,
                        Fragment::Zipper(Zipper {
                            span_ol: Span::empty(),
                            span_or: Span::empty(),
                            middle: Context(vec![Tooth {
                                label: ExprLabel::new(query.clone(), vec![]),
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
        ui.label(egui::RichText::new(label.constructor.clone()))
    }
}
