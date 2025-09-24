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
                    constructor: format!("label"),
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
                EditMenuPattern::Dynamic(|query| Some(query.clone())),
                insert_node,
            ),
            EditMenuOption::new(EditMenuPattern::Static(format!("copy")), |_query, state| {
                let frag = state.expr.get_fragment_at_handle(&state.handle)?;
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

fn insert_node(query: &String, state: &CoreEditorState<Ce>) -> Option<CoreEditorState<Ce>> {
    let state = state.clone();

    let (new_expr, new_handle) = state.expr.clone().insert_fragment_at_handle(
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
        state.handle,
    );
    Some(CoreEditorState {
        expr: new_expr,
        handle: new_handle,
        clipboard: state.clipboard,
    })
}
