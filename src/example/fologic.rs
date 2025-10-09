use crate::{
    editor::{self, *}, expr::*
};

// (Name, Sort, kids' sorts)
const _PROP_GRAMMAR : &[(&str, &str, &[&str])]  = &[
    // Prop
    ("<", "Prop", &["Num", "Num"]),
    (">", "Prop", &["Num", "Num"]),
    (">=", "Prop", &["Num", "Num"]),
    (">=", "Prop", &["Num", "Num"]),
    ("=", "Prop", &["Num", "Num"]),
    ("And", "Prop", &["Prop", "Prop"]),
    ("Or", "Prop", &["Prop", "Prop"]),
    ("Arrow", "Prop", &["Prop", "Prop"]),
    ("Not", "Prop", &["Prop"]),
    ("Forall", "Prop", &["Var", "Prop"]),
    ("Exists", "Prop", &["Var", "Prop"]),
    // Num
    ("+", "Num", &["Num", "Num"]),
    ("-", "Num", &["Num", "Num"]),
    ("*", "Num", &["Num", "Num"]),
    ("/", "Num", &["Num", "Num"]),
    // TODO: but how can we add number literals if the labels are strings?
    // maybe we'll add these too:
    // ("abs", "Num", &["Num"]),
    // ("ceil", "Num", &["Num"]),
    // ("floor", "Num", &["Num"]),
];

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
        ]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Diagnostic> {
        Default::default()
    }

    fn is_valid_handle(_h: &Handle, _e: &EditorExpr) -> bool {
        true
    }

    // TODO, this is currently just copied from ce
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
