// use crate::{
//     editor::{self, *},
//     expr::*,
// };
// use log::trace;

// pub struct Ce {}

// impl Ce {}

// impl EditorSpec for Ce {
//     fn name() -> String {
//         "ce".to_owned()
//     }

//     fn initial_state() -> CoreEditorState {
//         CoreEditorState::new(
//             Expr::new(
//                 EditorLabel {
//                     constructor: Constructor::Root,
//                     metadata: Default::default(),
//                 },
//                 Span::empty(),
//             ),
//             Default::default(),
//         )
//     }

//     fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption> {
//         vec![
//             EditMenuOption::new(
//                 EditMenuPattern::Dynamic("<name> (constructor)".to_owned(), |query| {
//                     Some(format!("{query} (constructor)"))
//                 }),
//                 |query, state| {
//                     trace!(target: "ce", "insert_node: {query}");

//                     let mut state = state;

//                     let handle = state.expr.insert(
//                         state.handle,
//                         Fragment::Zipper(Zipper {
//                             span_ol: Span::empty(),
//                             span_or: Span::empty(),
//                             middle: Context(vec![Tooth {
//                                 label: EditorLabel::new(
//                                     Constructor::Literal(query.clone()),
//                                     Vec::new(),
//                                 ),
//                                 span_l: Span::empty(),
//                                 span_r: Span::empty(),
//                             }]),
//                         }),
//                     );

//                     state.handle = handle;

//                     Some(state)
//                 },
//             ),
//             EditMenuOption::new(
//                 EditMenuPattern::Static("copy".to_owned()),
//                 |_query, state| {
//                     println!("copy");
//                     state
//                         .expr
//                         .at_handle(&state.handle)
//                         .map(|frag| CoreEditorState {
//                             expr: state.expr.clone(),
//                             handle: state.handle.clone(),
//                             clipboard: Some(frag),
//                         })
//                 },
//             ),
//         ]
//     }

//     fn get_metadata(_state: EditorState<Self>) -> Vec<Diagnostic> {
//         Default::default()
//     }

//     fn is_valid_handle_specialized(_h: &Handle, _e: &EditorExpr) -> bool {
//         true
//     }

//     fn assemble_rendered_expr(
//         ctx: &egui::Context,
//         ui: &mut egui::Ui,
//         ren_ctx: &RenderContext,
//         state: &mut EditorState<Self>,
//         path: &Path,
//         _expr: &EditorExpr,
//         render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
//         label: &str,
//     ) {
//         let color_scheme = editor::EditorState::<Self>::color_scheme(ui);

//         let selected = state.core.handle.contains_path(path);
//         let fill_color = if selected {
//             color_scheme.highlight_background
//         } else {
//             color_scheme.normal_background
//         };
//         let text_color = color_scheme.normal_text;

//         egui::Frame::new().fill(fill_color).show(ui, |ui| {
//             ui.add(egui::Label::new(egui::RichText::new("(").color(text_color)).selectable(false));
//         });

//         egui::Frame::new().fill(fill_color).show(ui, |ui| {
//             ui.add(
//                 egui::Label::new(egui::RichText::new(label.to_owned()).color(text_color))
//                     .selectable(false),
//             );
//         });

//         for (step, kid) in &render_steps_and_kids {
//             step.render(ctx, ui, ren_ctx, state);
//             if let Some(kid) = kid {
//                 kid.render(ctx, ui, ren_ctx, state);
//             }
//         }

//         egui::Frame::new().fill(fill_color).show(ui, |ui| {
//             ui.add(egui::Label::new(egui::RichText::new(")").color(text_color)).selectable(false));
//         });
//     }
// }

// #[cfg(test)]
// mod tests {

//     #[test]
//     fn test_move1() {
//         todo!()
//     }
// }
