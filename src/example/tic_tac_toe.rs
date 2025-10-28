// use std::fmt::Display;

// use crate::editor::*;
// use crate::editor_ex;
// use crate::expr::*;

// #[derive(Default)]
// pub struct Game {
//     pub focus: Option<(usize, usize)>,
//     pub grid: [[Cell; 3]; 3],
// }

// pub type Cell = Option<bool>;

// #[derive(Default)]
// pub enum Winner {
//     #[default]
//     Unknown,
//     Tie,
//     Winner(bool),
// }

// impl Display for Winner {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Unknown => write!(f, "Unknown"),
//             Self::Tie => write!(f, "Tie"),
//             Self::Winner(false) => write!(f, "X"),
//             Self::Winner(true) => write!(f, "O"),
//         }
//     }
// }

// impl Game {
//     pub fn parse<D>(e: &GenEditorExpr<D>, h: &Handle) -> Result<Self, String> {
//         fn parse_grid<D>(e: &GenEditorExpr<D>) -> Result<[[Cell; 3]; 3], String> {
//             match e.pat_literal() {
//                 ("Grid", [a, b, c]) => {
//                     let a = parse_row(a)?;
//                     let b = parse_row(b)?;
//                     let c = parse_row(c)?;
//                     Ok([a, b, c])
//                 }
//                 (s, _) => Err(format!("expected Grid, but got unrecognized literal: {s}")),
//             }
//         }

//         fn parse_row<D>(e: &GenEditorExpr<D>) -> Result<[Cell; 3], String> {
//             match e.pat_literal() {
//                 ("Row", [a, b, c]) => {
//                     let a = parse_cell(a)?;
//                     let b = parse_cell(b)?;
//                     let c = parse_cell(c)?;
//                     Ok([a, b, c])
//                 }
//                 (s, _) => Err(format!("expected Row, but got unrecognized literal: {s}")),
//             }
//         }

//         fn parse_cell<D>(e: &GenEditorExpr<D>) -> Result<Cell, String> {
//             match e.pat_literal() {
//                 ("Cell", [a]) => match a.pat_pos_arg() {
//                     [a] => match a.pat_literal() {
//                         ("X", []) => Ok(Some(false)),
//                         ("O", []) => Ok(Some(true)),
//                         (s, _) => Err(format!("expected X|O, but got unrecognized literal: {s}")),
//                     },
//                     [] => Ok(None),
//                     _ => Err("expected Cell to have 0|1 arguments".to_owned()),
//                 },
//                 (s, _) => Err(format!("expected Cell, but got unrecognized literal: {s}")),
//             }
//         }

//         match (&e.label.constructor, e.kids.0.as_slice()) {
//             (Constructor::Root, [e]) => match e.pat_literal() {
//                 ("Game", [grid]) => {
//                     let focus = match h {
//                         Handle::Point(p) => match (p.path.0.as_slice(), p.i.0) {
//                             ([Step(0), Step(0), i, j, Step(0)], 0) => Some((i.0, j.0)),
//                             _ => None,
//                         },
//                         _ => None,
//                     };
//                     let grid = parse_grid(grid)?;
//                     Ok(Self { focus, grid })
//                 }
//                 (s, _) => Err(format!("expected Game, but got unrecognized literal: {s}")),
//             },
//             (s, _) => Err(format!(
//                 "expected Root, but got unrecognized constructor: {s}"
//             )),
//         }
//     }

//     pub fn render(&self) -> (DiagExpr, Handle) {
//         let cell = |i: usize, j: usize| -> Vec<DiagExpr> {
//             self.grid[i][j].map_or(vec![], |x| {
//                 vec![GenEditorExpr::new_lit(
//                     if !x { "X".to_owned() } else { "O".to_owned() },
//                     vec![],
//                 )]
//             })
//         };

//         (
//             DiagExpr::new(
//                 DiagEditorLabel::new(Constructor::Root, Default::default()),
//                 Span(vec![editor_ex!(
//                     "Game",
//                     [editor_ex!(
//                         "Grid",
//                         [
//                             editor_ex!(
//                                 "Row",
//                                 [
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(0, 0))]),
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(0, 1))]),
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(0, 2))])
//                                 ]
//                             ),
//                             editor_ex!(
//                                 "Row",
//                                 [
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(1, 0))]),
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(1, 1))]),
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(1, 2))])
//                                 ]
//                             ),
//                             editor_ex!(
//                                 "Row",
//                                 [
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(2, 0))]),
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(2, 1))]),
//                                     editor_ex!("Cell", [GenEditorExpr::new_pos_arg(cell(2, 2))])
//                                 ]
//                             )
//                         ]
//                     )]
//                 )]),
//             ),
//             match self.focus {
//                 Some((i, j)) => Handle::Point(Point::new(
//                     Path(vec![Step(0), Step(0), Step(i), Step(j), Step(0)]),
//                     Index(0),
//                 )),
//                 None => Handle::default(),
//             },
//         )
//     }

//     #[expect(unused_variables)]
//     fn get_winner(&self) -> Winner {
//         fn three(b: bool, c1: Cell, c2: Cell) -> bool {
//             Some(b) == c1 && Some(b) == c2
//         }

//         match self.grid {
//             // rows
//             [[Some(b00), b01, b02], [b10, b11, b12], [b20, b21, b22]] if three(b00, b01, b02) => {
//                 Winner::Winner(b00)
//             }
//             [[b00, b01, b02], [Some(b10), b11, b12], [b20, b21, b22]] if three(b10, b11, b12) => {
//                 Winner::Winner(b10)
//             }
//             [[b00, b01, b02], [b10, b11, b12], [Some(b20), b21, b22]] if three(b20, b21, b22) => {
//                 Winner::Winner(b20)
//             }
//             // columns
//             [[Some(b00), b01, b02], [b10, b11, b12], [b20, b21, b22]] if three(b00, b10, b20) => {
//                 Winner::Winner(b00)
//             }
//             [[b00, Some(b01), b02], [b10, b11, b12], [b20, b21, b22]] if three(b01, b11, b21) => {
//                 Winner::Winner(b01)
//             }
//             [[b00, b01, Some(b02)], [b10, b11, b12], [b20, b21, b22]] if three(b02, b12, b22) => {
//                 Winner::Winner(b02)
//             }
//             // diagnonals
//             [[Some(b00), b01, b02], [b10, b11, b12], [b20, b21, b22]] if three(b00, b11, b22) => {
//                 Winner::Winner(b00)
//             }
//             [[b00, b01, Some(b02)], [b10, b11, b12], [b20, b21, b22]] if three(b02, b11, b22) => {
//                 Winner::Winner(b02)
//             }
//             // tie
//             rows if rows.iter().all(|row| row.iter().all(|cell| cell.is_some())) => Winner::Tie,
//             // unknown
//             _ => Winner::Unknown,
//         }
//     }

//     fn get_turn(&self) -> bool {
//         println!("get_turn: grid: {:?}", self.grid);
//         let count = self
//             .grid
//             .iter()
//             .map(|row| {
//                 row.iter()
//                     .map(|cell| match cell {
//                         Some(false) => 1,
//                         Some(true) => -1,
//                         None => 0,
//                     })
//                     .sum::<i8>()
//             })
//             .sum::<i8>();
//         println!("count: {count}");
//         count != 0
//     }
// }

// pub struct Ttt {}

// impl Ttt {}

// impl EditorSpec for Ttt {
//     fn name() -> String {
//         "Tic Tac Toe".to_owned()
//     }

//     fn initial_state() -> PlainCoreState {
//         PlainCoreState::default()
//     }

//     fn get_edits(state: &EditorState<Self>) -> Vec<EditMenuOption> {
//         match Game::parse(&state.core.root, &state.core.handle) {
//             Ok(game) => {
//                 if matches!(game.get_winner(), Winner::Tie | Winner::Winner(_)) {
//                     vec![EditMenuOption::new(
//                         EditMenuPattern::Static("restart".to_owned()),
//                         |_query, mut state| {
//                             let game = Game::default();
//                             let (root, handle) = game.render();
//                             state.root = root;
//                             state.handle = handle;
//                             Some(state)
//                         },
//                     )]
//                 } else if game.focus.is_some() {
//                     if !game.get_turn() {
//                         vec![EditMenuOption::new(
//                             EditMenuPattern::Static("X".to_owned()),
//                             |_query, mut state| {
//                                 if let Ok(mut game) = Game::parse(&state.root, &state.handle) {
//                                     if let Some((i, j)) = game.focus {
//                                         //
//                                         game.grid[i][j] = Some(false);
//                                         game.focus = None;
//                                         //
//                                         let (root, handle) = game.render();
//                                         state.root = root;
//                                         state.handle = handle;
//                                         //
//                                         Some(state)
//                                     } else {
//                                         None
//                                     }
//                                 } else {
//                                     None
//                                 }
//                             },
//                         )]
//                     } else {
//                         vec![EditMenuOption::new(
//                             EditMenuPattern::Static("O".to_owned()),
//                             |_query, mut state| {
//                                 if let Ok(mut game) = Game::parse(&state.root, &state.handle) {
//                                     if let Some((i, j)) = game.focus {
//                                         //
//                                         game.grid[i][j] = Some(true);
//                                         game.focus = None;
//                                         //
//                                         let (root, handle) = game.render();
//                                         state.root = root;
//                                         state.handle = handle;
//                                         //
//                                         Some(state)
//                                     } else {
//                                         None
//                                     }
//                                 } else {
//                                     None
//                                 }
//                             },
//                         )]
//                     }
//                 } else {
//                     vec![]
//                 }
//             }
//             Err(e) => {
//                 println!("Error parsing game state: {e}");
//                 vec![EditMenuOption::new(
//                     EditMenuPattern::Static("start".to_owned()),
//                     |_query, mut state| {
//                         let game = Game::default();
//                         let (root, handle) = game.render();
//                         state.root = root;
//                         state.handle = handle;
//                         Some(state)
//                     },
//                 )]
//             }
//         }
//     }

//     fn diagnose(state: &CoreState<MutDiagnostics>) {
//         if let Ok(game) = Game::parse(&state.root, &state.handle) {
//             let winner = game.get_winner();
//             let turn = game.get_turn();

//             let game_expr = &state.root.kids.0[0];
//             let mut ds = game_expr.label.diagnostics.0.take();
//             ds.push(Diagnostic(format!("winner: {winner}")));
//             ds.push(Diagnostic(format!(
//                 "turn: {}",
//                 if !turn { "X" } else { "O" }
//             )));
//             game_expr.label.diagnostics.0.set(ds);
//         }
//     }

//     fn is_valid_handle_specialized(h: &Handle, root: &DiagExpr) -> bool {
//         match h {
//             Handle::Point(p) => {
//                 let e = root.get_subexpr(&p.path);
//                 match e.label.constructor {
//                     Constructor::Root => true,
//                     Constructor::PosArg => e.kids.0.is_empty(),
//                     _ => false,
//                 }
//             }
//             _ => false,
//         }
//     }

//     fn assemble_rendered_expr(
//         ctx: &egui::Context,
//         ui: &mut egui::Ui,
//         rc: RenderContext<'_>,
//         _path: &Path,
//         _expr: &DiagExpr,
//         render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
//         literal: &str,
//     ) {
//         let color_scheme = EditorState::<Self>::color_scheme(ui);

//         match literal {
//             "X" => {
//                 egui::Frame::new()
//                     .fill(color_scheme.normal_background)
//                     .show(ui, |ui| {
//                         ui.add(egui::Label::new(
//                             egui::RichText::new("X".to_owned())
//                                 .monospace()
//                                 .color(egui::Color32::RED),
//                         ))
//                     });
//             }
//             "O" => {
//                 egui::Frame::new()
//                     .fill(color_scheme.normal_background)
//                     .show(ui, |ui| {
//                         ui.add(egui::Label::new(
//                             egui::RichText::new("O".to_owned())
//                                 .monospace()
//                                 .color(egui::Color32::BLUE),
//                         ))
//                     });
//             }
//             "Game" => {
//                 ui.end_row();
//                 egui::Frame::new().show(ui, |ui| {
//                     ui.add(egui::Label::new(
//                         egui::RichText::new("Game:".to_owned()).underline(),
//                     ))
//                 });
//                 ui.end_row();

//                 let RenderContext {
//                     handle,
//                     root,
//                     color_scheme,
//                     drag_origin,
//                     menu,
//                     actions,
//                     interactive,
//                     indent_level,
//                 } = rc;
//                 for (_, kid) in &render_steps_and_kids {
//                     if let Some(kid) = kid {
//                         kid.render::<Self>(
//                             ctx,
//                             ui,
//                             // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
//                             RenderContext {
//                                 handle,
//                                 root,
//                                 color_scheme,
//                                 drag_origin,
//                                 menu,
//                                 actions,
//                                 interactive,
//                                 indent_level,
//                             },
//                         );
//                         ui.end_row();
//                     };
//                 }
//             }
//             "Winner" => {
//                 let winner = match render_steps_and_kids.first() {
//                     Some((_, Some(winner))) => match winner.expr.pat_literal() {
//                         (s, []) => s.to_owned(),
//                         _ => "ERROR".to_owned(),
//                     },
//                     _ => "ERROR".to_owned(),
//                 };

//                 egui::Frame::new().show(ui, |ui| {
//                     ui.add(egui::Label::new(
//                         egui::RichText::new("Winner:".to_owned()).underline(),
//                     ))
//                 });
//                 egui::Frame::new().show(ui, |ui| {
//                     ui.add(egui::Label::new(egui::RichText::new(format!(" {winner}"))))
//                 });
//                 ui.end_row();
//             }
//             "Turn" => {
//                 let turn = match render_steps_and_kids.first() {
//                     Some((_, Some(turn))) => match turn.expr.pat_literal() {
//                         (s, []) => s.to_owned(),
//                         _ => "ERROR".to_owned(),
//                     },
//                     _ => "ERROR".to_owned(),
//                 };

//                 egui::Frame::new().show(ui, |ui| {
//                     ui.add(egui::Label::new(
//                         egui::RichText::new("Turn:".to_owned()).underline(),
//                     ))
//                 });
//                 egui::Frame::new().show(ui, |ui| {
//                     ui.add(egui::Label::new(egui::RichText::new(format!(" {turn}"))))
//                 });
//                 ui.end_row();
//             }
//             "Grid" => {
//                 egui::Frame::new().show(ui, |ui| {
//                     ui.add(egui::Label::new(
//                         egui::RichText::new("Grid:".to_owned()).underline(),
//                     ))
//                 });
//                 ui.end_row();

//                 let RenderContext {
//                     handle,
//                     root,
//                     color_scheme,
//                     drag_origin,
//                     menu,
//                     actions,
//                     interactive,
//                     indent_level,
//                 } = rc;
//                 for (_, kid) in &render_steps_and_kids {
//                     if let Some(kid) = kid {
//                         kid.render::<Self>(
//                             ctx,
//                             ui,
//                             // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
//                             RenderContext {
//                                 handle,
//                                 root,
//                                 color_scheme,
//                                 drag_origin,
//                                 menu,
//                                 actions,
//                                 interactive,
//                                 indent_level,
//                             },
//                         );
//                         ui.end_row();
//                     };
//                 }
//             }
//             "Row" => {
//                 let RenderContext {
//                     handle,
//                     root,
//                     color_scheme,
//                     drag_origin,
//                     menu,
//                     actions,
//                     interactive,
//                     indent_level,
//                 } = rc;
//                 for (_, kid) in &render_steps_and_kids {
//                     if let Some(kid) = kid {
//                         kid.render::<Self>(
//                             ctx,
//                             ui,
//                             // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
//                             RenderContext {
//                                 handle,
//                                 root,
//                                 color_scheme,
//                                 drag_origin,
//                                 menu,
//                                 actions,
//                                 interactive,
//                                 indent_level,
//                             },
//                         );

//                         egui::Frame::new()
//                             .fill(color_scheme.normal_background)
//                             .show(ui, |ui| {
//                                 ui.add(egui::Label::new(egui::RichText::new(" ".to_owned())))
//                             });
//                     };
//                 }
//             }
//             "Cell" => {
//                 let RenderContext {
//                     handle,
//                     root,
//                     color_scheme,
//                     drag_origin,
//                     menu,
//                     actions,
//                     interactive,
//                     indent_level,
//                 } = rc;
//                 for (_, kid) in &render_steps_and_kids {
//                     if let Some(kid) = kid {
//                         kid.render::<Self>(
//                             ctx,
//                             ui,
//                             // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
//                             RenderContext {
//                                 handle,
//                                 root,
//                                 color_scheme,
//                                 drag_origin,
//                                 menu,
//                                 actions,
//                                 interactive,
//                                 indent_level,
//                             },
//                         );
//                     };
//                 }
//             }
//             s => panic!("Unknown literal: {s}"),
//         }
//     }
// }
