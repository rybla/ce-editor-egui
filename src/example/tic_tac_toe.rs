use crate::editor::*;
use crate::editor_ex;
use crate::expr::*;

#[derive(Default)]
pub struct Game {
    pub turn: bool,
    pub focus: Option<(usize, usize)>,
    pub grid: [[Cell; 3]; 3],
}

pub type Cell = Option<bool>;

impl Game {
    pub fn parse<D>(e: &GenEditorExpr<D>, h: &Handle) -> Option<Self> {
        fn parse_turn<D>(e: &GenEditorExpr<D>) -> Option<bool> {
            match e.pat_literal() {
                ("Turn", [x]) => match x.pat_literal() {
                    (t, []) => t.parse::<bool>().ok(),
                    _ => None,
                },
                _ => None,
            }
        }

        fn parse_grid<D>(e: &GenEditorExpr<D>) -> Option<[[Cell; 3]; 3]> {
            match e.pat_literal() {
                ("Grid", [a, b, c]) => {
                    let a = parse_row(a)?;
                    let b = parse_row(b)?;
                    let c = parse_row(c)?;
                    Some([a, b, c])
                }
                _ => None,
            }
        }

        fn parse_row<D>(e: &GenEditorExpr<D>) -> Option<[Cell; 3]> {
            match e.pat_literal() {
                ("Row", [a, b, c]) => {
                    let a = parse_cell(a)?;
                    let b = parse_cell(b)?;
                    let c = parse_cell(c)?;
                    Some([a, b, c])
                }
                _ => None,
            }
        }

        fn parse_cell<D>(e: &GenEditorExpr<D>) -> Option<Cell> {
            match e.pat_literal() {
                ("Cell", [a]) => match a.pat_pos_arg() {
                    [a] => match a.pat_literal() {
                        ("X", []) => Some(Some(true)),
                        ("O", []) => Some(Some(false)),
                        _ => None,
                    },
                    [] => Some(None),
                    _ => None,
                },
                _ => None,
            }
        }

        match (&e.label.constructor, e.kids.0.as_slice()) {
            (Constructor::Root, [e]) => match e.pat_literal() {
                ("Game", [turn, grid]) => {
                    let focus = match h {
                        Handle::Point(p) => match (p.path.0.as_slice(), p.i.0) {
                            ([Step(0), Step(1), i, j, Step(0)], 0) => Some((i.0, j.0)),
                            _ => None,
                        },
                        _ => None,
                    };
                    let turn = parse_turn(turn)?;
                    let grid = parse_grid(grid)?;
                    Some(Self { turn, focus, grid })
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn render(&self) -> (DiagExpr, Handle) {
        let cell = |i: usize, j: usize| -> Vec<DiagExpr> {
            self.grid[i][j].map_or(vec![], |x| {
                vec![GenEditorExpr::new_lit(
                    if x { "X".to_owned() } else { "O".to_owned() },
                    vec![],
                )]
            })
        };

        (
            DiagExpr::new(
                DiagEditorLabel::new(Constructor::Root, Default::default()),
                Span(vec![editor_ex!(
                    "Game",
                    [
                        editor_ex!(
                            "Turn",
                            [if self.turn {
                                editor_ex!("true", [])
                            } else {
                                editor_ex!("false", [])
                            }]
                        ),
                        editor_ex!(
                            "Grid",
                            [
                                editor_ex!(
                                    "Row",
                                    [
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(0, 0))]
                                        ),
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(0, 1))]
                                        ),
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(0, 2))]
                                        )
                                    ]
                                ),
                                editor_ex!(
                                    "Row",
                                    [
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(1, 0))]
                                        ),
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(1, 1))]
                                        ),
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(1, 2))]
                                        )
                                    ]
                                ),
                                editor_ex!(
                                    "Row",
                                    [
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(2, 0))]
                                        ),
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(2, 1))]
                                        ),
                                        editor_ex!(
                                            "Cell",
                                            [GenEditorExpr::new_pos_arg(cell(2, 2))]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]
                )]),
            ),
            match self.focus {
                Some((i, j)) => Handle::Point(Point::new(
                    Path(vec![Step(0), Step(1), Step(i), Step(j), Step(0)]),
                    Index(0),
                )),
                None => Handle::default(),
            },
        )
    }
}
pub struct Ttt {}

impl Ttt {}

impl EditorSpec for Ttt {
    fn name() -> String {
        "Tic Tac Toe".to_owned()
    }

    fn initial_state() -> PlainCoreState {
        PlainCoreState::default()
    }

    fn get_edits(state: &EditorState<Self>) -> Vec<EditMenuOption> {
        if let Some(ttt) = Game::parse(&state.core.root, &state.core.handle) {
            if ttt.turn && ttt.focus.is_some() {
                vec![EditMenuOption::new(
                    EditMenuPattern::Static("X".to_owned()),
                    |_query, mut state| {
                        if let Some(mut ttt) = Game::parse(&state.root, &state.handle) {
                            if let Some((i, j)) = ttt.focus {
                                //
                                ttt.grid[i][j] = Some(ttt.turn);
                                ttt.turn = !ttt.turn;
                                ttt.focus = None;
                                //
                                let (root, handle) = ttt.render();
                                state.root = root;
                                state.handle = handle;
                                //
                                Some(state)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    },
                )]
            } else if !ttt.turn && ttt.focus.is_some() {
                vec![EditMenuOption::new(
                    EditMenuPattern::Static("O".to_owned()),
                    |_query, mut state| {
                        if let Some(mut ttt) = Game::parse(&state.root, &state.handle) {
                            if let Some((i, j)) = ttt.focus {
                                //
                                ttt.grid[i][j] = Some(ttt.turn);
                                ttt.turn = !ttt.turn;
                                ttt.focus = None;
                                //
                                let (root, handle) = ttt.render();
                                state.root = root;
                                state.handle = handle;
                                //
                                Some(state)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    },
                )]
            } else {
                vec![]
            }
        } else {
            vec![EditMenuOption::new(
                EditMenuPattern::Static("start".to_owned()),
                |_query, mut state| {
                    //
                    state.root = DiagExpr::new(
                        DiagEditorLabel::new(Constructor::Root, Default::default()),
                        Span(vec![editor_ex!(
                            "Game",
                            [
                                editor_ex!("Turn", [editor_ex!("false", [])]),
                                editor_ex!(
                                    "Grid",
                                    [
                                        editor_ex!(
                                            "Row",
                                            [
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                ),
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                ),
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                )
                                            ]
                                        ),
                                        editor_ex!(
                                            "Row",
                                            [
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                ),
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                ),
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                )
                                            ]
                                        ),
                                        editor_ex!(
                                            "Row",
                                            [
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                ),
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                ),
                                                editor_ex!(
                                                    "Cell",
                                                    [GenEditorExpr::new_pos_arg(vec![])]
                                                )
                                            ]
                                        )
                                    ]
                                )
                            ]
                        )]),
                    );
                    Some(state)
                },
            )]
        }
    }

    fn diagnose(_state: &CoreState<MutDiagnostics>) {
        // todo!()
    }

    fn is_valid_handle_specialized(h: &Handle, root: &DiagExpr) -> bool {
        match h {
            Handle::Point(p) => {
                let e = root.get_subexpr(&p.path);
                match e.label.constructor {
                    Constructor::Root => true,
                    Constructor::PosArg => e.kids.0.is_empty(),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn assemble_rendered_expr(
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        rc: RenderContext<'_>,
        path: &Path,
        expr: &DiagExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        literal: &str,
    ) {
        let color_scheme = EditorState::<Self>::color_scheme(ui);

        match literal {
            "X" => {
                egui::Frame::new()
                    .fill(color_scheme.normal_background)
                    .show(ui, |ui| {
                        ui.add(egui::Label::new(
                            egui::RichText::new("X".to_owned())
                                .monospace()
                                .color(egui::Color32::RED),
                        ))
                    });
            }
            "O" => {
                egui::Frame::new()
                    .fill(color_scheme.normal_background)
                    .show(ui, |ui| {
                        ui.add(egui::Label::new(
                            egui::RichText::new("O".to_owned())
                                .monospace()
                                .color(egui::Color32::BLUE),
                        ))
                    });
            }
            "Game" => {
                ui.end_row();
                egui::Frame::new().show(ui, |ui| {
                    ui.add(egui::Label::new(
                        egui::RichText::new("Game:".to_owned()).underline(),
                    ))
                });
                ui.end_row();

                let RenderContext {
                    handle,
                    root,
                    color_scheme,
                    drag_origin,
                    menu,
                    actions,
                    interactive,
                    indent_level,
                } = rc;
                for (_, kid) in &render_steps_and_kids {
                    if let Some(kid) = kid {
                        kid.render::<Self>(
                            ctx,
                            ui,
                            // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                            RenderContext {
                                handle,
                                root,
                                color_scheme,
                                drag_origin,
                                menu,
                                actions,
                                interactive,
                                indent_level,
                            },
                        );
                        ui.end_row();
                    };
                }
            }
            "Turn" => {
                let turn = match render_steps_and_kids.first() {
                    Some((_, Some(turn))) => match turn.expr.pat_literal() {
                        (s, []) => match s.parse::<bool>() {
                            Ok(t) => format!("{t}"),
                            _ => "ERROR".to_owned(),
                        },
                        _ => "ERROR".to_owned(),
                    },
                    _ => "ERROR".to_owned(),
                };

                egui::Frame::new().show(ui, |ui| {
                    ui.add(egui::Label::new(
                        egui::RichText::new("Turn:".to_owned()).underline(),
                    ))
                });
                egui::Frame::new().show(ui, |ui| {
                    ui.add(egui::Label::new(egui::RichText::new(format!(" {turn}"))))
                });
                ui.end_row();
            }
            "Grid" => {
                egui::Frame::new().show(ui, |ui| {
                    ui.add(egui::Label::new(
                        egui::RichText::new("Grid:".to_owned()).underline(),
                    ))
                });
                ui.end_row();

                let RenderContext {
                    handle,
                    root,
                    color_scheme,
                    drag_origin,
                    menu,
                    actions,
                    interactive,
                    indent_level,
                } = rc;
                for (_, kid) in &render_steps_and_kids {
                    if let Some(kid) = kid {
                        kid.render::<Self>(
                            ctx,
                            ui,
                            // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                            RenderContext {
                                handle,
                                root,
                                color_scheme,
                                drag_origin,
                                menu,
                                actions,
                                interactive,
                                indent_level,
                            },
                        );
                        ui.end_row();
                    };
                }
            }
            "Row" => {
                let RenderContext {
                    handle,
                    root,
                    color_scheme,
                    drag_origin,
                    menu,
                    actions,
                    interactive,
                    indent_level,
                } = rc;
                for (_, kid) in &render_steps_and_kids {
                    if let Some(kid) = kid {
                        kid.render::<Self>(
                            ctx,
                            ui,
                            // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                            RenderContext {
                                handle,
                                root,
                                color_scheme,
                                drag_origin,
                                menu,
                                actions,
                                interactive,
                                indent_level,
                            },
                        );

                        egui::Frame::new()
                            .fill(color_scheme.normal_background)
                            .show(ui, |ui| {
                                ui.add(egui::Label::new(egui::RichText::new(" ".to_owned())))
                            });
                    };
                }
            }
            "Cell" => {
                let RenderContext {
                    handle,
                    root,
                    color_scheme,
                    drag_origin,
                    menu,
                    actions,
                    interactive,
                    indent_level,
                } = rc;
                for (_, kid) in &render_steps_and_kids {
                    if let Some(kid) = kid {
                        kid.render::<Self>(
                            ctx,
                            ui,
                            // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                            RenderContext {
                                handle,
                                root,
                                color_scheme,
                                drag_origin,
                                menu,
                                actions,
                                interactive,
                                indent_level,
                            },
                        );
                    };
                }
            }
            s => panic!("Unknown literal: {s}"),
        }
    }
}
