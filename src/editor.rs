use crate::{ex, expr::*, span};
use egui;
use lazy_static::lazy_static;
use log::trace;
use nucleo_matcher::Matcher;
use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
};

pub const INDENT_WIDTH_EM: f32 = 1.0;

#[derive(Clone)]
pub struct ColorScheme {
    pub normal_border: egui::Color32,
    pub normal_text: egui::Color32,
    pub ghost_text: egui::Color32,
    pub keyword_text: egui::Color32,
    pub accent_text: egui::Color32,
    pub normal_background: egui::Color32,
    pub active_text: egui::Color32,
    pub active_background: egui::Color32,
    pub inactive_text: egui::Color32,
    pub inactive_background: egui::Color32,
    pub highlight_background: egui::Color32,
}

lazy_static! {
    pub static ref dark_color_scheme: ColorScheme = ColorScheme {
        accent_text: egui::Color32::RED,
        keyword_text: egui::Color32::BLUE,
        normal_text: egui::Color32::WHITE,
        ghost_text: egui::Color32::GRAY,
        normal_background: egui::Color32::BLACK,
        normal_border: egui::Color32::WHITE,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::BLUE,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::DARK_BLUE,
        highlight_background: egui::Color32::DARK_BLUE,
    };
    pub static ref light_color_scheme: ColorScheme = ColorScheme {
        accent_text: egui::Color32::RED,
        keyword_text: egui::Color32::BLUE,
        normal_text: egui::Color32::BLACK,
        ghost_text: egui::Color32::GRAY,
        normal_background: egui::Color32::WHITE,
        normal_border: egui::Color32::BLACK,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::BLUE,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::LIGHT_BLUE,
        highlight_background: egui::Color32::LIGHT_BLUE,
    };
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ExprLabel {
    pub constructor: Constructor,
    pub diagnostic: Vec<Diagnostic>,
}

impl Display for ExprLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constructor)
    }
}

impl ExprLabel {
    pub fn new(constructor: Constructor, diagnostic: Vec<Diagnostic>) -> Self {
        Self {
            constructor,
            diagnostic,
        }
    }
}

pub type EditorExpr = Expr<ExprLabel>;

pub struct EditorState<ES: EditorSpec + ?Sized> {
    pub spec: PhantomData<ES>,
    pub core: CoreEditorState,
    pub menu: Option<EditMenu>,
    pub history: Vec<CoreEditorState>,
    pub future: Vec<CoreEditorState>,
    pub drag_origin: Option<Handle>,
}

impl<ES: EditorSpec + ?Sized> Default for EditorState<ES> {
    fn default() -> Self {
        Self {
            spec: PhantomData,
            core: ES::initial_state(),
            menu: Default::default(),
            history: vec![],
            future: vec![],
            drag_origin: Default::default(),
        }
    }
}

impl<ES: EditorSpec + ?Sized> EditorState<ES> {
    pub fn escape(&mut self) {
        if self.menu.is_some() {
            self.menu = None;
        } else {
            // TODO: unify this into Action enumeration
            self.core
                .handle
                .escape()
                .unwrap_or_else(|e| trace!(target: "editor.move", "escape failed: {e:?}"));
        }
    }

    pub fn update(&mut self, ctx: &egui::Context) {
        trace!(target: "editor.update", "update");

        if self.drag_origin.is_some() && ctx.input(|i| i.pointer.primary_released()) {
            trace!(target: "editor.drag", "end drag; h = {}", self.core.handle);
            self.drag_origin = None;
        }

        if false {
            // this is just a placeholder so the subsequent branches can all use `else if` syntax
        }
        // escape
        else if ctx.input(|i| i.key_pressed(egui::Key::Escape)) {
            self.escape();
        }
        // when edit menu is open
        else if let Some(menu) = &mut self.menu {
            if false {
            }
            // submit menu option
            else if ctx
                .input(|i| i.key_pressed(egui::Key::Tab) || i.key_pressed(egui::Key::Enter))
            {
                if let Some(option) = menu.focus_option() {
                    // TODO: this still seems problematic that we are cloning the entire core state for every single update
                    if let Some(core) = (option.edit)(&menu.query, self.core.clone()) {
                        self.do_action(Action::SetCore(core));
                    } else {
                        trace!(target: "editor.edit", "edit failed");
                    }
                } else {
                    trace!(target: "editor.edit", "there are no edit options available");
                }
            }
            // move menu option
            else if let Some(dir) = match_input_cycle_dir(ctx) {
                match dir {
                    CycleDir::Prev => menu.index -= 1,
                    CycleDir::Next => menu.index += 1,
                }
                menu.update(false);
            }
        }
        // open edit menu
        else if ctx.input(|i| i.key_pressed(egui::Key::Space)) {
            trace!(target: "editor.edit", "open EditMenu");
            let menu = ES::get_edits(self);
            self.menu = Some(EditMenu::new(menu));
        }
        // undo
        else if ctx.input(|i| i.key_pressed(egui::Key::Z)) {
            self.do_action(Action::Undo);
        }
        // redo
        else if ctx.input(|i| i.key_pressed(egui::Key::R)) {
            self.do_action(Action::Redo);
        }
        // delete
        else if ctx
            .input(|i| i.key_pressed(egui::Key::Delete) || i.key_pressed(egui::Key::Backspace))
        {
            self.do_action(Action::Delete);
        }
        // cut
        else if ctx.input(|i| i.key_pressed(egui::Key::X)) {
            self.do_action(Action::Cut);
        }
        // copy
        else if ctx.input(|i| i.key_pressed(egui::Key::C)) {
            self.do_action(Action::Copy);
        }
        // paste
        else if ctx.input(|i| i.key_pressed(egui::Key::V)) {
            self.do_action(Action::Paste);
        }
        // newline
        else if ctx.input(|i| i.key_pressed(egui::Key::Enter)) {
            let mut core = self.core.clone();
            let h = core.expr.insert(
                core.handle,
                Fragment::Span(span![ex![ExprLabel::new(Constructor::Newline, vec![]), []]]),
            );
            core.handle = h;
            self.do_action(Action::SetCore(core));
        }
        // rotate focus
        else if ctx.input(|i| i.modifiers.command_only())
            && let Some(dir) = match_input_move_dir(ctx)
        {
            self.core.handle.rotate_focus_dir(&dir);
        }
        // select
        else if ctx.input(|i| i.modifiers.shift)
            && let Some(dir) = match_input_move_dir(ctx)
        {
            let mut target = self.core.handle.focus_point();
            loop {
                let move_status = target.move_dir(&self.core.expr, &dir);
                if move_status.is_err() {
                    trace!(target: "editor.move", "select bailed since a move failed");
                    break;
                }

                if let Some(h) = self.core.handle.clone().drag(&self.core.expr, &target)
                    && ES::is_valid_handle(&h, &self.core.expr)
                {
                    self.do_action(Action::SetHandle(SetHandle {
                        handle: h,
                        snapshot: false,
                    }));
                    break;
                }
            }
        }
        // move
        else if let Some(dir) = match_input_move_dir(ctx) {
            let mut origin = self.core.handle.clone();
            #[expect(unused_assignments)]
            let mut move_status = Ok(());
            loop {
                move_status = origin.move_dir(&self.core.expr, &dir);
                if let Err(err) = &move_status {
                    trace!(target: "editor.move", "move bailed since a move failed: {err:?}");
                    break;
                }

                if ES::is_valid_handle(&origin, &self.core.expr) {
                    break;
                }
            }
            if move_status.is_ok() {
                trace!(target: "editor.move", "move succeeded: {origin}");
                self.do_action(Action::SetHandle(SetHandle {
                    handle: origin.clone(),
                    snapshot: false,
                }));
            }
        }
        // move up
        else if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            let mut origin = self.core.handle.clone();
            loop {
                let move_status = origin.move_up(&self.core.expr);
                if move_status.is_err() {
                    trace!(target: "editor.move", "move bailed since a move failed");
                    break;
                }

                if ES::is_valid_handle_specialized(&self.core.handle, &self.core.expr) {
                    break;
                }
            }
            self.do_action(Action::SetHandle(SetHandle {
                handle: origin.clone(),
                snapshot: false,
            }));
        }
    }

    pub fn color_scheme(ui: &egui::Ui) -> &'static ColorScheme {
        match ui.ctx().theme() {
            egui::Theme::Dark => &dark_color_scheme,
            egui::Theme::Light => &light_color_scheme,
        }
    }

    pub fn render(&mut self, ctx: &egui::Context, ui: &mut egui::Ui) {
        egui::Frame::new().show(ui, |ui| {
            egui::ScrollArea::both().show(ui, |ui| {
                ui.with_layout(
                    egui::Layout::left_to_right(egui::Align::TOP).with_main_wrap(true),
                    |ui| {
                        ui.spacing_mut().item_spacing = egui::Vec2::ZERO;
                        ui.set_row_height(ui.text_style_height(&egui::TextStyle::Body));

                        self.render_expr(
                            ctx,
                            ui,
                            &RenderContext {
                                interactive: true,
                                indent_level: 0,
                            },
                            &Path::empty(),
                            &self.core.expr.clone(),
                        );
                    },
                )
            })
        });
    }

    pub fn render_point(
        &mut self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        ren_ctx: &RenderContext,
        p: &Point,
    ) {
        let handle_at_point = Handle::Point(p.clone());
        let is_valid_handle = ES::is_valid_handle(&handle_at_point, &self.core.expr);

        if is_valid_handle {
            let color_scheme = Self::color_scheme(ui);

            let is_handle = p == &self.core.handle.focus_point();

            let is_at_handle = match &self.core.handle {
                Handle::Point(handle) => p == handle,
                Handle::Span(handle) => p == &handle.p_l() || p == &handle.p_r(),
                Handle::Zipper(handle) => {
                    p == &handle.p_ol()
                        || p == &handle.p_or()
                        || p == &handle.p_il()
                        || p == &handle.p_ir()
                }
            };

            let is_in_handle = self.core.handle.contains_point(p);

            let fill_color = if is_handle {
                color_scheme.active_background
            } else if is_at_handle {
                color_scheme.inactive_background
            } else if is_in_handle {
                color_scheme.highlight_background
            } else {
                color_scheme.normal_background
            };

            egui::Frame::new().fill(fill_color).show(ui, |ui| {
                let label = ui.add(
                    egui::Label::new(
                        egui::RichText::new("•".to_owned()).color(color_scheme.ghost_text),
                    )
                    .selectable(false)
                    .sense(egui::Sense::click_and_drag()),
                );

                if ren_ctx.interactive {
                    if label.clicked() {
                        trace!(target: "editor", "click at point: p = {p}");
                        let h = Handle::Point(p.clone());
                        self.do_action(Action::SetHandle(SetHandle {
                            handle: h,
                            snapshot: false,
                        }));
                    }

                    if label.drag_started_by(egui::PointerButton::Primary) {
                        trace!(target: "editor.drag", "drag start at point: p = {p}");
                        self.do_action(Action::SetHandle(SetHandle {
                            handle: Handle::Point(p.clone()),
                            snapshot: false,
                        }));
                        self.drag_origin = Some(Handle::Point(p.clone()));
                    } else if let Some(drag_origin) = &self.drag_origin
                        && let Some(pointer_pos) = ctx.pointer_latest_pos()
                        && label.rect.contains(pointer_pos)
                    {
                        trace!(target: "editor.drag", "drag hover at point: p = {p}");
                        if let Some(h) = drag_origin.clone().drag(&self.core.expr, p) {
                            self.do_action(Action::SetHandle(SetHandle {
                                handle: h.clone(),
                                snapshot: false,
                            }));
                        }
                    }
                }

                if is_handle && let Some(menu) = &mut self.menu {
                    let menu_width = 100f32;
                    let menu_query_height = ui.text_style_height(&egui::TextStyle::Body);
                    let menu_options_height = 80f32;
                    let menu_height = menu_query_height + menu_options_height;

                    // render edit menu stuff
                    egui::Frame::new()
                        .fill(color_scheme.normal_background)
                        .outer_margin(egui::Margin {
                            left: 0i8,
                            right: 5i8,
                            top: 0i8,
                            bottom: 5i8,
                        })
                        .show(ui, |ui| {
                            ui.with_layout(
                                egui::Layout::top_down_justified(egui::Align::TOP),
                                |ui| {
                                    ui.set_width(menu_width);
                                    ui.set_height(menu_height);
                                    ui.spacing_mut().item_spacing = egui::Vec2::ZERO;

                                    // query
                                    egui::Frame::new().show(ui, |ui| {
                                        ui.set_height(menu_query_height);

                                        // TODO: prevent default behavior on ArrowUp and ArrowDown, since that controls menu cycling
                                        let textedit = egui::TextEdit::singleline(&mut menu.query)
                                            .hint_text("query edit menu")
                                            .desired_width(menu_width) // 100f32 is a fine width for now
                                            .cursor_at_end(true);
                                        let textedit_response = ui.add(textedit);
                                        // on change, update menu
                                        if textedit_response.changed() {
                                            menu.update(true);
                                        }
                                        // when the menu is open, the query should always have focus
                                        if !textedit_response.has_focus() {
                                            textedit_response.request_focus();
                                        }
                                    });

                                    ui.end_row();

                                    // options
                                    egui::Frame::new().show(ui, |ui| {
                                        ui.set_height(menu_options_height);

                                        let menu_index = menu.index;
                                        let matched_items = menu.matched_items();
                                        let matched_items_count = matched_items.len();
                                        let focus_index = if matched_items_count == 0 {
                                            None
                                        } else {
                                            Some(menu_index.rem_euclid(matched_items_count as i8)
                                                as usize)
                                        };
                                        let requested_scroll_to_option =
                                            menu.requested_scroll_to_option;

                                        egui::ScrollArea::vertical().show(ui, |ui| {
                                            ui.with_layout(
                                                egui::Layout::left_to_right(egui::Align::TOP)
                                                    .with_main_wrap(true),
                                                |ui| {
                                                    ui.spacing_mut().item_spacing =
                                                        egui::Vec2::ZERO;
                                                    ui.set_row_height(
                                                        ui.text_style_height(
                                                            &egui::TextStyle::Body,
                                                        ),
                                                    );

                                                    let matched_items = menu.matched_items();

                                                    // let mut responses = vec![];
                                                    let mut focus_response = None;

                                                    for (i, (label, _item)) in
                                                        matched_items.iter().enumerate()
                                                    {
                                                        let focused = Some(i) == focus_index;

                                                        let frame =
                                                            egui::Frame::new().show(ui, |ui| {
                                                                ui.set_width(menu_width);
                                                                ui.with_layout(
                                                        egui::Layout::left_to_right(
                                                            egui::Align::TOP,
                                                        )
                                                        .with_main_wrap(true),
                                                        |ui| {
                                                            ui.spacing_mut().item_spacing =
                                                                egui::Vec2::ZERO;
                                                            ui.set_row_height(
                                                                ui.text_style_height(
                                                                    &egui::TextStyle::Body,
                                                                ),
                                                            );

                                                            egui::Frame::new().show(ui, |ui| {
                                                                ui.set_width(10.0f32);
                                                                ui.vertical_centered(|ui| {
                                                                    if focused {
                                                                        ui.add(
                                                                            egui::Label::new(
                                                                                egui::RichText::new(
                                                                                    "•".to_owned(),
                                                                                ),
                                                                            )
                                                                            .selectable(false),
                                                                        );
                                                                    }
                                                                });
                                                            });

                                                            egui::Frame::new().show(ui, |ui| {
                                                                ui.add(
                                                                    egui::Label::new(
                                                                        egui::RichText::new(
                                                                            label.clone(),
                                                                        ),
                                                                    )
                                                                    .selectable(false),
                                                                );
                                                            })
                                                        },
                                                    );
                                                            });

                                                        if requested_scroll_to_option && focused {
                                                            focus_response = Some(frame.response);
                                                        }

                                                        ui.end_row();
                                                    }

                                                    if let Some(focus_response) = focus_response {
                                                        focus_response
                                                            .scroll_to_me(Some(egui::Align::TOP));
                                                    }
                                                    menu.requested_scroll_to_option = false;
                                                },
                                            );
                                        });
                                    })
                                },
                            );
                        });
                }
            });
        }
    }

    pub fn render_expr(
        &mut self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        ren_ctx: &RenderContext,
        path: &Path,
        expr: &EditorExpr,
    ) {
        self.render_expr_contents(ctx, ui, ren_ctx, path, expr);
    }

    pub fn render_expr_contents(
        &mut self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        ren_ctx: &RenderContext,
        path: &Path,
        expr: &EditorExpr,
    ) {
        let color_scheme = Self::color_scheme(ui);

        let selected = self.core.handle.contains_path(path);
        let fill_color = if selected {
            color_scheme.highlight_background
        } else {
            color_scheme.normal_background
        };

        let mut render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)> = vec![];

        for (s, e) in expr.kids.steps_and_kids() {
            render_steps_and_kids.push((
                RenderPoint {
                    path,
                    i: s.left_index(),
                },
                Some(RenderExpr {
                    expr: e,
                    path: path.clone().append(Path(vec![s])),
                }),
            ));
        }

        render_steps_and_kids.push((
            RenderPoint {
                path,
                i: expr.kids.rightmost_index(),
            },
            None,
        ));

        let literal_kids_count = render_steps_and_kids
            .iter()
            .filter(|(_, o_re)| match o_re {
                Some(re) => matches!(re.expr.label.constructor, Constructor::Literal(_)),
                None => false,
            })
            .collect::<Vec<_>>()
            .len();

        match &expr.label.constructor {
            Constructor::Literal(literal) => ES::assemble_rendered_expr(
                ctx,
                ui,
                ren_ctx,
                self,
                path,
                expr,
                render_steps_and_kids,
                literal,
            ),
            Constructor::Root => {
                for (step, kid) in &render_steps_and_kids {
                    step.render(ctx, ui, ren_ctx, self);
                    if let Some(kid) = kid {
                        kid.render(ctx, ui, ren_ctx, self);
                    }
                }
            }
            Constructor::Newline => {
                ui.end_row();
                egui::Frame::new().show(ui, |ui| {
                    ui.set_width(
                        (std::cmp::max(path.0.len(), 1) - 1) as f32
                            * INDENT_WIDTH_EM
                            * ui.text_style_height(&egui::TextStyle::Body),
                    );
                });
            }
            Constructor::PosArg if literal_kids_count == 1 => {
                for (step, kid) in &render_steps_and_kids {
                    step.render(ctx, ui, ren_ctx, self);
                    if let Some(kid) = kid {
                        kid.render(ctx, ui, ren_ctx, self);
                    }
                }
            }
            Constructor::PosArg => {
                egui::Frame::new().fill(fill_color).show(ui, |ui| {
                    ui.add(
                        egui::Label::new(egui::RichText::new("[").color(color_scheme.accent_text))
                            .selectable(false),
                    );
                });

                for (step, kid) in &render_steps_and_kids {
                    step.render(ctx, ui, ren_ctx, self);
                    if let Some(kid) = kid {
                        kid.render(ctx, ui, ren_ctx, self);
                    }
                }

                egui::Frame::new().fill(fill_color).show(ui, |ui| {
                    ui.add(
                        egui::Label::new(egui::RichText::new("]").color(color_scheme.accent_text))
                            .selectable(false),
                    );
                });
            }
        }
    }

    pub fn snapshot(&mut self) {
        self.history.push(self.core.clone());
        self.future = vec![];
    }

    pub fn do_action(&mut self, action: Action) {
        match action {
            Action::Redo => {
                if let Some(core) = self.future.pop() {
                    self.history.push(self.core.clone());
                    self.menu = None;
                    self.core = core;
                }
            }
            Action::Undo => {
                if let Some(core) = self.history.pop() {
                    self.future.push(self.core.clone());
                    self.menu = None;
                    self.core = core;
                }
            }
            Action::Copy => {
                if let Some(frag) = self.core.expr.at_handle_cloned(&self.core.handle) {
                    self.snapshot();
                    self.core.clipboard = Some(frag);
                }
            }
            Action::Paste => {
                if let Some(frag) = self.core.clipboard.clone() {
                    self.snapshot();
                    self.menu = None;
                    let handle = self.core.expr.insert(self.core.handle.clone(), frag);
                    self.core.handle = handle;
                }
            }
            Action::Cut => {
                self.snapshot();
                if let Some((frag, h)) = self.core.expr.cut(&self.core.handle) {
                    self.menu = None;
                    self.core.clipboard = Some(frag);
                    self.core.handle = h;
                }
            }
            Action::Delete => {
                self.snapshot();
                if let Some((_frag, h)) = self.core.expr.cut(&self.core.handle) {
                    self.menu = None;
                    self.core.handle = h;
                }
            }
            Action::SetHandle(args) => {
                if args.snapshot {
                    self.snapshot();
                }
                self.menu = None;
                self.core.handle = args.handle;
            }
            Action::SetCore(core) => {
                self.snapshot();
                self.menu = None;
                self.core = core;
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CoreEditorState {
    pub expr: EditorExpr,
    pub handle: Handle,
    pub clipboard: Option<Fragment<ExprLabel>>,
}

impl CoreEditorState {
    pub fn new(expr: EditorExpr, handle: Handle) -> Self {
        Self {
            expr,
            handle,
            clipboard: None,
        }
    }
}

pub struct EditMenu {
    pub query: String,
    pub all_options: Vec<EditMenuOption>,
    pub index: i8,
    pub matcher: nucleo_matcher::Matcher,
    pub requested_scroll_to_option: bool,
}

impl EditMenu {
    fn new(all_options: Vec<EditMenuOption>) -> Self {
        Self {
            query: Default::default(),
            all_options,
            index: 0,
            matcher: Matcher::new(nucleo_matcher::Config::DEFAULT),
            requested_scroll_to_option: true,
        }
    }

    pub fn matched_items(&mut self) -> Vec<(String, &EditMenuOption)> {
        let matched_dynamic_items: Vec<(String, &EditMenuOption)> = self
            .all_options
            .iter()
            .filter_map(|item| match item.pattern {
                EditMenuPattern::Static(_) => None,
                EditMenuPattern::Dynamic(_, f) => Some((f(&self.query)?, item)),
            })
            .collect::<Vec<_>>();

        let matched_static_items: Vec<(String, &EditMenuOption)> =
            nucleo_matcher::pattern::Pattern::parse(
                &self.query,
                nucleo_matcher::pattern::CaseMatching::Smart,
                nucleo_matcher::pattern::Normalization::Smart,
            )
            .match_list(
                self.all_options.iter().filter(|item| match item.pattern {
                    EditMenuPattern::Static(_) => true,
                    EditMenuPattern::Dynamic(_, _) => false,
                }),
                &mut self.matcher,
            )
            .iter()
            .map(|item| (item.0.pattern.label().to_owned(), item.0))
            .collect();

        // include dynamic before static
        [matched_dynamic_items, matched_static_items].concat()
    }

    pub fn focus_option(&mut self) -> Option<&EditMenuOption> {
        let index = self.index;
        let matched_items = self.matched_items();

        if matched_items.is_empty() {
            return None;
        }

        let index = index.rem_euclid(matched_items.len() as i8);
        let item = matched_items.get(index as usize)?;
        Some(item.1)
    }

    pub fn update(&mut self, reset_index: bool) {
        trace!(target: "editor.edit", "update; self.query = {}", self.query);
        self.requested_scroll_to_option = true;
        if reset_index {
            self.index = 0;
        }
    }
}

#[derive(Debug, Clone)]
pub struct EditMenuOption {
    pub pattern: EditMenuPattern,
    pub edit: Edit,
}

impl AsRef<str> for EditMenuOption {
    fn as_ref(&self) -> &str {
        self.pattern.label()
    }
}

impl EditMenuOption {
    pub fn new(pattern: EditMenuPattern, edit: Edit) -> Self {
        Self { pattern, edit }
    }
}

#[derive(Debug, Clone)]
pub enum EditMenuPattern {
    Static(String),
    Dynamic(String, fn(&String) -> Option<String>),
}

impl EditMenuPattern {
    pub fn label(&self) -> &str {
        match self {
            Self::Static(s) | Self::Dynamic(s, _) => s,
        }
    }
}

pub type Edit = fn(&String, CoreEditorState) -> Option<CoreEditorState>;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Constructor {
    Literal(String),
    Root,
    Newline,
    /// positional argument
    PosArg,
}

impl Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(s) => write!(f, "{s}"),
            Self::Newline => write!(f, "<newline>"),
            Self::Root => write!(f, "<root>"),
            Self::PosArg => write!(f, "<pos_arg>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Diagnostic(pub String);

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct RenderContext {
    pub interactive: bool,
    pub indent_level: u8,
}

pub struct AssembleRenderExprArgs {}

pub trait EditorSpec: 'static {
    fn name() -> String;

    fn initial_state() -> CoreEditorState;

    fn get_edits(state: &EditorState<Self>) -> Vec<EditMenuOption>;

    fn get_diagnostics(state: EditorState<Self>) -> Vec<Diagnostic>;

    fn is_valid_handle_specialized(handle: &Handle, root: &EditorExpr) -> bool;

    fn is_valid_handle(handle: &Handle, root: &EditorExpr) -> bool {
        trace!(target: "is_valid_handle", "handle = {handle}");
        trace!(target: "is_valid_handle", "root   = {root}");

        let b_general = || match handle {
            Handle::Point(p) => {
                let e = root.at_path(&p.path);
                #[expect(clippy::match_like_matches_macro)]
                match e.label.constructor {
                    Constructor::Newline => false,
                    _ => true,
                }
            }
            Handle::Span(h) => {
                let e = root.at_path(&h.path);
                #[expect(clippy::match_like_matches_macro)]
                match e.label.constructor {
                    Constructor::Newline => false,
                    _ => true,
                }
            }
            Handle::Zipper(h) => {
                // These are closures so that the final conjunction is evaluated
                // in a short-circuited fashion.
                let b1 = || {
                    let e = root.at_path(&h.path_o);
                    #[expect(clippy::match_like_matches_macro)]
                    match e.label.constructor {
                        Constructor::Newline => false,
                        _ => true,
                    }
                };
                let b2 = || {
                    let e = root.at_path(&h.path_i());
                    #[expect(clippy::match_like_matches_macro)]
                    match e.label.constructor {
                        Constructor::Newline => false,
                        _ => true,
                    }
                };
                b1() && b2()
            }
        };

        let b_specialized = || Self::is_valid_handle_specialized(handle, root);

        b_general() && b_specialized()
    }

    #[expect(clippy::too_many_arguments)]
    fn assemble_rendered_expr(
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        ren_ctx: &RenderContext,
        state: &mut EditorState<Self>,
        path: &Path,
        expr: &EditorExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        literal: &str,
    );
}

pub fn match_input_move_dir(ctx: &egui::Context) -> Option<MoveDir> {
    if ctx.input(|i| i.key_pressed(egui::Key::ArrowLeft)) {
        Some(MoveDir::Prev)
    } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowRight)) {
        Some(MoveDir::Next)
    } else {
        None
    }
}

pub fn match_input_cycle_dir(ctx: &egui::Context) -> Option<CycleDir> {
    if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
        Some(CycleDir::Prev)
    } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowDown)) {
        Some(CycleDir::Next)
    } else {
        None
    }
}

// -----------------------------------------------------------------------------

pub struct RenderPoint<'a> {
    pub path: &'a Path,
    pub i: Index,
}

impl<'a> RenderPoint<'a> {
    pub fn render<ES: EditorSpec + ?Sized>(
        &self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        ren_ctx: &RenderContext,
        state: &mut EditorState<ES>,
    ) {
        state.render_point(
            ctx,
            ui,
            ren_ctx,
            &Point {
                path: self.path.clone(),
                i: self.i,
            },
        );
    }
}

pub struct RenderExpr<'a> {
    pub path: Path,
    pub expr: &'a EditorExpr,
}

impl<'a> RenderExpr<'a> {
    pub fn render<ES: EditorSpec + ?Sized>(
        &self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        ren_ctx: &RenderContext,
        state: &mut EditorState<ES>,
    ) {
        EditorState::render_expr(state, ctx, ui, ren_ctx, &self.path, self.expr);
    }
}

// -----------------------------------------------------------------------------

pub struct StateAction {
    pub state: CoreEditorState,
    pub action: Action,
}

pub enum Action {
    Copy,
    Paste,
    Cut,
    Delete,
    SetCore(CoreEditorState),
    SetHandle(SetHandle),
    Undo,
    Redo,
}

pub struct SetHandle {
    pub handle: Handle,
    pub snapshot: bool,
}
