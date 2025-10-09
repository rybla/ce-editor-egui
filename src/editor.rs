use crate::{ex, expr::*, span};
use egui::{Frame, Layout};
use lazy_static::lazy_static;
use nucleo;
use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
    sync::Arc,
};

pub const WRAP_EXPR_AT_HEIGHT: bool = false;
pub const MAX_EXPR_HEIGHT_FOR_HORIZONTAL: u32 = 2;

#[derive(Clone)]
pub struct ColorScheme {
    pub normal_border: egui::Color32,
    pub normal_text: egui::Color32,
    pub normal_background: egui::Color32,
    pub active_text: egui::Color32,
    pub active_background: egui::Color32,
    pub inactive_text: egui::Color32,
    pub inactive_background: egui::Color32,
    pub highlight_background: egui::Color32,
}

lazy_static! {
    pub static ref dark_color_scheme: ColorScheme = ColorScheme {
        normal_text: egui::Color32::WHITE,
        normal_background: egui::Color32::BLACK,
        normal_border: egui::Color32::WHITE,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::BLUE,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::DARK_BLUE,
        highlight_background: egui::Color32::DARK_BLUE,
    };
    pub static ref light_color_scheme: ColorScheme = ColorScheme {
        normal_text: egui::Color32::BLACK,
        normal_background: egui::Color32::WHITE,
        normal_border: egui::Color32::BLACK,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::BLUE,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::LIGHT_BLUE,
        highlight_background: egui::Color32::LIGHT_BLUE,
    };
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
}

impl<ES: EditorSpec + ?Sized> EditorState<ES> {
    pub fn default() -> Self {
        Self {
            spec: PhantomData,
            core: ES::initial_state(),
            menu: Default::default(),
            history: vec![],
            future: vec![],
        }
    }

    pub fn escape(&mut self) {
        if self.menu.is_some() {
            self.menu = None;
        } else {
            // TODO: unify this into Action enumeration
            self.core
                .handle
                .escape()
                .unwrap_or_else(|err| println!("Failed to move: {err:?}"))
        }
    }

    pub fn update(&mut self, ctx: &egui::Context) {
        println!("\n[update]");

        if let Some(menu) = &mut self.menu {
            menu.nucleo.tick(10);
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
                        println!("Edit failed");
                    }
                } else {
                    println!("There are no edit options available")
                }
            }
            // move menu option
            else if let Some(dir) = match_input_cycle_dir(ctx) {
                match dir {
                    CycleDir::Prev => menu.index -= 1,
                    CycleDir::Next => menu.index += 1,
                }
            }
        }
        // open edit menu
        else if ctx.input(|i| i.key_pressed(egui::Key::Space)) {
            println!("[menu] open");
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
            self.do_action(Action::Cut);
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
            let mut target: Point = self.core.handle.focus_point();
            loop {
                let move_status = target.move_dir(&self.core.expr, &dir);
                if !move_status.is_ok() {
                    println!("[select] bailed since a move failed");
                    break;
                }

                if let Some(h) = self.core.handle.clone().drag(&self.core.expr, &target) {
                    if ES::is_valid_handle(&h, &self.core.expr) {
                        self.do_action(Action::SetHandle(SetHandle {
                            handle: h,
                            snapshot: false,
                        }));
                        break;
                    }
                }
            }
        }
        // move
        else if let Some(dir) = match_input_move_dir(ctx) {
            let origin = self.core.handle.clone();
            loop {
                let move_status = self.core.handle.move_dir(&self.core.expr, &dir);
                if !move_status.is_ok() {
                    println!("[move] bailed since a move failed");
                    break;
                }

                if ES::is_valid_handle(&self.core.handle, &self.core.expr) {
                    break;
                } else {
                    self.do_action(Action::SetHandle(SetHandle {
                        handle: origin.clone(),
                        snapshot: false,
                    }));
                }
            }
        }
        // move up
        else if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            // TODO: unify this into Action enumeration

            let mut result = self.core.handle.move_up(&self.core.expr);

            while result.is_ok() && !ES::is_valid_handle(&self.core.handle, &self.core.expr) {
                result = self.core.handle.move_up(&self.core.expr);
            }

            if let Err(err) = result {
                println!("Failed to move up: {err:?}")
            }
        }
    }

    pub fn color_scheme(ui: &egui::Ui) -> &'static ColorScheme {
        match ui.ctx().theme() {
            egui::Theme::Dark => &dark_color_scheme,
            egui::Theme::Light => &light_color_scheme,
        }
    }

    pub fn render(&mut self, ui: &mut egui::Ui) {
        Frame::new().show(ui, |ui| {
            egui::ScrollArea::both().show(ui, |ui| {
                ui.with_layout(
                    Layout::left_to_right(egui::Align::TOP).with_main_wrap(true),
                    |ui| {
                        ui.spacing_mut().item_spacing = egui::Vec2::ZERO;
                        let text_height = ui.text_style_height(&egui::TextStyle::Body);
                        ui.set_row_height(text_height);

                        self.render_expr(ui, true, &Path::empty(), &self.core.expr.clone());
                    },
                )
            })
        });

        // ui.horizontal_top(|ui| {
        //     ui.style_mut().spacing.item_spacing.x = 0f32;
        //     ui.style_mut().spacing.item_spacing.y = 0f32;

        //     self.render_expr(ui, true, &Path::empty(), &self.core.expr.clone());
        // });
    }

    pub fn render_point(&mut self, ui: &mut egui::Ui, interactive: bool, point: &Point) {
        let color_scheme = Self::color_scheme(ui);

        let is_handle = point == &self.core.handle.focus_point();

        let is_at_handle = match &self.core.handle {
            Handle::Point(handle) => point == handle,
            Handle::Span(handle) => point == &handle.p_l() || point == &handle.p_r(),
            Handle::Zipper(handle) => {
                point == &handle.p_ol()
                    || point == &handle.p_or()
                    || point == &handle.p_il()
                    || point == &handle.p_ir()
            }
        };

        let is_in_handle = self.core.handle.contains_point(point);

        let fill_color = if is_handle {
            color_scheme.active_background
        } else if is_at_handle {
            color_scheme.inactive_background
        } else if is_in_handle {
            color_scheme.highlight_background
        } else {
            color_scheme.normal_background
        };

        let text_color = color_scheme.normal_text;

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            let label = ui.add(egui::Label::new(
                egui::RichText::new(format!("•")).color(text_color),
            ));

            if interactive && label.clicked() {
                println!("clicked point frame");
                let h = Handle::Point(point.clone());
                self.do_action(Action::SetHandle(SetHandle {
                    handle: h,
                    snapshot: false,
                }));
            }

            if is_handle && let Some(menu) = &mut self.menu {
                // render edit menu stuff
                ui.vertical(|ui| {
                    // query

                    // TODO: prevent default behavior on ArrowUp and ArrowDown, since that controls menu cycling
                    let textedit = egui::TextEdit::singleline(&mut menu.query)
                        .hint_text("query edit menu")
                        .desired_width(100f32) // 100f32 is a fine width for now
                        .cursor_at_end(true);
                    let response = ui.add(textedit);
                    // on change, update menu
                    if response.changed() {
                        menu.update();
                    }
                    // when the menu is open, the query should always have focus
                    if !response.has_focus() {
                        response.request_focus();
                    }

                    // options

                    let snapshot = menu.nucleo.snapshot();

                    if menu.query.is_empty() {
                        if menu.all_options.is_empty() {
                            let rich_text =
                                egui::RichText::new(format!("no edit options available here!"));
                            ui.label(
                                rich_text
                                    .color(Self::color_scheme(ui).active_text)
                                    .background_color(Self::color_scheme(ui).active_background),
                            );
                        } else {
                            let focus_index =
                                menu.index.rem_euclid(menu.all_options.len() as i8) as usize;
                            for (item_index, option) in menu.all_options.iter().enumerate() {
                                let label = option.pattern.label();
                                let rich_text =
                                    egui::RichText::new(format!("[{item_index}] {}", label));
                                if item_index == focus_index {
                                    ui.label(
                                        rich_text
                                            .color(Self::color_scheme(ui).active_text)
                                            .background_color(
                                                Self::color_scheme(ui).active_background,
                                            ),
                                    );
                                } else {
                                    ui.label(
                                        rich_text
                                            .color(Self::color_scheme(ui).normal_text)
                                            .background_color(
                                                Self::color_scheme(ui).normal_background,
                                            ),
                                    );
                                }
                            }
                        }
                    } else {
                        // TODO: refactor this to use menu.matched_items

                        let matched_dynamic_items = menu
                            .all_options
                            .iter()
                            .filter_map(|item| match item.pattern {
                                EditMenuPattern::Static(_) => None,
                                EditMenuPattern::Dynamic(_, f) => Some((f(&menu.query)?, item)),
                            })
                            .collect::<Vec<_>>();
                        let matched_items_count =
                            snapshot.item_count() + matched_dynamic_items.len() as u32;
                        let focus_index = if matched_items_count == 0 {
                            None
                        } else {
                            Some(menu.index.rem_euclid(matched_items_count as i8) as usize)
                        };

                        if let Some(focus_index) = focus_index {
                            let matched_static_items = menu
                                .nucleo
                                .snapshot()
                                .matched_items(..)
                                .map(|item| (item.data.pattern.label(), item.data))
                                .collect::<Vec<_>>();
                            // include dynamic before static
                            let matched_items = matched_dynamic_items
                                .iter()
                                .chain(matched_static_items.iter());
                            for (item_index, (label, _item)) in matched_items.enumerate() {
                                let rich_text =
                                    egui::RichText::new(format!("[{item_index}] {}", label));
                                if item_index == focus_index {
                                    ui.label(
                                        rich_text
                                            .color(Self::color_scheme(ui).active_text)
                                            .background_color(
                                                Self::color_scheme(ui).active_background,
                                            ),
                                    );
                                } else {
                                    ui.label(
                                        rich_text
                                            .color(Self::color_scheme(ui).normal_text)
                                            .background_color(
                                                Self::color_scheme(ui).normal_background,
                                            ),
                                    );
                                }
                            }
                        }
                    }
                });
            }
        });
    }

    pub fn render_expr(
        &mut self,
        ui: &mut egui::Ui,
        interactive: bool,
        path: &Path,
        expr: &EditorExpr,
    ) {
        self.render_expr_contents(ui, interactive, path, expr)
    }

    pub fn render_expr_contents(
        &mut self,
        ui: &mut egui::Ui,
        interactive: bool,
        path: &Path,
        expr: &EditorExpr,
    ) {
        let mut render_steps_and_kids = vec![];

        for (s, e) in expr.kids.steps_and_kids() {
            render_steps_and_kids.push((
                RenderPoint {
                    path,
                    i: s.left_index(),
                    interactive,
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
                interactive,
            },
            None,
        ));

        match &expr.label.constructor {
            Constructor::Literal(literal) => {
                ES::assemble_rendered_expr(self, ui, path, expr, render_steps_and_kids, literal)
            }
            Constructor::Newline => {
                ui.end_row();
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
                if let Some((frag, h)) = self.core.expr.cut(&self.core.handle) {
                    self.snapshot();
                    self.menu = None;
                    self.core.clipboard = Some(frag);
                    self.core.handle = h;
                }
            }
            Action::Delete => {
                if let Some((_frag, h)) = self.core.expr.cut(&self.core.handle) {
                    self.snapshot();
                    self.menu = None;
                    self.core.handle = h;
                }
            }
            Action::SetHandle(args) => {
                if ES::is_valid_handle(&args.handle, &self.core.expr) {
                    if args.snapshot {
                        self.snapshot();
                    }
                    self.menu = None;
                    self.core.handle = args.handle;
                }
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
        CoreEditorState {
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
    pub nucleo: nucleo::Nucleo<EditMenuOption>,
}

impl EditMenu {
    fn new(all_options: Vec<EditMenuOption>) -> Self {
        let nucleo = nucleo::Nucleo::new(
            nucleo::Config::DEFAULT,
            Arc::new(|| {
                // TODO: not sure what this could be useful for
                println!("[notify]");
            }),
            Some(1),
            1,
        );
        let injector = nucleo.injector();
        for option in all_options.iter() {
            match option.pattern {
                EditMenuPattern::Static(_) => {
                    injector.push(option.clone(), |x, cols| cols[0] = x.pattern.label().into());
                }
                EditMenuPattern::Dynamic(_, _) => {}
            };
        }
        Self {
            nucleo,
            query: Default::default(),
            all_options,
            index: 0,
        }
    }

    pub fn matched_items(&self) -> Vec<(Option<String>, &EditMenuOption)> {
        let matched_dynamic_items: Vec<(Option<String>, &EditMenuOption)> = self
            .all_options
            .iter()
            .filter_map(|item| match item.pattern {
                EditMenuPattern::Static(_) => None,
                EditMenuPattern::Dynamic(_, f) => Some((Some(f(&self.query)?), item)),
            })
            .collect::<Vec<_>>();

        let matched_static_items: Vec<(Option<String>, &EditMenuOption)> = self
            .nucleo
            .snapshot()
            .matched_items(..)
            .map(|x| (None, x.data))
            .collect::<Vec<_>>();

        [matched_dynamic_items, matched_static_items].concat()
    }

    pub fn focus_option(&self) -> Option<&EditMenuOption> {
        let matched_items = self.matched_items();

        if matched_items.is_empty() {
            return None;
        }

        let index = self.index.rem_euclid(matched_items.len() as i8);
        let item = matched_items.get(index as usize)?;
        Some(item.1)
    }

    pub fn update(&mut self) {
        // println!("[EditMenu.update] {}", self.query);
        self.nucleo.pattern.reparse(
            0,
            &self.query,
            nucleo::pattern::CaseMatching::Smart,
            nucleo::pattern::Normalization::Smart,
            // TODO: I could store the previous search query somewhere and then
            // check if it's a prefix of the new search query if I want to make
            // this `true` sometimes for optimization. But it's not necessary.
            false,
        );
    }
}

#[derive(Debug, Clone)]
pub struct EditMenuOption {
    pub pattern: EditMenuPattern,
    pub edit: Edit,
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
    pub fn label(&self) -> String {
        match self {
            EditMenuPattern::Static(s) => s.clone(),
            EditMenuPattern::Dynamic(s, _f) => s.clone(), // TODO: instead of just this tring, have dynamic patterns also have a static laberl that's shown in this situation
        }
    }
}

pub type Edit = fn(&String, CoreEditorState) -> Option<CoreEditorState>;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Constructor {
    Literal(String),
    Newline,
}

impl Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constructor::Literal(s) => write!(f, "{}", s),
            Constructor::Newline => write!(f, "<newline>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Diagnostic(pub String);

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub trait EditorSpec: 'static {
    fn name() -> String;

    fn initial_state() -> CoreEditorState;

    fn get_edits(state: &EditorState<Self>) -> Vec<EditMenuOption>;

    fn get_diagnostics(state: EditorState<Self>) -> Vec<Diagnostic>;

    fn is_valid_handle(handle: &Handle, expr: &EditorExpr) -> bool;

    fn assemble_rendered_expr(
        state: &mut EditorState<Self>,
        ui: &mut egui::Ui,
        path: &Path,
        expr: &EditorExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        literal: &String,
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
    pub interactive: bool,
}

impl<'a> RenderPoint<'a> {
    pub fn render<ES: EditorSpec>(&self, state: &mut EditorState<ES>, ui: &mut egui::Ui) {
        EditorState::render_point(
            state,
            ui,
            self.interactive,
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
    pub fn render<ES: EditorSpec>(&self, state: &mut EditorState<ES>, ui: &mut egui::Ui) {
        EditorState::render_expr(state, ui, true, &self.path, self.expr)
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
