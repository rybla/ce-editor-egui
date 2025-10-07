use crate::expr::*;
use egui::Frame;
use lazy_static::lazy_static;
use nucleo;
use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

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
        active_background: egui::Color32::RED,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::PURPLE,
        highlight_background: egui::Color32::DARK_BLUE,
    };
    pub static ref light_color_scheme: ColorScheme = ColorScheme {
        normal_text: egui::Color32::BLACK,
        normal_background: egui::Color32::WHITE,
        normal_border: egui::Color32::BLACK,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::RED,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::PURPLE,
        highlight_background: egui::Color32::LIGHT_BLUE,
    };
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct ExprLabel<ES: EditorSpec + ?Sized> {
    pub constructor: ES::Constructor,
    pub diagnostic: ES::Diagnostic,
}

impl<ES: EditorSpec + ?Sized> Display for ExprLabel<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constructor)
    }
}

impl<ES: EditorSpec + ?Sized> Debug for ExprLabel<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExprLabel")
            .field("constructor", &self.constructor)
            .field("diagnostic", &self.diagnostic)
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized> Clone for ExprLabel<ES> {
    fn clone(&self) -> Self {
        Self {
            constructor: self.constructor.clone(),
            diagnostic: self.diagnostic.clone(),
        }
    }
}

impl<ES: EditorSpec + ?Sized> ExprLabel<ES> {
    pub fn new(constructor: ES::Constructor, diagnostic: ES::Diagnostic) -> Self {
        Self {
            constructor,
            diagnostic,
        }
    }
}

pub type EditorExpr<ES> = Expr<ExprLabel<ES>>;

pub struct EditorState<ES: EditorSpec + ?Sized> {
    pub core: CoreEditorState<ES>,
    pub menu: Option<EditMenu<ES>>,
    pub history: Vec<CoreEditorState<ES>>,
}

impl<ES: EditorSpec + ?Sized> Debug for EditorState<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EditorState")
            .field("core", &self.core)
            .field("menu", &self.menu)
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized> EditorState<ES> {
    pub fn default() -> Self {
        Self {
            core: ES::initial_state(),
            menu: Default::default(),
            history: vec![],
        }
    }

    pub fn escape(&mut self) {
        if self.menu.is_some() {
            self.menu = None;
        } else {
            self.core
                .handle
                .escape()
                .unwrap_or_else(|err| println!("Failed to move: {err:?}"))
        }
    }

    /// Sets handle and does associated updates after checking if the handle is
    /// valid (via [EditorSpec::is_valid_handle]). Returns the handle back if
    /// the handle is invalid.
    pub fn set_handle_safe(&mut self, handle: Handle) -> Result<(), Handle> {
        if !ES::is_valid_handle(&handle, &self.core.expr) {
            return Err(handle);
        }
        self.set_handle_unsafe(handle);
        Ok(())
    }

    /// Sets handle and does associated updates without checking if the handle
    /// is valid first.
    pub fn set_handle_unsafe(&mut self, handle: Handle) {
        self.core.handle = handle;
        self.menu = Option::None;
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
                        self.set_core(core);
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
        // cut
        else if ctx.input(|i| i.key_pressed(egui::Key::X)) {
            if let Some((frag, h)) = self.core.expr.cut(&self.core.handle) {
                self.core.clipboard = Some(frag);
                self.core.handle = h;
            }
        }
        // copy
        else if ctx.input(|i| i.key_pressed(egui::Key::C)) {
            if let Some(frag) = self.core.expr.at_handle_cloned(&self.core.handle) {
                self.core.clipboard = Some(frag);
            }
        }
        // paste
        else if ctx.input(|i| i.key_pressed(egui::Key::V)) {
            if let Some(frag) = self.core.clipboard.clone() {
                let handle = self.core.expr.insert(self.core.handle.clone(), frag);
                self.core.handle = handle;
            }
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

                if let Some(handle) = self.core.handle.clone().drag(&self.core.expr, &target) {
                    if ES::is_valid_handle(&handle, &self.core.expr) {
                        self.set_handle_unsafe(handle);
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
                    self.set_handle_unsafe(origin.clone());
                }
            }
        }
        // move up
        else if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            self.core
                .handle
                .move_up()
                .unwrap_or_else(|err| println!("Failed to move up: {err:?}"))
        }
    }

    pub fn color_scheme(ui: &egui::Ui) -> &'static ColorScheme {
        match ui.ctx().theme() {
            egui::Theme::Dark => &dark_color_scheme,
            egui::Theme::Light => &light_color_scheme,
        }
    }

    pub fn render(&mut self, ui: &mut egui::Ui) {
        self.render_expr(ui, &self.core.expr.clone(), &Path::empty());
    }

    pub fn render_point(&mut self, ui: &mut egui::Ui, point: &Point) {
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

        let frame = Frame::new()
            .outer_margin(0)
            .inner_margin(egui::Margin {
                left: 4,
                right: 4,
                top: 0,
                bottom: 0,
            })
            .fill(if is_handle {
                Self::color_scheme(ui).active_background
            } else if is_at_handle {
                Self::color_scheme(ui).inactive_background
            } else if is_in_handle {
                Self::color_scheme(ui).highlight_background
            } else {
                Self::color_scheme(ui).normal_background
            });

        frame.show(ui, |ui| {
            let label = ui.label(egui::RichText::new(format!("•")).color(if is_handle {
                Self::color_scheme(ui).active_text
            } else if is_at_handle {
                Self::color_scheme(ui).inactive_text
            } else {
                Self::color_scheme(ui).normal_text
            }));
            if label.clicked() {
                let handle = Handle::Point(point.clone());
                let invalid_handle = self.set_handle_safe(handle);
                if let Err(invalid_handle) = invalid_handle {
                    println!("Invalid handle: {:?}", invalid_handle);
                }
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

    pub fn render_expr(&mut self, ui: &mut egui::Ui, expr: &EditorExpr<ES>, path: &Path) {
        if expr.height() <= MAX_EXPR_HEIGHT_FOR_HORIZONTAL {
            ui.horizontal_top(|ui| self.render_expr_contents(ui, expr, path));
        } else {
            ui.vertical(|ui| self.render_expr_contents(ui, expr, path));
        }
    }

    pub fn render_expr_contents(&mut self, ui: &mut egui::Ui, expr: &EditorExpr<ES>, path: &Path) {
        // This is the spacing between items in the horizontal/vertical row
        ui.style_mut().spacing.item_spacing.x = 0f32;
        ui.style_mut().spacing.item_spacing.y = 0f32;

        let frame = Frame::new()
            .outer_margin(0)
            .inner_margin(egui::Margin {
                left: 0,
                right: 0,
                top: 4,
                bottom: 4,
            })
            .fill(if self.core.handle.contains_path(path) {
                Self::color_scheme(ui).highlight_background
            } else {
                Self::color_scheme(ui).normal_background
            })
            .stroke(egui::Stroke::new(1.0, Self::color_scheme(ui).normal_border));

        frame.show(ui, |ui| {
            let label = ES::render_label(ui, &expr.label);
            if label.clicked() {
                let mut path = path.clone();
                if let Some(s) = path.0.pop() {
                    let is_invalid_handle = self.set_handle_safe(Handle::Span(SpanHandle {
                        path: path,
                        i_l: s.left_index(),
                        i_r: s.right_index(),
                        focus: SpanFocus::Left,
                    }));
                    if let Err(invalid_handle) = is_invalid_handle {
                        println!("Invalid handle: {:?}", invalid_handle);
                    }
                }
            }

            for (step, kid) in expr.kids.steps_and_exprs() {
                // render left point
                self.render_point(
                    ui,
                    &Point {
                        path: path.clone(),
                        i: step.left_index(),
                    },
                );

                // render kid
                let mut kid_path = path.clone();
                kid_path.0.push(step);
                self.render_expr(ui, kid, &kid_path);
            }

            // render last point
            self.render_point(
                ui,
                &Point {
                    path: path.clone(),
                    i: expr.kids.rightmost_index(),
                },
            );
        });

        ui.set_max_size(ui.min_size());
    }

    pub fn set_expr_and_handle(&mut self, expr: Expr<ExprLabel<ES>>, handle: Handle) {
        self.core.expr = expr;
        let invalid_handle = self.set_handle_safe(handle);
        if let Err(invalid_handle) = invalid_handle {
            panic!(
                "Invalid handle:\nexpr = {:?}\nhandle= {:?}",
                self.core.expr, invalid_handle
            );
        }
    }

    pub fn set_core(&mut self, core: CoreEditorState<ES>) {
        self.core.expr = core.expr;
        let invalid_handle = self.set_handle_safe(core.handle);
        if let Err(invalid_handle) = invalid_handle {
            panic!(
                "Invalid handle:\nexpr = {:?}\nhandle= {:?}",
                self.core.expr, invalid_handle
            );
        }
        if let Some(clipboard) = core.clipboard {
            println!("[set_core] update clipboard");
            self.core.clipboard = Some(clipboard);
        } else {
            println!("[set_core] don't update clipboard");
        }
    }

    pub fn handle_action(&mut self, _action: Action<ES>) {
        todo!()
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct CoreEditorState<ES: EditorSpec + ?Sized> {
    pub expr: EditorExpr<ES>,
    pub handle: Handle,
    pub clipboard: Option<Fragment<ExprLabel<ES>>>,
}

impl<ES: EditorSpec + ?Sized> Debug for CoreEditorState<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CoreEditorState")
            .field("expr", &self.expr)
            .field("handle", &self.handle)
            .field("clipboard", &self.clipboard)
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized> Clone for CoreEditorState<ES> {
    fn clone(&self) -> Self {
        Self {
            expr: self.expr.clone(),
            handle: self.handle.clone(),
            clipboard: self.clipboard.clone(),
        }
    }
}

impl<ES: EditorSpec + ?Sized> CoreEditorState<ES> {
    pub fn new(expr: EditorExpr<ES>, handle: Handle) -> Self {
        CoreEditorState {
            expr,
            handle,
            clipboard: Option::None,
        }
    }
}

pub struct EditMenu<ES: EditorSpec + ?Sized> {
    pub query: String,
    pub all_options: Vec<EditMenuOption<ES>>,
    pub index: i8,
    pub nucleo: nucleo::Nucleo<EditMenuOption<ES>>,
}

impl<ES: EditorSpec + ?Sized> Debug for EditMenu<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EditMenu")
            .field("query", &self.query)
            .field("options", &self.all_options)
            .field("index", &self.index)
            .field("nucleo", &"nucleo")
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized> EditMenu<ES> {
    fn new(all_options: Vec<EditMenuOption<ES>>) -> Self {
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

    pub fn matched_items(&self) -> Vec<(Option<String>, &EditMenuOption<ES>)> {
        let matched_dynamic_items: Vec<(Option<String>, &EditMenuOption<ES>)> = self
            .all_options
            .iter()
            .filter_map(|item| match item.pattern {
                EditMenuPattern::Static(_) => None,
                EditMenuPattern::Dynamic(_, f) => Some((Some(f(&self.query)?), item)),
            })
            .collect::<Vec<_>>();

        let matched_static_items: Vec<(Option<String>, &EditMenuOption<ES>)> = self
            .nucleo
            .snapshot()
            .matched_items(..)
            .map(|x| (None, x.data))
            .collect::<Vec<_>>();

        [matched_dynamic_items, matched_static_items].concat()
    }

    pub fn focus_option(&self) -> Option<&EditMenuOption<ES>> {
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

pub struct EditMenuOption<ES: EditorSpec + ?Sized> {
    pub pattern: EditMenuPattern,
    pub edit: Edit<ES>,
}

impl<ES: EditorSpec + ?Sized> Clone for EditMenuOption<ES> {
    fn clone(&self) -> Self {
        Self {
            pattern: self.pattern.clone(),
            edit: self.edit.clone(),
        }
    }
}

impl<ES: EditorSpec + ?Sized> Debug for EditMenuOption<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EditMenuOption")
            .field("label", &self.pattern)
            .field("edit", &format!("<function>"))
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized> EditMenuOption<ES> {
    pub fn new(pattern: EditMenuPattern, edit: Edit<ES>) -> Self {
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

pub type Edit<ES> = fn(&String, CoreEditorState<ES>) -> Option<CoreEditorState<ES>>;

pub trait EditorSpec: 'static {
    type Constructor: Debug
        + Display
        + Clone
        + Sized
        + PartialEq
        + serde::Serialize
        + for<'a> serde::Deserialize<'a>;
    type Diagnostic: Debug
        + Display
        + Clone
        + Sized
        + PartialEq
        + serde::Serialize
        + for<'a> serde::Deserialize<'a>;

    fn name() -> String;

    fn initial_state() -> CoreEditorState<Self>;

    fn get_edits(state: &EditorState<Self>) -> Vec<EditMenuOption<Self>>;

    fn get_diagnostics(state: EditorState<Self>) -> Vec<Self::Diagnostic>;

    fn is_valid_handle(handle: &Handle, expr: &EditorExpr<Self>) -> bool;

    fn render_label(ui: &mut egui::Ui, label: &ExprLabel<Self>) -> egui::Response;
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

pub enum Action<ES: EditorSpec + ?Sized> {
    Copy,
    Paste,
    Cut,
    Insert(Fragment<ExprLabel<ES>>),
}

pub struct StateAction<ES: EditorSpec + ?Sized> {
    pub state: CoreEditorState<ES>,
    pub action: Action<ES>,
}
