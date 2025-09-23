use crate::{expr::*, utility::modulus_i8_to_usize};
use egui::Frame;
use lazy_static::lazy_static;
use nucleo;
use std::{fmt::Debug, sync::Arc};

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

pub struct ExprLabel<ES: EditorSpec + ?Sized + 'static> {
    pub constructor: ES::Constructor,
    pub diagnostic: ES::Diagnostic,
}

impl<ES: EditorSpec + ?Sized + 'static> Debug for ExprLabel<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExprLabel")
            .field("constructor", &self.constructor)
            .field("diagnostic", &self.diagnostic)
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized + 'static> Clone for ExprLabel<ES> {
    fn clone(&self) -> Self {
        Self {
            constructor: self.constructor.clone(),
            diagnostic: self.diagnostic.clone(),
        }
    }
}

pub type EditorExpr<ES> = Expr<ExprLabel<ES>>;

pub struct EditorState<ES: EditorSpec + ?Sized + 'static> {
    pub core: CoreEditorState<ES>,
    pub menu: Option<EditMenu<ES>>,
    pub requested_menu_focus: bool,
}

impl<ES: EditorSpec + ?Sized + 'static> Debug for EditorState<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EditorState")
            .field("core", &self.core)
            .field("menu", &self.menu)
            .field("requested_menu_focus", &self.requested_menu_focus)
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized + 'static> EditorState<ES> {
    pub fn new(expr: EditorExpr<ES>, handle: Handle) -> Self {
        Self {
            core: CoreEditorState {
                expr,
                handle,
                clipboard: Default::default(),
            },
            menu: Default::default(),
            requested_menu_focus: false,
        }
    }

    pub fn escape(&mut self) {
        if self.menu.is_some() {
            self.menu = None;
            self.requested_menu_focus = false;
        } else {
            self.core.handle.escape();
        }
    }

    /// Sets handle and does associated updates after checking if the handle is
    /// valid (via [EditorSpec::is_valid_handle]). Returns the handle back if
    /// the handle is invalid.
    pub fn set_handle(&mut self, handle: Handle) -> Option<Handle> {
        if !ES::is_valid_handle(&handle, &self.core.expr) {
            return Some(handle);
        }
        self.set_handle_unsafe(handle);
        None
    }

    /// Sets handle and does associated updates without checking if the handle
    /// is valid first.
    pub fn set_handle_unsafe(&mut self, handle: Handle) {
        self.core.handle = handle;
        self.menu = Option::None;
        self.requested_menu_focus = false;
    }

    pub fn update(&mut self, ctx: &egui::Context) {
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
                    if let Some(state) = (option.edit)(&self.core) {
                        self.set_expr_and_handle(state.expr, state.handle);
                    } else {
                        println!("Edit failed");
                    }
                } else {
                    println!("No menu option available");
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
        // copy
        else if ctx.input(|i| i.key_pressed(egui::Key::C)) {
            println!("[copy] attempting to copy");
            if let Some(frag) = self.core.expr.get_fragment_at_handle(&self.core.handle) {
                println!("[copy] copied fragment");
                self.core.clipboard = Some(frag)
            }
        }
        // paste
        else if ctx.input(|i| i.key_pressed(egui::Key::V)) {
            println!("[paste]");
            if let Some(frag) = &self.core.clipboard {
                let (expr, handle) = self
                    .core
                    .expr
                    .clone()
                    .insert_fragment_at_handle(frag.clone(), self.core.handle.clone());
                self.core.handle = handle;
                self.core.expr = expr;
            }
        }
        // rotate focus
        else if ctx.input(|i| i.modifiers.command_only())
            && let Some(dir) = match_input_move_dir(ctx)
        {
            self.core.handle.rotate_focus_dir(dir);
        }
        // select
        else if ctx.input(|i| i.modifiers.shift)
            && let Some(dir) = match_input_move_dir(ctx)
        {
            let mut target = self.core.handle.focus_point();
            loop {
                let moved = target.move_dir(dir, &self.core.expr);
                if !moved {
                    println!("[select] bailed since a move failed");
                    break;
                }

                if let Some(handle) = self.core.handle.select_to(&target, &self.core.expr) {
                    if ES::is_valid_handle(&handle, &self.core.expr) {
                        self.set_handle_unsafe(handle);
                        break;
                    }
                }
            }
        }
        // move
        else if let Some(dir) = match_input_move_dir(ctx) {
            let mut handle = self.core.handle.clone();
            loop {
                let moved = handle.move_dir(dir, &self.core.expr);
                if !moved {
                    println!("[move] bailed since a move failed");
                    break;
                }

                if ES::is_valid_handle(&handle, &self.core.expr) {
                    self.set_handle_unsafe(handle);
                    break;
                }
            }
        }
        // move up
        else if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            self.core.handle.move_up(&self.core.expr);
        }
    }

    pub fn color_scheme(ui: &egui::Ui) -> &'static ColorScheme {
        match ui.ctx().theme() {
            egui::Theme::Dark => &dark_color_scheme,
            egui::Theme::Light => &light_color_scheme,
        }
    }

    pub fn render(&mut self, ui: &mut egui::Ui) {
        self.render_expr(ui, &self.core.expr.clone(), &Path::default());
    }

    pub fn render_point(&mut self, ui: &mut egui::Ui, point: &Point) {
        let is_handle = point == &self.core.handle.focus_point();

        let is_at_handle = match &self.core.handle {
            Handle::Point(handle) => point == handle,
            Handle::Span(handle) => {
                *point == handle.span_handle.left_point()
                    || *point == handle.span_handle.right_point()
            }
            Handle::Zipper(handle) => {
                *point == handle.zipper_handle.outer_left_point()
                    || *point == handle.zipper_handle.outer_right_point()
                    || *point == handle.zipper_handle.inner_left_point()
                    || *point == handle.zipper_handle.inner_right_point()
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
                let invalid_handle = self.set_handle(handle);
                if let Some(invalid_handle) = invalid_handle {
                    println!("Invalid handle: {:?}", invalid_handle);
                }
            }

            if is_handle && let Some(menu) = &mut self.menu {
                // render edit menu stuff
                ui.vertical(|ui| {
                    let menu_index_usize = menu.index_mod();
                    // TODO: prevent default behavior on ArrowUp and ArrowDown, since that controls menu cycling
                    let textedit = egui::TextEdit::singleline(&mut menu.query)
                        .hint_text("query edit menu")
                        // 100f32 is a fine width for now
                        .desired_width(100f32)
                        .cursor_at_end(true);

                    for (option_index, option) in menu.options.iter().enumerate() {
                        if option_index == menu_index_usize {
                            ui.label(
                                egui::RichText::new(format!(
                                    "[{option_index}] {}",
                                    option.label.clone()
                                ))
                                .color(Self::color_scheme(ui).active_text)
                                .background_color(Self::color_scheme(ui).active_background),
                            );
                        } else {
                            ui.label(
                                egui::RichText::new(format!(
                                    "[{option_index}] {}",
                                    option.label.clone()
                                ))
                                .color(Self::color_scheme(ui).normal_text)
                                .background_color(Self::color_scheme(ui).normal_background),
                            );
                        }
                    }

                    // on appearance, request menu focus
                    let response = ui.add(textedit);
                    if response.changed() {
                        menu.update();
                    }
                    if !self.requested_menu_focus {
                        response.request_focus();
                        self.requested_menu_focus = true;
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
                if let Some(step_parent) = path.pop() {
                    let invalid_handle = self.set_handle(Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: path,
                            left: step_parent.left_index(),
                            right: step_parent.right_index(),
                        },
                        focus: SpanFocus::Left,
                    }));
                    if let Some(invalid_handle) = invalid_handle {
                        println!("Invalid handle: {:?}", invalid_handle);
                    }
                }
            }

            for (step, kid) in expr.kids_and_steps() {
                // render left point
                self.render_point(
                    ui,
                    &Point {
                        path: path.clone(),
                        index: step.left_index(),
                    },
                );

                // render kid
                let mut kid_path = path.clone();
                kid_path.push(step);
                self.render_expr(ui, kid, &kid_path);
            }

            // render last point
            self.render_point(
                ui,
                &Point {
                    path: path.clone(),
                    index: expr.kids.extreme_indexes().1,
                },
            );
        });

        ui.set_max_size(ui.min_size());
    }

    pub fn set_expr_and_handle(&mut self, expr: Expr<ExprLabel<ES>>, handle: Handle) {
        self.core.expr = expr;
        let invalid_handle = self.set_handle(handle);
        if let Some(invalid_handle) = invalid_handle {
            panic!(
                "Invalid handle:\nexpr = {:?}\nhandle= {:?}",
                self.core.expr, invalid_handle
            );
        }
    }
}

pub struct CoreEditorState<ES: EditorSpec + ?Sized + 'static> {
    pub expr: EditorExpr<ES>,
    pub handle: Handle,
    pub clipboard: Option<Fragment<ExprLabel<ES>>>,
}

impl<ES: EditorSpec + ?Sized + 'static> Debug for CoreEditorState<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CoreEditorState")
            .field("expr", &self.expr)
            .field("handle", &self.handle)
            .field("clipboard", &self.clipboard)
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized + 'static> Clone for CoreEditorState<ES> {
    fn clone(&self) -> Self {
        Self {
            expr: self.expr.clone(),
            handle: self.handle.clone(),
            clipboard: self.clipboard.clone(),
        }
    }
}

pub struct EditMenu<ES: EditorSpec + ?Sized + 'static> {
    pub query: String,
    pub options: Vec<EditMenuOption<ES>>,
    pub index: i8,
    pub nucleo: nucleo::Nucleo<EditMenuOption<ES>>,
}

impl<ES: EditorSpec + ?Sized + 'static> Debug for EditMenu<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EditMenu")
            .field("query", &self.query)
            .field("options", &self.options)
            .field("index", &self.index)
            .field("nucleo", &"nucleo")
            .finish()
    }
}

impl<ES: EditorSpec + ?Sized + 'static> EditMenu<ES> {
    fn new(options: Vec<EditMenuOption<ES>>) -> Self {
        // TODO
        let nucleo = nucleo::Nucleo::new(nucleo::Config::DEFAULT, Arc::new(|| {}), Some(1), 100);
        // let injector = nucleo.injector();
        // for option in options.iter() {
        //     injector.push(option.label, |s, cols| cols[0] = s.clone().into());
        // }
        Self {
            nucleo,
            query: Default::default(),
            options,
            index: 0,
        }
    }

    pub fn index_mod(&self) -> usize {
        modulus_i8_to_usize(self.index, self.options.len())
    }

    pub fn focus_option(&self) -> Option<&EditMenuOption<ES>> {
        self.options.get(self.index_mod())
    }

    pub fn update(&mut self) {
        // self.nucleo.
        todo!()
    }
}

pub struct EditMenuOption<ES: EditorSpec + ?Sized + 'static> {
    pub label: String,
    pub edit: Edit<ES>,
}

impl<ES: EditorSpec + ?Sized + 'static> Clone for EditMenuOption<ES> {
    fn clone(&self) -> Self {
        Self {
            label: self.label.clone(),
            edit: self.edit.clone(),
        }
    }
}

impl<ES: EditorSpec + ?Sized + 'static> Debug for EditMenuOption<ES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EditMenuOption")
            .field("label", &self.label)
            .field("edit", &format!("<function>"))
            .finish()
    }
}

pub type Edit<ES> = fn(&CoreEditorState<ES>) -> Option<CoreEditorState<ES>>;

pub trait EditorSpec {
    type Constructor: Debug + Clone + Sized + PartialEq;
    type Diagnostic: Debug + Clone + Sized + PartialEq;

    fn name() -> String;

    fn initial_state() -> EditorState<Self>;

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
