use crate::expr::*;
use egui::Frame;
use lazy_static::lazy_static;
use std::fmt::Debug;

pub const MAX_EXPR_HEIGHT_FOR_HORIZONTAL: u32 = 2;

#[derive(Clone)]
pub struct ColorScheme {
    pub normal_border: egui::Color32,
    pub normal_text: egui::Color32,
    pub normal_background: egui::Color32,
    pub active_text: egui::Color32,
    pub active_background: egui::Color32,
    pub highlight_background: egui::Color32,
}

lazy_static! {
    pub static ref dark_color_scheme: ColorScheme = ColorScheme {
        normal_text: egui::Color32::WHITE,
        normal_background: egui::Color32::TRANSPARENT,
        normal_border: egui::Color32::WHITE,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::PURPLE,
        highlight_background: egui::Color32::DARK_BLUE,
    };
    pub static ref light_color_scheme: ColorScheme = ColorScheme {
        normal_text: egui::Color32::BLACK,
        normal_background: egui::Color32::TRANSPARENT,
        normal_border: egui::Color32::BLACK,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::BLUE,
        highlight_background: egui::Color32::LIGHT_BLUE,
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLabel<C, D> {
    pub constructor: C,
    pub diagnostic: D,
}

#[derive(Debug)]
pub struct EditorState<C, D> {
    pub expr: Expr<ExprLabel<C, D>>,
    pub handle: Handle,
}

#[derive(Debug, Default)]
pub struct EditMenu {
    // TODO
}

pub trait EditorSpec {
    type Constructor: Debug + Clone;
    type Diagnostic: Debug + Clone;

    fn name() -> String;

    fn initial_state() -> EditorState<Self::Constructor, Self::Diagnostic>;

    fn get_edit_menu(state: EditorState<Self::Constructor, Self::Diagnostic>) -> EditMenu;

    fn get_diagnostics(
        state: EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> Vec<Self::Diagnostic>;

    fn is_valid_handle<L: Debug>(handle: &Handle, expr: &Expr<L>) -> bool;

    fn render_label(
        ui: &mut egui::Ui,
        label: &ExprLabel<Self::Constructor, Self::Diagnostic>,
    ) -> egui::Response;

    fn color_scheme(ui: &egui::Ui) -> &'static ColorScheme {
        match ui.ctx().theme() {
            egui::Theme::Dark => &dark_color_scheme,
            egui::Theme::Light => &light_color_scheme,
        }
    }

    fn match_input_move_dir(ctx: &egui::Context) -> Option<MoveDir> {
        if ctx.input(|i| i.key_pressed(egui::Key::ArrowLeft)) {
            Some(MoveDir::Prev)
        } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowRight)) {
            Some(MoveDir::Next)
        } else {
            None
        }
    }

    fn update(state: &mut EditorState<Self::Constructor, Self::Diagnostic>, ctx: &egui::Context) {
        // escape
        if ctx.input(|i| i.key_pressed(egui::Key::Escape)) {
            state.handle.escape();
        }
        // rotate focus
        else if ctx.input(|i| i.modifiers.command_only())
            && let Some(dir) = Self::match_input_move_dir(ctx)
        {
            state.handle.rotate_focus_dir(dir);
        }
        // move select
        else if ctx.input(|i| i.modifiers.shift)
            && let Some(dir) = Self::match_input_move_dir(ctx)
        {
            let origin = state.handle.clone();
            let mut success = false;
            while !success {
                let moved = state.handle.move_select_dir(dir, &state.expr);
                if !moved {
                    break;
                }
                success = Self::is_valid_handle(&state.handle, &state.expr);
            }
            if !success {
                println!("bailed select since move returned false before success");
                state.handle = origin;
            }
        }
        // move
        else if let Some(dir) = Self::match_input_move_dir(ctx) {
            let origin = state.handle.clone();
            // move until success
            let mut success = false;
            while !success {
                let moved = state.handle.move_dir(dir, &state.expr);
                if !moved {
                    break;
                }
                success = Self::is_valid_handle(&state.handle, &state.expr);
            }
            // if broke before a success, then reset to origin
            if !success {
                println!("bailed move since move returned false before success");
                state.handle = origin;
            }
        }
        // move up
        else if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            state.handle.move_up(&state.expr);
        }
    }

    fn render(state: &mut EditorState<Self::Constructor, Self::Diagnostic>, ui: &mut egui::Ui) {
        Self::render_expr(state, ui, &state.expr.clone(), &Path::default());
    }

    fn render_point(
        state: &mut EditorState<Self::Constructor, Self::Diagnostic>,
        ui: &mut egui::Ui,
        point: &Point,
    ) {
        let is_handle = point == &state.handle.focus_point();

        let is_at_handle = match &state.handle {
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

        let is_in_handle = state.handle.contains_point(point);

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
                Self::color_scheme(ui).highlight_background
            } else if is_in_handle {
                Self::color_scheme(ui).highlight_background
            } else {
                Self::color_scheme(ui).normal_background
            });

        frame.show(ui, |ui| {
            let label = ui.label(egui::RichText::new(format!("•")).color(if is_at_handle {
                Self::color_scheme(ui).active_text
            } else {
                Self::color_scheme(ui).normal_text
            }));
            if label.clicked() {
                let handle = Handle::Point(point.clone());
                if Self::is_valid_handle(&handle, &state.expr) {
                    state.handle = handle;
                }
            }
        });
    }

    fn render_expr(
        state: &mut EditorState<Self::Constructor, Self::Diagnostic>,
        ui: &mut egui::Ui,
        expr: &Expr<ExprLabel<Self::Constructor, Self::Diagnostic>>,
        path: &Path,
    ) {
        if expr.height() <= MAX_EXPR_HEIGHT_FOR_HORIZONTAL {
            ui.horizontal_top(|ui| Self::render_expr_contents(state, ui, expr, path));
        } else {
            ui.vertical(|ui| Self::render_expr_contents(state, ui, expr, path));
        }
    }

    fn render_expr_contents(
        state: &mut EditorState<Self::Constructor, Self::Diagnostic>,
        ui: &mut egui::Ui,
        expr: &Expr<ExprLabel<Self::Constructor, Self::Diagnostic>>,
        path: &Path,
    ) {
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
            .fill(if state.handle.contains_path(path) {
                Self::color_scheme(ui).highlight_background
            } else {
                Self::color_scheme(ui).normal_background
            })
            .stroke(egui::Stroke::new(1.0, Self::color_scheme(ui).normal_border));

        frame.show(ui, |ui| {
            let label = Self::render_label(ui, &expr.label);
            if label.clicked() {
                let mut path = path.clone();
                if let Some(step_parent) = path.pop() {
                    state.handle = Handle::Span(SpanHandleAndFocus {
                        span_handle: SpanHandle {
                            path: path,
                            left: step_parent.left_index(),
                            right: step_parent.right_index(),
                        },
                        focus: SpanFocus::Left,
                    });
                }
            }

            for (step, kid) in expr.kids_and_steps() {
                // render left point
                Self::render_point(
                    state,
                    ui,
                    &Point {
                        path: path.clone(),
                        index: step.left_index(),
                    },
                );

                // render kid
                let mut kid_path = path.clone();
                kid_path.push(step);
                Self::render_expr(state, ui, kid, &kid_path);
            }

            // render last point
            Self::render_point(
                state,
                ui,
                &Point {
                    path: path.clone(),
                    index: expr.kids.extreme_indexes().1,
                },
            );
        });

        ui.set_max_size(ui.min_size());
    }
}
