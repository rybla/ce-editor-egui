use crate::tree::{self, Index, Tree};
use egui::{Frame, Ui};
use std::collections::HashMap;

/// We derive Deserialize/Serialize so we can persist app state on shutdown.
#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)] // if we add new fields, give them default values when deserializing old state
pub struct App {
    root: Tree,
    focus: Index,
}

impl Default for App {
    fn default() -> Self {
        Self {
            root: tree::big_tree(5, 5),
            focus: Index::default(),
        }
    }
}

impl App {
    /// Called once before the first frame.
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customize the look and feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.

        // Load previous app state (if any).
        // Note that you must enable the `persistence` feature for this to work.
        if false {
            if let Some(storage) = cc.storage {
                eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
            } else {
                Default::default()
            }
        } else {
            Default::default()
        }
    }

    fn render_tree(&mut self, ui: &mut Ui, ctx: &egui::Context) {
        type IndexToResponse = HashMap<Index, egui::Response>;
        let mut index_to_response: IndexToResponse = HashMap::new();
        let mut moved = false;

        fn go(
            app: &mut App,
            ui: &mut Ui,
            index_to_response: &mut IndexToResponse,
            moved: &mut bool,
            outside_focus: bool,
            tree: &Tree,
            index: Index,
        ) {
            ui.vertical(|ui| {
                let frame = Frame::new()
                    .inner_margin(12)
                    .outer_margin(12)
                    .corner_radius(12)
                    .shadow(egui::Shadow {
                        offset: [4, 4],
                        blur: 12,
                        spread: 0,
                        color: egui::Color32::from_black_alpha(180),
                    })
                    .fill(egui::Color32::BLUE)
                    .stroke(if outside_focus && index.len() == app.focus.len() {
                        egui::Stroke::new(2.0, egui::Color32::RED)
                    } else {
                        egui::Stroke::new(2.0, egui::Color32::BLACK)
                    });

                let frame_response = frame.show(ui, |ui| {
                    let label = ui.button(egui::RichText::new(tree.label.clone()));
                    if label.clicked() {
                        app.focus = index.clone();
                        *moved = true;
                    }

                    ui.horizontal(|ui| {
                        for (i, kid) in tree.kids.iter().enumerate() {
                            let mut index_kid = index.clone();
                            index_kid.push(i);

                            let outside_focus_kid = outside_focus
                                && match app.focus.get(index_kid.len() - 1) {
                                    None => false,
                                    Some(j) => i == j,
                                };

                            go(
                                app,
                                ui,
                                index_to_response,
                                moved,
                                outside_focus_kid,
                                kid,
                                index_kid,
                            );
                        }
                    })
                });

                index_to_response.insert(index.clone(), frame_response.response);

                ui.set_max_size(ui.min_size());
            });
        }

        go(
            self,
            ui,
            &mut index_to_response,
            &mut moved,
            true,
            &self.root.clone(),
            Index::default(),
        );

        if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            self.focus.move_up_unsafe();
            moved = true;
        } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowDown)) {
            self.focus.move_down_unsafe(0);
            moved = true;
        } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowLeft)) {
            let result = self.focus.move_prev(&self.root);
            if let Err(msg) = &result {
                println!("move prev error: {msg}")
            }
            moved = result.is_ok();
        } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowRight)) {
            let result = self.focus.move_next(&self.root);
            if let Err(msg) = &result {
                println!("move next error: {msg}")
            }
            moved = result.is_ok();
        } else if ctx.input(|i| i.key_pressed(egui::Key::Enter)) {
            self.root.wrap_with_path_at_index(
                &self.focus,
                vec![tree::Tooth {
                    label: "A".to_string(),
                    kids_left: vec![tree::Tree {
                        label: "B".to_string(),
                        kids: vec![],
                    }],
                    kids_right: vec![tree::Tree {
                        label: "C".to_string(),
                        kids: vec![],
                    }],
                }],
            );
            moved = true;
        }

        if moved {
            let response = index_to_response.get(&self.focus).unwrap();
            response.scroll_to_me(Some(egui::Align::LEFT));
        }
    }
}

impl eframe::App for App {
    /// Called by the framework to save state before shutdown.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                // NOTE: no File->Quit on web pages!
                let is_web = cfg!(target_arch = "wasm32");
                if !is_web {
                    ui.menu_button("File", |ui| {
                        if ui.button("Quit").clicked() {
                            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                        }
                    });
                    ui.add_space(16.0);
                }

                egui::widgets::global_theme_preference_buttons(ui);
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("ce-editor-egui");

            ui.label(format!("focus: {:?}", self.focus));

            egui::ScrollArea::both()
                .auto_shrink([false, true])
                .scroll_source(egui::containers::scroll_area::ScrollSource::MOUSE_WHEEL)
                .show(ui, |ui| {
                    ui.style_mut().wrap_mode = Some(egui::TextWrapMode::Extend);
                    self.render_tree(ui, ctx);
                })
        });
    }
}
