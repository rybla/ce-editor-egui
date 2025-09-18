use crate::editor;

pub struct App<EditorSpec: editor::EditorSpec> {
    pub editor_state: editor::EditorState<EditorSpec>,
}

impl<EditorSpec: editor::EditorSpec> Default for App<EditorSpec> {
    fn default() -> Self {
        Self {
            editor_state: EditorSpec::initial_state(),
        }
    }
}

impl<EditorSpec: editor::EditorSpec> App<EditorSpec> {
    /// Called once before the first frame.
    pub fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customize the look and feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.

        Default::default()
    }
}

impl<EditorSpec: editor::EditorSpec> eframe::App for App<EditorSpec> {
    /// Called by the framework to save state before shutdown.
    fn save(&mut self, _storage: &mut dyn eframe::Storage) {}

    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.editor_state.update(ctx);

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

            egui::ScrollArea::both()
                .auto_shrink([false, true])
                .scroll_source(egui::containers::scroll_area::ScrollSource::MOUSE_WHEEL)
                .show(ui, |ui| {
                    ui.style_mut().wrap_mode = Some(egui::TextWrapMode::Extend);
                    self.editor_state.render(ui);
                });

            ui.label(format!("handle: {:#?}", &self.editor_state.core.handle));
            ui.label(format!(
                "clipboard: {:#?}",
                &self.editor_state.core.clipboard
            ));
        });
    }
}
