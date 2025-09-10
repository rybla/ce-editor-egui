use crate::editor;

pub struct App<Constructor, Diagnostic, EditorSpec>
where
    EditorSpec: editor::EditorSpec<Constructor, Diagnostic>,
{
    editor_spec: EditorSpec,
    editor_state: editor::EditorState<Constructor, Diagnostic>,
}

impl<Constructor, Diagnostic, EditorSpec> Default for App<Constructor, Diagnostic, EditorSpec>
where
    EditorSpec: editor::EditorSpec<Constructor, Diagnostic>,
{
    fn default() -> Self {
        Self {
            editor_spec: todo!(),
            editor_state: todo!(),
        }
    }
}

impl<Constructor, Diagnostic, EditorSpec> App<Constructor, Diagnostic, EditorSpec>
where
    EditorSpec: editor::EditorSpec<Constructor, Diagnostic>,
{
    /// Called once before the first frame.
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customize the look and feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.

        Default::default()
    }
}

impl<Constructor, Diagnostic, EditorSpec> eframe::App for App<Constructor, Diagnostic, EditorSpec>
where
    EditorSpec: editor::EditorSpec<Constructor, Diagnostic>,
{
    /// Called by the framework to save state before shutdown.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        // TODO: remove this since not doing storage
        // eframe::set_value(storage, eframe::APP_KEY, self);
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

            // ui.label(format!("focus: {:?}", self.focus));
            todo!();

            egui::ScrollArea::both()
                .auto_shrink([false, true])
                .scroll_source(egui::containers::scroll_area::ScrollSource::MOUSE_WHEEL)
                .show(ui, |ui| {
                    ui.style_mut().wrap_mode = Some(egui::TextWrapMode::Extend);
                    // self.render_tree(ui, ctx);
                    todo!();
                })
        });
    }
}
