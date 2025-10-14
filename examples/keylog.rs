use eframe::egui;

fn main() -> Result<(), eframe::Error> {
    let options = Default::default();
    eframe::run_native(
        "Key Press Viewer",
        options,
        Box::new(|_cc| Ok(Box::<KeyPressApp>::default())),
    )
}

#[derive(Default)]
struct KeyPressApp {
    /// Stores the last key press event: (key, modifiers).
    last_key_press: Option<(egui::Key, egui::Modifiers)>,
}

impl eframe::App for KeyPressApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // On each frame, check for input events.
        ctx.input(|i| {
            // Iterate through all events, looking for key presses.
            for event in &i.events {
                if let egui::Event::Key {
                    key,
                    pressed: true, // We only care about key presses, not releases.
                    modifiers,
                    ..
                } = event
                {
                    // If a key is pressed, update our state with the key and its modifiers.
                    self.last_key_press = Some((*key, *modifiers));
                }
            }
        });

        // The main UI display.
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Last Key Press");
            ui.separator();

            if let Some((key, modifiers)) = self.last_key_press {
                // Use a grid for clean alignment of labels.
                egui::Grid::new("key_info_grid")
                    .num_columns(2)
                    .spacing([40.0, 4.0])
                    .show(ui, |ui| {
                        ui.label("Key:");
                        ui.label(format!("{key:?}"));
                        ui.end_row();

                        ui.label("Modifiers:");
                        ui.label(format_modifiers(modifiers));
                        ui.end_row();
                    });
            } else {
                ui.label("Press any key...");
            }
        });
    }
}

/// A helper function to format the modifiers in a user-friendly way.
fn format_modifiers(modifiers: egui::Modifiers) -> String {
    if modifiers.is_none() {
        return "None".to_owned();
    }
    let mut parts = vec![];
    if modifiers.alt {
        parts.push("Alt");
    }
    if modifiers.ctrl {
        parts.push("Ctrl");
    }
    if modifiers.shift {
        parts.push("Shift");
    }
    if modifiers.mac_cmd {
        parts.push("⌘");
    }
    if modifiers.command {
        parts.push("Command");
    }
    parts.join(" + ")
}
