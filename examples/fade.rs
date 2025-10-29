use eframe::egui;

/// Main function to run the eframe application
fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([400.0, 300.0]),
        ..Default::default()
    };

    eframe::run_native(
        "Fade Demo",
        options,
        Box::new(|cc| {
            cc.egui_ctx.set_style({
                let mut style = (*cc.egui_ctx.style()).clone();
                style.animation_time = 1.0;
                style
            });
            Ok(Box::new(MyApp::default()))
        }),
    )
}

/// The application state
#[derive(Default)]
struct MyApp {
    /// Whether the frame should be visible or not
    show_frame: bool,
}

/// Implementation of the [`eframe::App`] trait
impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Frame Fade-in/Fade-out Demo");
            ui.add_space(10.0);

            // 1. A button to toggle the boolean state
            if ui.button("Toggle Frame").clicked() {
                self.show_frame = !self.show_frame;
            }
            ui.label(format!(
                "Frame should be: {}",
                if self.show_frame { "Visible" } else { "Hidden" }
            ));

            ui.add_space(20.0);

            // 2. Get the animated progress
            // ctx.animate_bool() takes a unique Id and the bool to animate.
            // It returns a f32 that smoothly animates between 0.0 (false) and 1.0 (true).
            // egui automatically requests repaints while the animation is running.
            let animation_progress =
                ctx.animate_bool(ui.id().with("fade_animation"), self.show_frame);

            // 3. Set the opacity for all widgets within this scope
            // ui.set_opacity() will affect all widgets added to this `ui`
            // and any child UIs.
            ui.set_opacity(animation_progress);

            // 4. Only draw the frame if it's not fully invisible.
            // This prevents it from taking up space and being interactable
            // when fully faded out.
            if animation_progress > 0.0 {
                // 5. Draw the frame
                // We use a simple `egui::Frame` here.
                egui::Frame::window(&ctx.style())
                    .inner_margin(egui::Margin::same(20))
                    .show(ui, |ui| {
                        ui.label("Hello!");
                        ui.label("This whole frame fades in and out.");
                        ui.label(format!("Current Opacity: {animation_progress:.2}"));
                        ui.horizontal(|ui| {
                            ui.label("Even buttons:");
                            if ui.button("Click me!").clicked() {
                                self.show_frame = !self.show_frame;
                            }
                        });
                    });
            }
        });
    }
}
