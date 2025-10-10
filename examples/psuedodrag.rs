use eframe::egui;

/// Main function to run the eframe application.
fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        ..Default::default()
    };
    eframe::run_native(
        "Circle Connector",
        options,
        Box::new(|_cc| Ok(Box::<MyApp>::default())),
    )
}

/// Represents a single circle to be drawn and interacted with.
struct Circle {
    pos: egui::Pos2,
    radius: f32,
    label: String,
}

/// The main application state.
struct MyApp {
    /// A vector of all circles to be displayed.
    circles: Vec<Circle>,
    /// The index of the circle where a drag gesture started. `None` if no drag is active.
    drag_start_idx: Option<usize>,
    /// The final, successfully connected pair of circles, stored by their indices.
    connection: Option<(usize, usize)>,
}

/// Implementation of the `Default` trait to create the initial state of the app.
impl Default for MyApp {
    fn default() -> Self {
        Self {
            // Pre-populate with some circles at different positions.
            circles: vec![
                Circle {
                    pos: egui::pos2(100.0, 150.0),
                    radius: 30.0,
                    label: "Alpha".to_string(),
                },
                Circle {
                    pos: egui::pos2(500.0, 450.0),
                    radius: 30.0,
                    label: "Beta".to_string(),
                },
                Circle {
                    pos: egui::pos2(250.0, 400.0),
                    radius: 30.0,
                    label: "Gamma".to_string(),
                },
                Circle {
                    pos: egui::pos2(400.0, 200.0),
                    radius: 30.0,
                    label: "Delta".to_string(),
                },
            ],
            drag_start_idx: None,
            connection: None,
        }
    }
}

/// Implementation of the main application logic.
impl eframe::App for MyApp {
    /// This method is called on each frame to update the UI.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            // Display instructions and the result of the last connection at the top.
            ui.vertical_centered(|ui| {
                ui.heading("Circle Connector");
                if let Some((start_idx, end_idx)) = self.connection {
                    let start_label = &self.circles[start_idx].label;
                    let end_label = &self.circles[end_idx].label;
                    // 5. A UI element is updated with the labels.
                    ui.label(format!("Last connection: {} -> {}", start_label, end_label));
                } else {
                    ui.label("Drag from one circle to another to connect them.");
                }
            });
            ui.separator();

            // Allocate the rest of the panel for our custom painting and interaction.
            let (response, painter) =
                ui.allocate_painter(ui.available_size(), egui::Sense::click_and_drag());

            // --- Interaction Logic ---

            // 1. Detect which circle is currently being hovered over by the pointer.
            let mut hovered_circle_idx = None;
            if let Some(hover_pos) = response.hover_pos() {
                for (i, circle) in self.circles.iter().enumerate() {
                    if circle.pos.distance(hover_pos) <= circle.radius {
                        hovered_circle_idx = Some(i);
                        break;
                    }
                }
            }

            // 2. Handle the start of a drag.
            if response.drag_started() {
                // If the drag started on a circle, remember which one.
                self.drag_start_idx = hovered_circle_idx;
            }

            // 4. Handle the end of a drag.
            if response.drag_stopped() {
                if let (Some(start_idx), Some(end_idx)) = (self.drag_start_idx, hovered_circle_idx)
                {
                    // Ensure the drag ends on a different circle than it started.
                    if start_idx != end_idx {
                        // Store the successful connection.
                        self.connection = Some((start_idx, end_idx));
                    }
                }
                // End the drag regardless of whether it was successful.
                self.drag_start_idx = None;
            }

            // --- Drawing Logic ---

            // While dragging, draw a line from the center of the start circle to the pointer.
            if let Some(start_idx) = self.drag_start_idx {
                if let Some(hover_pos) = response.hover_pos() {
                    let start_circle = &self.circles[start_idx];
                    painter.line_segment(
                        [start_circle.pos, hover_pos],
                        egui::Stroke::new(2.0, egui::Color32::WHITE),
                    );
                }
            }

            // Draw all the circles, changing their color based on the interaction state.
            for (i, circle) in self.circles.iter().enumerate() {
                let is_drag_start = self.drag_start_idx == Some(i);
                let is_drag_target = self.drag_start_idx.is_some()
                    && hovered_circle_idx == Some(i)
                    && !is_drag_start;

                // Determine the fill color based on the interaction state.
                let fill_color = if is_drag_start {
                    egui::Color32::GREEN // 2. The circle being dragged from is green.
                } else if is_drag_target {
                    egui::Color32::RED // 3. The circle being dragged to is red.
                } else {
                    ui.visuals().widgets.inactive.bg_fill // Default color.
                };

                painter.circle_filled(circle.pos, circle.radius, fill_color);
                painter.text(
                    circle.pos,
                    egui::Align2::CENTER_CENTER,
                    &circle.label,
                    egui::FontId::proportional(20.0),
                    ui.visuals().strong_text_color(),
                );
            }
        });
    }
}
