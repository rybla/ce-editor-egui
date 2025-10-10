//! A simple egui app that demonstrates drag-and-drop interaction between labels.

use eframe::egui;
use egui::{Color32, Rect, RichText, Sense};

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Label Drag-and-Drop",
        options,
        Box::new(|_cc| Ok(Box::new(MyApp::new()))),
    )
}

struct MyApp {
    /// The list of items we are displaying.
    items: Vec<String>,
    /// The index of the item the user is currently dragging.
    drag_start_index: Option<usize>,
    /// The index of the item the user is currently hovering over while dragging.
    /// This is updated each frame.
    hovered_index: Option<usize>,
    /// The successfully connected start and end indices.
    connection: Option<(usize, usize)>,
}

impl MyApp {
    /// Initializes the application with some default items.
    fn new() -> Self {
        Self {
            items: (0..5).map(|i| format!("Label {}", i + 1)).collect(),
            drag_start_index: None,
            hovered_index: None,
            connection: None,
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        println!("self.hovered_index = {:?}", self.hovered_index);

        // --- State Update ---
        // First, handle the end of a drag operation.
        // We detect this when the primary mouse button is released.
        let is_pointer_released = ctx.input(|i| i.pointer.primary_released());
        let is_dragging = self.drag_start_index.is_some();

        if is_dragging && is_pointer_released {
            // A drag operation was just completed.
            if let (Some(start_index), Some(hovered_index)) =
                (self.drag_start_index, self.hovered_index)
            {
                // We have both a start and an end point, so a connection is made.
                if start_index != hovered_index {
                    self.connection = Some((start_index, hovered_index));
                }
            }
            // Clear the drag state regardless of whether a connection was made.
            self.drag_start_index = None;
            self.hovered_index = None;
        }

        // // Reset the hovered index at the beginning of each frame's UI rendering.
        // self.hovered_index = None;

        // --- UI Rendering ---
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Drag-and-Drop Connection");
            ui.label("Click and drag from one label to another.");
            ui.separator();

            // Display the current connection status.
            if let Some((start, end)) = self.connection {
                ui.label(
                    RichText::new(format!(
                        "Connected: {} -> {}",
                        self.items[start], self.items[end]
                    ))
                    .color(Color32::LIGHT_BLUE)
                    .strong(),
                );
            } else {
                ui.label("Connection: None");
            }
            ui.add_space(10.0);

            let mut label_rects: Vec<(Rect, usize)> = vec![];

            // Create a vertical layout for our labels.
            ui.vertical(|ui| {
                // Iterate over each item to create a label for it.
                for i in 0..self.items.len() {
                    let item_text = &self.items[i];
                    let mut rich_text = RichText::new(item_text).size(20.0);

                    // Determine the label's color based on the interaction state.
                    if Some(i) == self.drag_start_index {
                        rich_text = rich_text.color(Color32::GREEN).strong();
                    } else if Some(i) == self.hovered_index {
                        rich_text = rich_text.color(Color32::RED).strong();
                    }

                    // Create the label widget and make it sensitive to clicks and drags.
                    let response = ui.add(
                        egui::Label::new(rich_text)
                            .sense(Sense::click_and_drag())
                            .selectable(false),
                    );

                    label_rects.push((response.rect, i));

                    // --- Post-Render State Update ---
                    // After adding the widget, we check its response to update our state.
                    if response.drag_started() {
                        // The user started dragging this label (Step 2).
                        self.drag_start_index = Some(i);
                        // Clear any previous connection when starting a new drag.
                        self.connection = None;
                    }
                }
            });

            if self.drag_start_index.is_some() {
                self.hovered_index = None;
                if let Some(pointer_pos) = ui.ctx().pointer_latest_pos() {
                    for (rect, i) in label_rects.iter() {
                        if rect.contains(pointer_pos) {
                            self.hovered_index = Some(*i);
                        }
                    }
                }
            }
        });
    }
}
