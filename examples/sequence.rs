//! A demo showing a wrapping horizontal layout with colored labels.

use eframe::egui;
use egui::{Align, Color32, CornerRadius, Frame, Layout, Vec2, epaint::Hsva};

/// Represents the different types of elements we can have in our sequence.
#[derive(Debug, Clone)]
enum SequenceElement {
    /// A text label with its content.
    Label(String),
    /// An element that forces a line break in the layout.
    LineBreak,
}

/// The main application state.
struct MyApp {
    /// The sequence of elements to be displayed.
    elements: Vec<SequenceElement>,
    /// A counter to generate unique label text.
    label_counter: usize,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            // Start with some default elements to demonstrate the layout.
            elements: vec![
                SequenceElement::Label("Hello".to_string()),
                SequenceElement::Label("World!".to_string()),
                SequenceElement::LineBreak,
                SequenceElement::Label("This".to_string()),
                SequenceElement::Label("is".to_string()),
                SequenceElement::Label("a".to_string()),
                SequenceElement::Label("wrapping".to_string()),
                SequenceElement::Label("layout.".to_string()),
                SequenceElement::Label("Labels".to_string()),
                SequenceElement::Label("with".to_string()),
                SequenceElement::Label("different".to_string()),
                SequenceElement::Label("lengths".to_string()),
                SequenceElement::Label("will".to_string()),
                SequenceElement::Label("wrap".to_string()),
                SequenceElement::Label("to".to_string()),
                SequenceElement::Label("the".to_string()),
                SequenceElement::Label("next".to_string()),
                SequenceElement::Label("line.".to_string()),
            ],
            label_counter: 0,
        }
    }
}

/// Generates a deterministic, visually distinct color for a given index.
fn color_for_index(i: usize) -> Color32 {
    let i = i as f32;
    // Use the golden angle approximation to generate evenly distributed hues.
    let hue = (i * 0.381).fract();
    Color32::from(Hsva::new(hue, 0.7, 0.8, 1.0))
}

impl eframe::App for MyApp {
    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // The CentralPanel is the main content area of the window.
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Wrapping Layout Demo");
            ui.label("This demo shows labels with different background colors in a wrapping horizontal layout. The colored blocks are perfectly aligned next to each other.");

            // --- Controls ---
            ui.horizontal(|ui| {
                if ui.button("Add Label").clicked() {
                    self.elements
                        .push(SequenceElement::Label(format!("Label {}", self.label_counter)));
                    self.label_counter += 1;
                }
                if ui.button("Add Long Label").clicked() {
                    self.elements.push(SequenceElement::Label(
                        "This is a much longer label".to_owned(),
                    ));
                }
                if ui.button("Add Line Break").clicked() {
                    self.elements.push(SequenceElement::LineBreak);
                }
                if ui.button("Clear All").clicked() {
                    self.elements.clear();
                    self.label_counter = 0;
                }
            });

            ui.separator();

            // --- Content Area ---
            // Use a dark canvas Frame as a background for the content.
            Frame::dark_canvas(ui.style()).show(ui, |ui| {
                // Define the wrapping layout.
                let layout = Layout::left_to_right(Align::TOP).with_main_wrap(true);

                // Wrap the content in a ScrollArea for both horizontal and vertical scrolling.
                egui::ScrollArea::both().show(ui, |ui| {ui.with_layout(layout, |ui| {
                    // Set item_spacing to zero to make elements touch.
                    ui.spacing_mut().item_spacing = Vec2::ZERO;

                    for (i, element) in self.elements.iter().enumerate() {
                        let color = color_for_index(i);

                        match element {
                            SequenceElement::Label(text) => {
                                // For each label, create a Frame for its background.
                                Frame::default()
                                    // Add some padding inside the colored box for the text.
                                    .inner_margin(egui::Margin::symmetric(4, 2))
                                    // No margin outside the box, so frames touch.
                                    .outer_margin(egui::Margin::ZERO)
                                    // Use square corners.
                                    .corner_radius(CornerRadius::ZERO)
                                    // Set the background color.
                                    .fill(color)
                                    .show(ui, |ui| {
                                        ui.label(text);
                                    });
                            }
                            SequenceElement::LineBreak => {
                                // ui.end_row() forces a new line in a wrapping layout.
                                ui.end_row();
                            }
                        }
                    }
                });})
            });
        });
    }
}

/// Main function to run the eframe application.
fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        ..Default::default()
    };
    eframe::run_native(
        "Wrapping Layout Demo",
        options,
        Box::new(|_cc| Ok(Box::<MyApp>::default())),
    )
}
