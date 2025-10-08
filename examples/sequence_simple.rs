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
    elements: Vec<SequenceElement>,
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
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::both().show(ui, |ui| {
                ui.with_layout(
                    Layout::left_to_right(Align::TOP)
                        .with_main_wrap(true)
                        .with_main_justify(false)
                        .with_cross_justify(false),
                    |ui| {
                        ui.spacing_mut().item_spacing = Vec2::ZERO;
                        let text_height = ui.text_style_height(&egui::TextStyle::Body);

                        for (i, e) in self.elements.iter().enumerate() {
                            match e {
                                SequenceElement::Label(text) => {
                                    ui.set_row_height(text_height);

                                    let color = color_for_index(i);
                                    Frame::default()
                                        .inner_margin(egui::Margin::symmetric(4, 2))
                                        .outer_margin(egui::Margin::ZERO)
                                        .corner_radius(CornerRadius::ZERO)
                                        .fill(color)
                                        .show(ui, |ui| {
                                            ui.add(
                                                egui::Label::new(text)
                                                    .wrap_mode(egui::TextWrapMode::Extend),
                                            )
                                        });
                                }
                                SequenceElement::LineBreak => {
                                    // ui.end_row() forces a new line in a wrapping layout.
                                    ui.end_row();
                                    ui.set_row_height(text_height);
                                }
                            }
                        }
                    },
                );
            });
        });
    }
}

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
