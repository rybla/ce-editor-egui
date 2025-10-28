use crate::{ex, expr::*, span, utility::is_single_char};
use egui;
use lazy_static::lazy_static;
use log::{trace, warn};
use nucleo_matcher::Matcher;
use std::{
    cell::Cell,
    fmt::{Debug, Display},
    marker::PhantomData,
};

pub const INDENT_WIDTH_EM: f32 = 1.0;

#[derive(Clone)]
pub struct ColorScheme {
    pub normal_border: egui::Color32,
    pub normal_text: egui::Color32,
    pub ghost_text: egui::Color32,
    pub keyword_text: egui::Color32,
    pub accent_text: egui::Color32,
    pub normal_background: egui::Color32,
    pub active_text: egui::Color32,
    pub active_background: egui::Color32,
    pub inactive_text: egui::Color32,
    pub inactive_background: egui::Color32,
    pub highlight_background: egui::Color32,
    pub error_background: egui::Color32,
    pub error_text: egui::Color32,
}

lazy_static! {
    pub static ref dark_color_scheme: ColorScheme = ColorScheme {
        accent_text: egui::Color32::RED,
        keyword_text: egui::Color32::BLUE,
        normal_text: egui::Color32::WHITE,
        ghost_text: egui::Color32::GRAY,
        normal_background: egui::Color32::BLACK,
        normal_border: egui::Color32::WHITE,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::BLUE,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::DARK_BLUE,
        highlight_background: egui::Color32::DARK_BLUE,
        error_background: egui::Color32::RED,
        error_text: egui::Color32::WHITE,
    };
    pub static ref light_color_scheme: ColorScheme = ColorScheme {
        accent_text: egui::Color32::RED,
        keyword_text: egui::Color32::BLUE,
        normal_text: egui::Color32::BLACK,
        ghost_text: egui::Color32::GRAY,
        normal_background: egui::Color32::WHITE,
        normal_border: egui::Color32::BLACK,
        active_text: egui::Color32::WHITE,
        active_background: egui::Color32::BLUE,
        inactive_text: egui::Color32::WHITE,
        inactive_background: egui::Color32::LIGHT_BLUE,
        highlight_background: egui::Color32::LIGHT_BLUE,
        error_background: egui::Color32::RED,
        error_text: egui::Color32::WHITE,
    };
}

#[derive(Default)]
pub struct MutDiagnostics(pub Cell<Vec<Diagnostic>>);

impl Debug for MutDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ds = self.0.take();
        let r = f.debug_tuple("Diagnostics").field(&ds).finish();
        self.0.set(ds);
        r
    }
}

// impl Clone for Diagnostics {
//     fn clone(&self) -> Self {
//         let ds = self.0.take();
//         let ds_clone = ds.clone();
//         self.0.set(ds);
//         Self(Cell::new(ds_clone))
//     }
// }

#[derive(Debug, Clone)]
pub struct GenEditorLabel<D> {
    pub constructor: Constructor,
    pub diagnostics: D,
}

impl<D> Display for GenEditorLabel<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constructor)
    }
}

pub type DiagEditorLabel = GenEditorLabel<MutDiagnostics>;
pub type PlainEditorLabel = GenEditorLabel<()>;

impl<D> GenEditorLabel<D> {
    pub fn new(constructor: Constructor, diagnostics: D) -> Self {
        Self {
            constructor,
            diagnostics,
        }
    }

    pub fn into_plain(self) -> PlainEditorLabel {
        GenEditorLabel {
            constructor: self.constructor,
            diagnostics: (),
        }
    }

    pub fn to_plain(&self) -> PlainEditorLabel {
        GenEditorLabel {
            constructor: self.constructor.clone(),
            diagnostics: (),
        }
    }

    pub fn to_diag(&self) -> DiagEditorLabel {
        GenEditorLabel {
            constructor: self.constructor.clone(),
            diagnostics: MutDiagnostics::default(),
        }
    }

    pub fn into_diag(self) -> DiagEditorLabel {
        GenEditorLabel {
            constructor: self.constructor,
            diagnostics: MutDiagnostics::default(),
        }
    }
}

impl Expr<DiagEditorLabel> {
    pub fn pat2(&self) -> (&Constructor, &[Self]) {
        (&self.label.constructor, self.kids.0.as_slice())
    }
}

// A GenEditorExpr is expected to always conform to the following grammar:
// e = (constructor [PosArg [d]])
// d = newline | e
pub type GenEditorExpr<D> = Expr<GenEditorLabel<D>>;
pub type DiagExpr = GenEditorExpr<MutDiagnostics>;
pub type PlainExpr = GenEditorExpr<()>;

impl PlainExpr {
    pub fn into_annotated(self) -> DiagExpr {
        self.map_owned(&mut |l| GenEditorLabel {
            constructor: l.constructor,
            diagnostics: MutDiagnostics::default(),
        })
    }
}

#[macro_export]
macro_rules! editor_ex {
    ( $label:expr, [ $( $e:expr ),* ] ) => {
        Expr {
            label: GenEditorLabel { constructor: Constructor::Literal($label.to_owned()), diagnostics: () },
            kids: Span(vec![ $( $e ),* ]),
        }
    };
}

impl<D> GenEditorExpr<D> {
    pub fn new_pos_arg(kids: Vec<Self>) -> Self
    where
        D: Default,
    {
        Self {
            label: GenEditorLabel {
                constructor: Constructor::PosArg,
                diagnostics: Default::default(),
            },
            kids: Span(kids),
        }
    }

    pub fn new_lit(lit: String, kids: Vec<Self>) -> Self
    where
        D: Default,
    {
        Self {
            label: GenEditorLabel {
                constructor: Constructor::Literal(lit),
                diagnostics: Default::default(),
            },
            kids: Span(kids),
        }
    }

    pub fn pat_pos_arg(&self) -> &[Self] {
        match self.label.constructor {
            Constructor::PosArg => self.kids.0.as_slice(),
            _ => panic!("expected a PostArg expr"),
        }
    }

    // this pat function assumes that the program is stored as a particular kind of structure
    // where each node has a list of positional arguments, each of which has a list of expressions
    // this returns the list of spots.
    pub fn pat_literal(&self) -> (&str, &[Self]) {
        (
            match &self.label.constructor {
                Constructor::Literal(s) => s.as_str(),
                c => panic!("invalid expr: {c:?}"),
            },
            self.kids.0.as_slice(),
        )
    }
    pub fn to_plain(&self) -> PlainExpr {
        self.map(&mut |l| l.to_plain())
    }

    pub fn into_plain(self) -> PlainExpr {
        self.map_owned(&mut |l| l.into_plain())
    }

    pub fn to_diag(&self) -> DiagExpr {
        self.map(&mut |l| l.to_diag())
    }

    pub fn into_diag(self) -> DiagExpr {
        self.map_owned(&mut |l| l.into_diag())
    }
}

impl DiagExpr {
    // clones but without cloning Cells, and just makes new Cells
    pub fn clone_without_diagnostics(&self) -> Self {
        let Self {
            label: DiagEditorLabel { constructor, .. },
            kids,
        } = self;
        Self {
            label: DiagEditorLabel {
                constructor: constructor.clone(),
                diagnostics: MutDiagnostics(Cell::new(vec![])),
            },
            kids: Span(
                kids.0
                    .iter()
                    .map(|x| x.clone_without_diagnostics())
                    .collect(),
            ),
        }
    }

    pub fn clear_diagnostics(&self) {
        let mut annotation = self.label.diagnostics.0.take();
        annotation.clear();
        self.label.diagnostics.0.set(annotation);
    }

    pub fn add_diagnostic(&self, d: Diagnostic) {
        println!("add_diagnostic: {d:?}");
        let mut annotation = self.label.diagnostics.0.take();
        annotation.push(d);
        self.label.diagnostics.0.set(annotation);
    }
}

pub type DiagFragment = Fragment<GenEditorLabel<MutDiagnostics>>;
pub type PlainFragment = Fragment<GenEditorLabel<()>>;

impl DiagFragment {
    pub fn into_plain(self) -> PlainFragment {
        match self {
            Self::Span(span) => Fragment::Span(span.into_plain()),
            Self::Zipper(zipper) => Fragment::Zipper(zipper.into_plain()),
        }
    }
}

pub type DiagSpan = Span<GenEditorLabel<MutDiagnostics>>;
pub type PlainSpan = Span<GenEditorLabel<()>>;

impl DiagSpan {
    pub fn into_plain(self) -> PlainSpan {
        Span(self.0.into_iter().map(|e| e.into_plain()).collect())
    }
}

pub type DiagZipper = Zipper<GenEditorLabel<MutDiagnostics>>;
pub type PlainZipper = Zipper<GenEditorLabel<()>>;

impl DiagZipper {
    pub fn into_plain(self) -> PlainZipper {
        Zipper {
            span_ol: self.span_ol.into_plain(),
            span_or: self.span_or.into_plain(),
            middle: self.middle.into_plain(),
        }
    }
}

pub type DiagContext = Context<GenEditorLabel<MutDiagnostics>>;
pub type PlainContext = Context<GenEditorLabel<()>>;

impl DiagContext {
    pub fn into_plain(self) -> PlainContext {
        Context(self.0.into_iter().map(|th| th.into_plain()).collect())
    }
}

pub type DiagTooth = Tooth<GenEditorLabel<MutDiagnostics>>;
pub type PlainTooth = Tooth<GenEditorLabel<()>>;

impl DiagTooth {
    pub fn into_plain(self) -> PlainTooth {
        Tooth {
            label: self.label.into_plain(),
            span_l: self.span_l.into_plain(),
            span_r: self.span_r.into_plain(),
        }
    }
}

pub struct EditorState<ES: EditorSpec + ?Sized> {
    pub spec: PhantomData<ES>,
    pub core: CoreState<MutDiagnostics>,
    pub menu: Option<EditMenu>,
    pub history: Vec<PlainCoreState>,
    pub future: Vec<PlainCoreState>,
    pub drag_origin: Option<Handle>,
}

impl<ES: EditorSpec + ?Sized> Default for EditorState<ES> {
    fn default() -> Self {
        let state: CoreState<MutDiagnostics> = {
            let state = ES::initial_state();
            let state: CoreState<MutDiagnostics> = CoreState {
                root: state.root.into_annotated(),
                handle: state.handle,
                clipboard: state.clipboard,
            };
            trace!(target: "diagnose", "BEGIN diagnose");
            ES::diagnose(&state);
            trace!(target: "diagnose", "END diagnose");
            state
        };
        Self {
            spec: PhantomData,
            core: state,
            menu: Default::default(),
            history: vec![],
            future: vec![],
            drag_origin: Default::default(),
        }
    }
}

struct ModKey {
    pub key: egui::Key,
    pub mods: egui::Modifiers,
}

impl Display for ModKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            if self.mods.shift { "Shift " } else { "" },
            if self.mods.alt { "Alt " } else { "" },
            if self.command() { "Cmd " } else { "" },
            self.key.symbol_or_name()
        )
    }
}

impl ModKey {
    pub fn command(&self) -> bool {
        self.mods.command || self.mods.mac_cmd || self.mods.ctrl
    }

    pub fn shift(&self) -> bool {
        self.mods.shift
    }

    pub fn alt(&self) -> bool {
        self.mods.alt
    }

    pub fn from_event(e: &egui::Event) -> Option<Self> {
        if let egui::Event::Key {
            key,
            pressed: true,
            modifiers,
            ..
        } = e
        {
            Some(Self {
                key: *key,
                mods: *modifiers,
            })
        } else {
            None
        }
    }
}

fn test_mod_key(opt_mod_key: &Option<ModKey>, f: impl Fn(&ModKey) -> bool) -> bool {
    match opt_mod_key {
        Some(mod_key) => f(mod_key),
        None => false,
    }
}

impl<ES: EditorSpec> EditorState<ES> {
    pub fn escape(&mut self) {
        if self.menu.is_some() {
            self.menu = None;
        } else {
            // TODO: unify this into Action enumeration
            self.core
                .handle
                .escape()
                .unwrap_or_else(|e| trace!(target: "editor.move", "escape failed: {e:?}"));
        }
    }

    pub fn do_edit(
        &mut self,
        edit: impl Fn(&str, DiagCoreState) -> Option<DiagCoreState>,
        query: &str,
    ) {
        let opt_core = (edit)(
            query,
            DiagCoreState {
                root: self.core.root.to_diag(),
                handle: self.core.handle.clone(),
                clipboard: self.core.clipboard.clone(),
            },
        );
        if let Some(core) = opt_core {
            self.do_action(Action::SetCore(core));
        } else {
            trace!(target: "editor.edit", "edit failed");
        }
    }

    pub fn update(&mut self, ctx: &egui::Context) {
        trace!(target: "editor.update", "update");

        if self.drag_origin.is_some() && ctx.input(|i| i.pointer.primary_released()) {
            trace!(target: "editor.drag", "end drag; h = {}", self.core.handle);
            self.drag_origin = None;
        }

        let mut opt_mod_key: Option<ModKey> = None;
        ctx.input(|i| {
            for e in &i.events {
                if let Some(mk) = ModKey::from_event(e) {
                    opt_mod_key = Some(mk);
                }
            }
        });

        trace!(target: "key", "{}", match &opt_mod_key {
            Some(mod_key) => format!("{mod_key}"),
            None => "None".to_owned(),
        });

        if false {
            // this is just a placeholder so the subsequent branches can all use `else if` syntax
        }
        // escape
        else if test_mod_key(&opt_mod_key, |mk| {
            !mk.command() && mk.key == egui::Key::Escape
        }) {
            self.escape();
        }
        // when edit menu is open
        else if let Some(menu) = &mut self.menu {
            if false {
            }
            // submit menu option
            else if test_mod_key(&opt_mod_key, |mk| {
                !mk.command()
                    && !mk.shift()
                    && (mk.key == egui::Key::Tab
                        || mk.key == egui::Key::Enter
                        || mk.key == egui::Key::Space)
            }) {
                if let Some(option) = menu.focus_option() {
                    // NOTE: this should be the same as
                    // self.do_edit(&option.edit, &menu.query), but
                    // unfortunately ownership prevents me from calling
                    // self.do_edit since that borrows self as mutable
                    let opt_core = (option.edit)(
                        &menu.query,
                        DiagCoreState {
                            root: self.core.root.to_diag(),
                            handle: self.core.handle.clone(),
                            clipboard: self.core.clipboard.clone(),
                        },
                    );
                    if let Some(core) = opt_core {
                        self.do_action(Action::SetCore(core));
                    } else {
                        trace!(target: "editor.edit", "edit failed");
                    }
                } else {
                    trace!(target: "editor.edit", "there are no edit options available");
                }
            }
            // move menu option
            else if let Some(dir) = match_input_cycle_dir(ctx) {
                match dir {
                    CycleDir::Prev => menu.index -= 1,
                    CycleDir::Next => menu.index += 1,
                }
                menu.update(false);
            }
        }
        // undo
        else if test_mod_key(&opt_mod_key, |mk| {
            mk.command() && !mk.shift() && mk.key == egui::Key::Z
        }) {
            self.do_action(Action::Undo);
        }
        // redo
        else if test_mod_key(&opt_mod_key, |mk| {
            mk.command() && mk.shift() && mk.key == egui::Key::Z
        }) {
            self.do_action(Action::Redo);
        }
        // delete
        else if test_mod_key(&opt_mod_key, |mk| {
            !mk.command()
                && !mk.shift()
                && (mk.key == egui::Key::Delete || mk.key == egui::Key::Backspace)
        }) {
            self.do_action(Action::Delete);
        }
        // cut
        else if test_mod_key(&opt_mod_key, |mk| {
            mk.command() && !mk.shift() && mk.key == egui::Key::X
        }) {
            self.do_action(Action::Cut);
        }
        // copy
        else if test_mod_key(&opt_mod_key, |mk| {
            mk.command() && !mk.shift() && mk.key == egui::Key::C
        }) {
            self.do_action(Action::Copy);
        }
        // paste
        else if test_mod_key(&opt_mod_key, |mk| {
            mk.command() && !mk.shift() && mk.key == egui::Key::V
        }) {
            self.do_action(Action::Paste);
        }
        // newline
        else if test_mod_key(&opt_mod_key, |mk| {
            !mk.command() && !mk.shift() && mk.key == egui::Key::Enter
        }) {
            self.do_edit(insert_newline, "");
        }
        // rotate focus
        else if test_mod_key(&opt_mod_key, |mk| mk.command())
            && let Some(dir) = match_input_move_dir(ctx)
        {
            self.core.handle.rotate_focus_dir(&dir);
        }
        // select
        else if test_mod_key(&opt_mod_key, |mk| !mk.command() && mk.shift())
            && let Some(dir) = match_input_move_dir(ctx)
        {
            let mut target = self.core.handle.focus_point();
            loop {
                let move_status = target.move_dir(&self.core.root, &dir);
                if move_status.is_err() {
                    trace!(target: "editor.move", "select bailed since a move failed");
                    break;
                }

                if let Some(h) = self.core.handle.clone().drag(&self.core.root, &target)
                    && ES::is_valid_handle(&h, &self.core.root)
                {
                    self.do_action(Action::SetHandle(SetHandle {
                        handle: h,
                        snapshot: false,
                    }));
                    break;
                }
            }
        }
        // move
        else if test_mod_key(&opt_mod_key, |mk| !mk.command() && !mk.shift())
            && let Some(dir) = match_input_move_dir(ctx)
        {
            let mut origin = self.core.handle.clone();
            #[expect(unused_assignments)]
            let mut move_status = Ok(());
            loop {
                move_status = origin.move_dir(&self.core.root, &dir);
                if let Err(err) = &move_status {
                    trace!(target: "editor.move", "move bailed since a move failed: {err:?}");
                    break;
                }

                if ES::is_valid_handle(&origin, &self.core.root) {
                    break;
                }
            }
            if move_status.is_ok() {
                trace!(target: "editor.move", "move succeeded: {origin}");
                self.do_action(Action::SetHandle(SetHandle {
                    handle: origin.clone(),
                    snapshot: false,
                }));
            }
        }
        // move up
        else if test_mod_key(&opt_mod_key, |mk| {
            !mk.command() && !mk.shift() && mk.key == egui::Key::ArrowUp
        }) {
            let mut origin = self.core.handle.clone();
            loop {
                let move_status = origin.move_up(&self.core.root);
                if move_status.is_err() {
                    trace!(target: "editor.move", "move bailed since a move failed");
                    break;
                }

                if ES::is_valid_handle_specialized(&self.core.handle, &self.core.root) {
                    break;
                }
            }
            self.do_action(Action::SetHandle(SetHandle {
                handle: origin.clone(),
                snapshot: false,
            }));
        }
        // open edit menu (without query)
        else if test_mod_key(&opt_mod_key, |mk| {
            !mk.command() && !mk.shift() && mk.key == egui::Key::Space
        }) {
            trace!(target: "editor.edit", "open EditMenu");
            let menu = ES::get_edits(self);
            self.menu = Some(EditMenu::new(menu));
        }
        // open edit menu (with query)
        else if let Some(mk) = opt_mod_key
            && !mk.command()
            && is_single_char(mk.key.symbol_or_name())
        {
            trace!(target: "editor.edit", "open EditMenu");
            let menu = ES::get_edits(self);
            let mut s = mk.key.symbol_or_name().to_owned();
            if mk.shift() {
                s = s.to_uppercase();
            } else {
                s = s.to_lowercase();
            }
            self.menu = Some(EditMenu::new_with_query(menu, s));
        }
    }

    pub fn color_scheme(ui: &egui::Ui) -> &'static ColorScheme {
        match ui.ctx().theme() {
            egui::Theme::Dark => &dark_color_scheme,
            egui::Theme::Light => &light_color_scheme,
        }
    }

    pub fn render(&mut self, ctx: &egui::Context, ui: &mut egui::Ui) {
        egui::Frame::new().show(ui, |ui| {
            egui::ScrollArea::both().show(ui, |ui| {
                ui.with_layout(
                    egui::Layout::left_to_right(egui::Align::TOP).with_main_wrap(true),
                    |ui| {
                        ui.spacing_mut().item_spacing = egui::Vec2::ZERO;
                        ui.set_row_height(ui.text_style_height(&egui::TextStyle::Body));

                        let mut actions = vec![];
                        render_expr::<ES>(
                            ctx,
                            ui,
                            RenderContext {
                                interactive: true,
                                indent_level: 0,
                                color_scheme: Self::color_scheme(ui),
                                handle: &self.core.handle,
                                root: &self.core.root,
                                drag_origin: &self.drag_origin,
                                menu: &mut self.menu,
                                actions: &mut actions,
                            },
                            &Path::empty(),
                            &self.core.root,
                        );
                        for action in actions {
                            self.do_action(action);
                        }
                    },
                )
            })
        });
    }

    pub fn do_action(&mut self, action: Action) {
        match action {
            Action::Redo => {
                if let Some(core) = self.future.pop() {
                    self.history.push(self.core.to_plain());
                    self.menu = None;
                    self.core = core.to_diag();
                    ES::diagnose(&self.core);
                } else {
                    warn!(target: "editor", "failed to redo since future was empty");
                }
            }
            Action::Undo => {
                if let Some(core) = self.history.pop() {
                    self.future.push(self.core.to_plain());
                    self.menu = None;
                    self.core = core.to_diag();
                    ES::diagnose(&self.core);
                } else {
                    warn!(target: "editor", "failed to undo since history was empty");
                }
            }
            Action::Copy => {
                if let Some(frag) = self.core.root.get_fragment(&self.core.handle) {
                    let clipboard = Some(frag.map_to_owned(&mut |l| l.to_plain()));
                    self.snapshot();
                    self.core.clipboard = clipboard;
                } else {
                    warn!(target: "editor", "failed to copy");
                }
            }
            Action::Paste => {
                if let Some(frag) = &self.core.clipboard {
                    let frag = frag.to_ref().map_to_owned(&mut |l| l.to_diag());
                    self.snapshot();
                    self.menu = None;
                    let handle = self
                        .core
                        .root
                        .insert_fragment(self.core.handle.clone(), frag);
                    self.core.handle = handle;
                    ES::diagnose(&self.core);
                } else {
                    warn!(target: "editor", "failed to paste since clipboard was empty");
                }
            }
            Action::Cut => {
                self.snapshot();
                if let Some((frag, h)) = self.core.root.cut(&self.core.handle) {
                    self.menu = None;
                    self.core.clipboard = Some(frag.into_plain());
                    self.core.handle = h;
                    ES::diagnose(&self.core);
                } else {
                    warn!(target: "editor", "failed to cut");
                }
            }
            Action::Delete => {
                self.snapshot();
                if let Some((_frag, h)) = self.core.root.cut(&self.core.handle) {
                    self.menu = None;
                    self.core.handle = h;
                    ES::diagnose(&self.core);
                } else {
                    warn!(target: "editor", "failed to delete");
                }
            }
            Action::SetHandle(args) => {
                if args.snapshot {
                    self.snapshot();
                }
                self.menu = None;
                self.core.handle = args.handle;
            }
            Action::SetCore(core) => {
                self.snapshot();
                self.menu = None;
                self.core = core;
                ES::diagnose(&self.core);
            }
            Action::SetDragOrigin(handle) => {
                self.drag_origin = handle;
            }
        }
    }

    pub fn snapshot(&mut self) {
        self.history.push(self.core.to_plain());
        self.future = vec![];
    }
}

#[expect(clippy::needless_pass_by_value)]
pub fn render_point<ES: EditorSpec>(
    ctx: &egui::Context,
    ui: &mut egui::Ui,
    rc: RenderContext<'_>,
    p: &Point,
) {
    let handle_at_point = Handle::Point(p.clone());
    let is_valid_handle = ES::is_valid_handle(&handle_at_point, rc.root);

    if is_valid_handle {
        let is_handle = *p == rc.handle.focus_point();

        let is_at_handle = match rc.handle {
            Handle::Point(handle) => p == handle,
            Handle::Span(handle) => p == &handle.p_l() || p == &handle.p_r(),
            Handle::Zipper(handle) => {
                p == &handle.p_ol()
                    || p == &handle.p_or()
                    || p == &handle.p_il()
                    || p == &handle.p_ir()
            }
        };

        let is_in_handle = rc.handle.contains_point(p);

        let fill_color = if is_handle {
            rc.color_scheme.active_background
        } else if is_at_handle {
            rc.color_scheme.inactive_background
        } else if is_in_handle {
            rc.color_scheme.highlight_background
        } else {
            rc.color_scheme.normal_background
        };

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            let label = ui.add(
                egui::Label::new(
                    egui::RichText::new("•".to_owned()).color(rc.color_scheme.ghost_text),
                )
                .selectable(false)
                .sense(egui::Sense::click_and_drag()),
            );

            if rc.interactive {
                if label.clicked() {
                    trace!(target: "editor", "click at point: p = {p}");
                    let h = Handle::Point(p.clone());
                    rc.actions.push(Action::SetHandle(SetHandle {
                        handle: h,
                        snapshot: false,
                    }));
                }

                if label.drag_started_by(egui::PointerButton::Primary) {
                    trace!(target: "editor.drag", "drag start at point: p = {p}");
                    rc.actions.push(Action::SetHandle(SetHandle {
                        handle: Handle::Point(p.clone()),
                        snapshot: false,
                    }));
                    rc.actions
                        .push(Action::SetDragOrigin(Some(Handle::Point(p.clone()))));
                } else if let Some(drag_origin) = rc.drag_origin
                    && let Some(pointer_pos) = ctx.pointer_latest_pos()
                    && label.rect.contains(pointer_pos)
                {
                    trace!(target: "editor.drag", "drag hover at point: p = {p}");
                    if let Some(h) = drag_origin.clone().drag(rc.root, p) {
                        rc.actions.push(Action::SetHandle(SetHandle {
                            handle: h.clone(),
                            snapshot: false,
                        }));
                    }
                }
            }

            if is_handle && let Some(menu) = rc.menu {
                let menu_width = 100f32;
                let menu_query_height = ui.text_style_height(&egui::TextStyle::Body);
                let menu_options_height = 80f32;
                let menu_height = menu_query_height + menu_options_height;

                // render edit menu stuff
                egui::Frame::new()
                    .fill(rc.color_scheme.normal_background)
                    .outer_margin(egui::Margin {
                        left: 0i8,
                        right: 5i8,
                        top: 0i8,
                        bottom: 5i8,
                    })
                    .show(ui, |ui| {
                        ui.with_layout(egui::Layout::top_down_justified(egui::Align::TOP), |ui| {
                            ui.set_width(menu_width);
                            ui.set_height(menu_height);
                            ui.spacing_mut().item_spacing = egui::Vec2::ZERO;

                            // query
                            egui::Frame::new().show(ui, |ui| {
                                ui.set_height(menu_query_height);

                                // TODO: prevent default behavior on ArrowUp and ArrowDown, since that controls menu cycling
                                let textedit = egui::TextEdit::singleline(&mut menu.query)
                                    .hint_text("query edit menu")
                                    .desired_width(menu_width) // 100f32 is a fine width for now
                                    .cursor_at_end(true);
                                let textedit_response = ui.add(textedit);
                                // on change, update menu
                                if textedit_response.changed() {
                                    menu.update(true);
                                }
                                // when the menu is open, the query should always have focus
                                if !textedit_response.has_focus() {
                                    textedit_response.request_focus();
                                }
                            });

                            ui.end_row();

                            // options
                            egui::Frame::new().show(ui, |ui| {
                                ui.set_height(menu_options_height);

                                let menu_index = menu.index;
                                let matched_items = menu.matched_items();
                                let matched_items_count = matched_items.len();
                                let focus_index = if matched_items_count == 0 {
                                    None
                                } else {
                                    Some(menu_index.rem_euclid(matched_items_count as i8) as usize)
                                };
                                let requested_scroll_to_option = menu.requested_scroll_to_option;

                                egui::ScrollArea::vertical().show(ui, |ui| {
                                    ui.with_layout(
                                        egui::Layout::left_to_right(egui::Align::TOP)
                                            .with_main_wrap(true),
                                        |ui| {
                                            ui.spacing_mut().item_spacing = egui::Vec2::ZERO;
                                            ui.set_row_height(
                                                ui.text_style_height(&egui::TextStyle::Body),
                                            );

                                            let matched_items = menu.matched_items();

                                            // let mut responses = vec![];
                                            let mut focus_response = None;

                                            for (i, (label, _item)) in
                                                matched_items.iter().enumerate()
                                            {
                                                let focused = Some(i) == focus_index;

                                                let frame = egui::Frame::new().show(ui, |ui| {
                                                    ui.set_width(menu_width);
                                                    ui.with_layout(
                                                        egui::Layout::left_to_right(
                                                            egui::Align::TOP,
                                                        )
                                                        .with_main_wrap(true),
                                                        |ui| {
                                                            ui.spacing_mut().item_spacing =
                                                                egui::Vec2::ZERO;
                                                            ui.set_row_height(
                                                                ui.text_style_height(
                                                                    &egui::TextStyle::Body,
                                                                ),
                                                            );

                                                            egui::Frame::new().show(ui, |ui| {
                                                                ui.set_width(10.0f32);
                                                                ui.vertical_centered(|ui| {
                                                                    if focused {
                                                                        ui.add(
                                                                            egui::Label::new(
                                                                                egui::RichText::new(
                                                                                    "•".to_owned(),
                                                                                ),
                                                                            )
                                                                            .selectable(false),
                                                                        );
                                                                    }
                                                                });
                                                            });

                                                            egui::Frame::new().show(ui, |ui| {
                                                                ui.add(
                                                                    egui::Label::new(
                                                                        egui::RichText::new(
                                                                            label.clone(),
                                                                        ),
                                                                    )
                                                                    .selectable(false),
                                                                );
                                                            })
                                                        },
                                                    );
                                                });

                                                if requested_scroll_to_option && focused {
                                                    focus_response = Some(frame.response);
                                                }

                                                ui.end_row();
                                            }

                                            if let Some(focus_response) = focus_response {
                                                focus_response.scroll_to_me(Some(egui::Align::TOP));
                                            }
                                            menu.requested_scroll_to_option = false;
                                        },
                                    );
                                });
                            })
                        });
                    });
            }
        });
    }
}

pub fn render_expr<ES: EditorSpec>(
    ctx: &egui::Context,
    ui: &mut egui::Ui,
    rc: RenderContext<'_>,
    path: &Path,
    expr: &DiagExpr,
) {
    // render diagnostics
    {
        let ds = expr.label.diagnostics.0.take();

        for (i, d) in ds.iter().enumerate() {
            let id = ui.id().with(i);

            let response = egui::Frame::new()
                .fill(rc.color_scheme.error_background)
                .show(ui, |ui| {
                    ui.add(
                        egui::Label::new(
                            egui::RichText::new(" ! ".to_owned()).color(rc.color_scheme.error_text),
                        )
                        .selectable(false),
                    )
                })
                .response;

            if response.hovered() {
                egui::Area::new(id)
                    .pivot(egui::Align2::LEFT_BOTTOM)
                    .fixed_pos(response.rect.left_top() + egui::Vec2::new(0.0, -2.0))
                    .show(ui.ctx(), |ui| {
                        egui::Frame::popup(ui.style())
                            .shadow(egui::Shadow::NONE)
                            .show(ui, |ui| {
                                ui.set_max_width(200.0);
                                ui.add(egui::Label::new(
                                    egui::RichText::new("Diagnostic".to_owned())
                                        .underline()
                                        .size(10.0),
                                ));
                                ui.label(d.0.clone())
                            })
                    });
            }
        }

        expr.label.diagnostics.0.set(ds);
    }

    let mut render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)> = vec![];

    for (s, e) in expr.kids.iter_steps_and_kids() {
        render_steps_and_kids.push((
            RenderPoint {
                path,
                i: s.left_index(),
            },
            Some(RenderExpr {
                expr: e,
                path: path.clone().append(Path(vec![s])),
            }),
        ));
    }

    render_steps_and_kids.push((
        RenderPoint {
            path,
            i: expr.kids.rightmost_index(),
        },
        None,
    ));

    let literal_kids_count = render_steps_and_kids
        .iter()
        .filter(|(_, o_re)| match o_re {
            Some(re) => matches!(re.expr.label.constructor, Constructor::Literal(_)),
            None => false,
        })
        .collect::<Vec<_>>()
        .len();

    match &expr.label.constructor {
        Constructor::Literal(literal) => {
            ES::assemble_rendered_expr(ctx, ui, rc, path, expr, render_steps_and_kids, literal);
        }
        Constructor::Root => {
            let RenderContext {
                handle,
                root,
                color_scheme,
                drag_origin,
                menu,
                actions,
                interactive,
                indent_level,
            } = rc;
            for (step, kid) in &render_steps_and_kids {
                step.render::<ES>(
                    ctx,
                    ui,
                    // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                    RenderContext {
                        handle,
                        root,
                        color_scheme,
                        drag_origin,
                        menu,
                        actions,
                        interactive,
                        indent_level,
                    },
                );
                if let Some(kid) = kid {
                    kid.render::<ES>(
                        ctx,
                        ui,
                        // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                        RenderContext {
                            handle,
                            root,
                            color_scheme,
                            drag_origin,
                            menu,
                            actions,
                            interactive,
                            indent_level,
                        },
                    );
                }
            }
        }
        Constructor::Newline => {
            ui.end_row();
            egui::Frame::new().show(ui, |ui| {
                ui.set_width(
                    (std::cmp::max(path.0.len(), 1) - 1) as f32
                        * INDENT_WIDTH_EM
                        * ui.text_style_height(&egui::TextStyle::Body),
                );
            });
        }
        Constructor::PosArg if literal_kids_count == 1 => {
            let RenderContext {
                handle,
                root,
                color_scheme,
                drag_origin,
                menu,
                actions,
                interactive,
                indent_level,
            } = rc;
            for (step, kid) in &render_steps_and_kids {
                step.render::<ES>(
                    ctx,
                    ui,
                    // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                    RenderContext {
                        handle,
                        root,
                        color_scheme,
                        drag_origin,
                        menu,
                        actions,
                        interactive,
                        indent_level,
                    },
                );
                if let Some(kid) = kid {
                    kid.render::<ES>(
                        ctx,
                        ui,
                        // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                        RenderContext {
                            handle,
                            root,
                            color_scheme,
                            drag_origin,
                            menu,
                            actions,
                            interactive,
                            indent_level,
                        },
                    );
                }
            }
        }
        Constructor::PosArg => {
            egui::Frame::new()
                .fill(rc.color_scheme.error_background)
                .show(ui, |ui| {
                    ui.add(egui::Label::new(
                        egui::RichText::new("[".to_owned()).color(rc.color_scheme.error_text),
                    ))
                });

            let RenderContext {
                handle,
                root,
                color_scheme,
                drag_origin,
                menu,
                actions,
                interactive,
                indent_level,
            } = rc;
            for (step, kid) in &render_steps_and_kids {
                step.render::<ES>(
                    ctx,
                    ui,
                    RenderContext {
                        handle,
                        root,
                        color_scheme,
                        drag_origin,
                        menu,
                        actions,
                        interactive,
                        indent_level,
                    },
                );
                if let Some(kid) = kid {
                    kid.render::<ES>(
                        ctx,
                        ui,
                        RenderContext {
                            handle,
                            root,
                            color_scheme,
                            drag_origin,
                            menu,
                            actions,
                            interactive,
                            indent_level,
                        },
                    );
                }
            }

            egui::Frame::new()
                .fill(rc.color_scheme.error_background)
                .show(ui, |ui| {
                    ui.add(egui::Label::new(
                        egui::RichText::new("]".to_owned()).color(rc.color_scheme.error_text),
                    ))
                });
        }
    }
}

#[derive(Debug, Clone)]
pub struct CoreState<D> {
    pub root: GenEditorExpr<D>,
    pub handle: Handle,
    pub clipboard: Option<PlainFragment>,
}

impl<D> CoreState<D> {
    pub fn new(expr: GenEditorExpr<D>, handle: Handle) -> Self {
        Self {
            root: expr,
            handle,
            clipboard: None,
        }
    }

    pub fn into_diag(self) -> DiagCoreState {
        CoreState {
            root: self.root.into_diag(),
            handle: self.handle,
            clipboard: self.clipboard,
        }
    }

    pub fn to_diag(&self) -> DiagCoreState {
        CoreState {
            root: self.root.to_diag(),
            handle: self.handle.clone(),
            clipboard: self.clipboard.clone(),
        }
    }

    pub fn into_plain(self) -> PlainCoreState {
        CoreState {
            root: self.root.into_plain(),
            handle: self.handle,
            clipboard: self.clipboard,
        }
    }

    pub fn to_plain(&self) -> PlainCoreState {
        CoreState {
            root: self.root.to_plain(),
            handle: self.handle.clone(),
            clipboard: self.clipboard.clone(),
        }
    }
}

pub type DiagCoreState = CoreState<MutDiagnostics>;

pub type PlainCoreState = CoreState<()>;

pub struct EditMenu {
    pub query: String,
    pub all_options: Vec<EditMenuOption>,
    pub index: i8,
    pub matcher: nucleo_matcher::Matcher,
    pub requested_scroll_to_option: bool,
}

impl EditMenu {
    pub fn new(all_options: Vec<EditMenuOption>) -> Self {
        Self {
            query: Default::default(),
            all_options,
            index: 0,
            matcher: Matcher::new(nucleo_matcher::Config::DEFAULT),
            requested_scroll_to_option: true,
        }
    }

    pub fn new_with_query(all_options: Vec<EditMenuOption>, query: String) -> Self {
        Self {
            query,
            all_options,
            index: 0,
            matcher: Matcher::new(nucleo_matcher::Config::DEFAULT),
            requested_scroll_to_option: true,
        }
    }

    pub fn matched_items(&mut self) -> Vec<(String, &EditMenuOption)> {
        let matched_dynamic_items: Vec<(String, &EditMenuOption)> = self
            .all_options
            .iter()
            .filter_map(|item| match item.pattern {
                EditMenuPattern::Static(_) => None,
                EditMenuPattern::Dynamic(_, f) => Some((f(&self.query)?, item)),
            })
            .collect::<Vec<_>>();

        let matched_static_items: Vec<(String, &EditMenuOption)> =
            nucleo_matcher::pattern::Pattern::parse(
                &self.query,
                nucleo_matcher::pattern::CaseMatching::Smart,
                nucleo_matcher::pattern::Normalization::Smart,
            )
            .match_list(
                self.all_options.iter().filter(|item| match item.pattern {
                    EditMenuPattern::Static(_) => true,
                    EditMenuPattern::Dynamic(_, _) => false,
                }),
                &mut self.matcher,
            )
            .iter()
            .map(|item| (item.0.pattern.label().to_owned(), item.0))
            .collect();

        // include dynamic before static
        [matched_dynamic_items, matched_static_items].concat()
    }

    pub fn focus_option(&mut self) -> Option<&EditMenuOption> {
        let index = self.index;
        let matched_items = self.matched_items();

        if matched_items.is_empty() {
            return None;
        }

        let index = index.rem_euclid(matched_items.len() as i8);
        let item = matched_items.get(index as usize)?;
        Some(item.1)
    }

    pub fn update(&mut self, reset_index: bool) {
        trace!(target: "editor.edit", "update; self.query = {}", self.query);
        self.requested_scroll_to_option = true;
        if reset_index {
            self.index = 0;
        }
    }
}

pub type Edit = fn(&str, DiagCoreState) -> Option<DiagCoreState>;

#[derive(Debug, Clone)]
pub struct EditMenuOption {
    pub pattern: EditMenuPattern,
    pub edit: Edit,
}

impl AsRef<str> for EditMenuOption {
    fn as_ref(&self) -> &str {
        self.pattern.label()
    }
}

impl EditMenuOption {
    pub fn new(pattern: EditMenuPattern, edit: Edit) -> Self {
        Self { pattern, edit }
    }
}

#[derive(Debug, Clone)]
pub enum EditMenuPattern {
    Static(String),
    Dynamic(String, fn(&str) -> Option<String>),
}

impl EditMenuPattern {
    pub fn label(&self) -> &str {
        match self {
            Self::Static(s) | Self::Dynamic(s, _) => s,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Hash)]
pub enum Constructor {
    Literal(String),
    Root,
    Newline,
    /// positional argument
    PosArg,
}

impl Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(s) => write!(f, "{s}"),
            Self::Newline => write!(f, "<newline>"),
            Self::Root => write!(f, "<root>"),
            Self::PosArg => write!(f, "<pos_arg>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Diagnostic(pub String);

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct RenderContext<'a> {
    pub handle: &'a Handle,
    pub root: &'a DiagExpr,
    pub color_scheme: &'a ColorScheme,
    pub drag_origin: &'a Option<Handle>,
    pub menu: &'a mut Option<EditMenu>,
    pub actions: &'a mut Vec<Action>,
    pub interactive: bool,
    pub indent_level: u8,
}

pub struct AssembleRenderExprArgs {}

pub trait EditorSpec: 'static {
    fn name() -> String;

    fn initial_state() -> PlainCoreState;

    fn get_edits(state: &EditorState<Self>) -> Vec<EditMenuOption>;

    fn diagnose(state: &CoreState<MutDiagnostics>);

    fn is_valid_handle_specialized(handle: &Handle, root: &DiagExpr) -> bool;

    fn is_valid_handle(handle: &Handle, root: &DiagExpr) -> bool {
        trace!(target: "is_valid_handle", "handle = {handle}");
        trace!(target: "is_valid_handle", "root   = {root}");

        let b_general = || match handle {
            Handle::Point(p) => {
                let e = root.get_subexpr(&p.path);
                #[expect(clippy::match_like_matches_macro)]
                match e.label.constructor {
                    Constructor::Newline => false,
                    _ => true,
                }
            }
            Handle::Span(h) => {
                let e = root.get_subexpr(&h.path);
                #[expect(clippy::match_like_matches_macro)]
                match e.label.constructor {
                    Constructor::Newline => false,
                    _ => true,
                }
            }
            Handle::Zipper(h) => {
                // These are closures so that the final conjunction is evaluated
                // in a short-circuited fashion.
                let b1 = || {
                    let e = root.get_subexpr(&h.path_o);
                    #[expect(clippy::match_like_matches_macro)]
                    match e.label.constructor {
                        Constructor::Newline => false,
                        _ => true,
                    }
                };
                let b2 = || {
                    let e = root.get_subexpr(&h.path_i());
                    #[expect(clippy::match_like_matches_macro)]
                    match e.label.constructor {
                        Constructor::Newline => false,
                        _ => true,
                    }
                };
                b1() && b2()
            }
        };

        let b_specialized = || Self::is_valid_handle_specialized(handle, root);

        b_general() && b_specialized()
    }

    fn assemble_rendered_expr(
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        rc: RenderContext<'_>,
        path: &Path,
        expr: &DiagExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        literal: &str,
    );
}

pub fn match_input_move_dir(ctx: &egui::Context) -> Option<MoveDir> {
    if ctx.input(|i| i.key_pressed(egui::Key::ArrowLeft)) {
        Some(MoveDir::Prev)
    } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowRight)) {
        Some(MoveDir::Next)
    } else {
        None
    }
}

pub fn match_input_cycle_dir(ctx: &egui::Context) -> Option<CycleDir> {
    if ctx.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
        Some(CycleDir::Prev)
    } else if ctx.input(|i| i.key_pressed(egui::Key::ArrowDown)) {
        Some(CycleDir::Next)
    } else {
        None
    }
}

// -----------------------------------------------------------------------------

pub struct RenderPoint<'a> {
    pub path: &'a Path,
    pub i: Index,
}

impl<'a> RenderPoint<'a> {
    pub fn render<ES: EditorSpec>(
        &self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        rc: RenderContext<'_>,
    ) {
        render_point::<ES>(
            ctx,
            ui,
            rc,
            &Point {
                path: self.path.clone(),
                i: self.i,
            },
        );
    }
}

pub struct RenderExpr<'a> {
    pub path: Path,
    pub expr: &'a DiagExpr,
}

impl<'a> RenderExpr<'a> {
    pub fn render<ES: EditorSpec>(
        &self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        rc: RenderContext<'_>,
    ) {
        render_expr::<ES>(ctx, ui, rc, &self.path, self.expr);
    }
}

// -----------------------------------------------------------------------------

pub enum Action {
    Copy,
    Paste,
    Cut,
    Delete,
    SetCore(DiagCoreState),
    SetHandle(SetHandle),
    SetDragOrigin(Option<Handle>),
    Undo,
    Redo,
}

pub struct SetHandle {
    pub handle: Handle,
    pub snapshot: bool,
}

pub fn insert_newline(_query: &str, mut state: DiagCoreState) -> Option<DiagCoreState> {
    let h = state.root.insert_fragment(
        state.handle,
        Fragment::Span(span![ex![
            GenEditorLabel::new(Constructor::Newline, MutDiagnostics(Cell::new(vec![]))),
            []
        ]]),
    );
    state.handle = h;
    Some(state)
}
