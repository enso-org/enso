//! This module provides a clickable view for a single breadcrumb.

use ensogl::display::shape::*;
use ensogl::prelude::*;

use crate::breadcrumbs::SharedMethodPointer;

use super::BACKGROUND_HEIGHT;
use super::TEXT_SIZE;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::ObjectOps;
use ensogl::display::shape::compound::rectangle::Rectangle;
use ensogl::Animation;
use ensogl_component::text;
use ensogl_hardcoded_theme::application::top_bar::breadcrumbs as theme;
use std::f32::consts::PI;



// =================
// === Constants ===
// =================

/// Breadcrumb left margin.
pub const LEFT_MARGIN: f32 = 0.0;
/// Breadcrumb right margin.
pub const RIGHT_MARGIN: f32 = 0.0;
const SEPARATOR_WIDTH: f32 = 6.0;
const SEPARATOR_HEIGHT: f32 = 8.0;
/// Breadcrumb padding.
pub const PADDING: f32 = 1.0;
const SEPARATOR_MARGIN: f32 = 6.0;



// =================
// === Separator ===
// =================

mod separator {
    use super::*;

    ensogl::shape! {
        pointer_events = false;
        alignment = center;
        (style: Style, red: f32, green: f32, blue: f32, alpha: f32) {
            let angle    = PI/2.0;
            let triangle = Triangle(SEPARATOR_HEIGHT.px(),SEPARATOR_WIDTH.px()).rotate(angle.radians());
            let color    = format!("vec4({red},{green},{blue},{alpha})");
            let color : Var<color::Rgba> = color.into();
            triangle.fill(color).into()
        }
    }
}



// ========================
// === RelativePosition ===
// ========================

/// The position of this breadcrumb relative to the selected breadcrumb. We use this to determine
/// the color.
#[derive(Debug, Clone, Copy)]
enum RelativePosition {
    Left,
    Right,
}



// ==================
// === Animations ===
// ==================

/// ProjectName's animations handlers.
#[derive(Debug, Clone, CloneRef)]
pub struct Animations {
    color:           Animation<color::Rgba>,
    separator_color: Animation<color::Rgba>,
}

impl Animations {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        let color = Animation::new(network);
        let separator_color = Animation::new(network);
        Self { color, separator_color }
    }
}



// =================
// === FrpInputs ===
// =================

/// Breadcrumb frp network inputs.
#[derive(Debug, Clone, CloneRef)]
pub struct FrpInputs {
    /// Select the breadcrumb, triggering the selection animation.
    pub select:   frp::Source,
    /// Select the breadcrumb, triggering the deselection animation, using the (self,new)
    /// breadcrumb indices to determine if the breadcrumb is on the left or on the right of the
    /// newly selected breadcrumb.
    pub deselect: frp::Source<(usize, usize)>,
}

impl FrpInputs {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! {network
            select   <- source();
            deselect <- source();
        }
        Self { select, deselect }
    }
}



// ==================
// === FrpOutputs ===
// ==================

/// Breadcrumb frp network outputs.
#[derive(Debug, Clone, CloneRef)]
pub struct FrpOutputs {
    /// Signalizes that the breadcrumb was clicked.
    pub clicked:     frp::Source,
    /// Signalizes that the breadcrumb's size changed.
    pub size:        frp::Source<Vector2<f32>>,
    /// Signalizes the breadcrumb's selection state.
    pub selected:    frp::Source<bool>,
    /// Used to check if the breadcrumb is selected.
    pub is_selected: frp::Sampler<bool>,
    /// Width of the breadcrumb.
    pub width:       frp::Source<f32>,
}

impl FrpOutputs {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            clicked     <- source();
            size        <- source();
            selected    <- source();
            is_selected <- selected.sampler();
            width <- source();
        }
        Self { clicked, size, selected, is_selected, width }
    }
}



// ===========
// === Frp ===
// ===========

/// A breadcrumb frp structure with its endpoints and network representation.
#[derive(Debug, Clone, CloneRef)]
#[allow(missing_docs)]
pub struct Frp {
    pub inputs:  FrpInputs,
    pub outputs: FrpOutputs,
    pub network: frp::Network,
}

impl Deref for Frp {
    type Target = FrpInputs;
    fn deref(&self) -> &Self::Target {
        &self.inputs
    }
}

impl Default for Frp {
    fn default() -> Self {
        Self::new()
    }
}

impl Frp {
    /// Constructor.
    pub fn new() -> Self {
        let network = frp::Network::new("breadcrumbs");
        let inputs = FrpInputs::new(&network);
        let outputs = FrpOutputs::new(&network);
        Self { inputs, outputs, network }
    }
}



// ======================
// === BreadcrumbInfo ===
// ======================

/// Breadcrumb information such as name and expression ID.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct BreadcrumbInfo {
    pub method_pointer: SharedMethodPointer,
    pub expression_id:  ast::Id,
}



// =======================
// === BreadcrumbModel ===
// =======================

/// Breadcrumbs model.
#[derive(Debug, Clone, CloneRef)]
pub struct BreadcrumbModel {
    display_object:    display::object::Instance,
    overlay:           Rectangle,
    separator:         separator::View,
    label:             text::Text,
    style:             StyleWatch,
    /// Breadcrumb information such as name and expression ID.
    pub info:          Rc<Option<BreadcrumbInfo>>,
    relative_position: Rc<Cell<Option<RelativePosition>>>,
    separator_visible: Rc<Cell<bool>>,
    label_width:       Rc<Cell<f32>>,
}

impl BreadcrumbModel {
    /// Constructor.
    #[profile(Detail)]
    fn new(app: &Application, info: Option<BreadcrumbInfo>) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let overlay = Rectangle::new().build(|r| {
            r.set_color(INVISIBLE_HOVER_COLOR);
        });
        let separator = separator::View::new();
        let label = app.new_view::<text::Text>();
        let relative_position = default();
        let separator_visible = default();
        let label_width = default();

        scene.layers.panel_overlay.add(&overlay);
        scene.layers.panel.add(&separator);

        label.add_to_scene_layer(&scene.layers.panel_text);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        //         system (#795)
        let style = StyleWatch::new(&scene.style_sheet);
        Self {
            display_object,
            overlay,
            separator,
            label,
            style,
            info: info.into(),
            relative_position,
            separator_visible,
            label_width,
        }
        .init()
    }

    fn init(self) -> Self {
        self.add_child(&self.overlay);
        self.add_child(&self.label);

        let styles = &self.style;
        let full_color = styles.get_color(theme::full);

        self.label.set_property_default(full_color);
        self.label.set_property_default(text::formatting::Size::from(TEXT_SIZE));
        self.label.set_single_line_mode(true);
        let label = self.info.deref().as_ref().map_or_default(|i| i.method_pointer.name.as_str());
        self.set_label(label);

        self.update_layout();

        self
    }

    fn update_layout(&self) {
        let width = self.width();
        let height = self.height();
        let offset = SEPARATOR_MARGIN + SEPARATOR_WIDTH / 2.0;

        if self.separator_visible.get() {
            self.label.set_x(offset + SEPARATOR_WIDTH / 2.0 + SEPARATOR_MARGIN);
        }

        self.label.set_y(height / 2.0 + TEXT_SIZE / 2.0);
        self.overlay.set_size(Vector2::new(width, height));
        let separator_width = (SEPARATOR_WIDTH + PADDING * 2.0).max(0.0);
        self.separator.set_size(Vector2::new(separator_width, SEPARATOR_HEIGHT));
        self.separator.set_x(offset.round());
        self.separator.set_y(height / 2.0);
    }

    /// Show a separator for this breadcrumb.
    pub fn show_separator(&self) {
        self.add_child(&self.separator);
        self.separator_visible.set(true);
        self.update_layout();
    }

    /// Set label content.
    pub fn set_label(&self, label: &str) {
        self.label.set_content(label);
        self.update_layout();
    }

    /// Update layout and return current width of the whole breadcrumb.
    fn update_label_width(&self, new_width: f32) -> f32 {
        self.label_width.set(new_width);
        self.update_layout();
        self.width()
    }

    /// Get the width of the view.
    pub fn width(&self) -> f32 {
        let label_width = self.label_width.get();
        let margin_and_padding = LEFT_MARGIN + RIGHT_MARGIN + PADDING * 2.0;
        let separator_width = if self.separator_visible.get() {
            SEPARATOR_MARGIN * 2.0 + SEPARATOR_WIDTH
        } else {
            0.0
        };
        let width = separator_width + label_width + margin_and_padding;
        width.ceil()
    }

    /// Get the height of the view.
    pub fn height(&self) -> f32 {
        BACKGROUND_HEIGHT
    }

    fn set_color(&self, color: color::Rgba) {
        self.label.set_property(.., color);
    }

    fn set_separator_color(&self, color: color::Rgba) {
        self.separator.red.set(color.red);
        self.separator.green.set(color.green);
        self.separator.blue.set(color.blue);
        self.separator.alpha.set(color.alpha);
    }

    fn deselect(&self, old: usize, new: usize) {
        let left = RelativePosition::Left;
        let right = RelativePosition::Right;
        self.relative_position
            .set((new > old).as_option().map(|_| Some(left)).unwrap_or(Some(right)));
    }
}

impl display::Object for BreadcrumbModel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ==================
// === Breadcrumb ===
// ==================

/// The breadcrumb's view which displays its name and exposes mouse press interactions.
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct Breadcrumb {
    #[deref]
    model:   Rc<BreadcrumbModel>,
    pub frp: Frp,
}

impl Breadcrumb {
    /// Constructor.
    pub fn new(
        app: &Application,
        method_pointer: &SharedMethodPointer,
        expression_id: ast::Id,
    ) -> Self {
        let info = BreadcrumbInfo { method_pointer: method_pointer.clone(), expression_id };
        Self::new_inner(app, Some(info))
    }

    /// Constructor. Creates an empty breadcrumb without [`BreadcrumbInfo`].
    pub fn new_empty(app: &Application) -> Self {
        Self::new_inner(app, None)
    }

    fn new_inner(app: &Application, info: Option<BreadcrumbInfo>) -> Self {
        let frp = Frp::new();
        let model = Rc::new(BreadcrumbModel::new(app, info));
        let network = &frp.network;
        let out = &frp.outputs;
        let scene = &app.display.default_scene;
        let animations = Animations::new(&frp.network);
        let styles = StyleWatchFrp::new(&scene.style_sheet);

        let hover_color = styles.get_color(theme::hover);
        let selected_color = styles.get_color(theme::selected);
        let left_deselected = styles.get_color(theme::deselected::left);
        let right_deselected = styles.get_color(theme::deselected::right);

        frp::extend! { network
            init <- source_();
            deselected_color <- all_with3(&selected_color, &left_deselected, &right_deselected,
                f!([model](selected, left, right) {
                    match model.relative_position.get() {
                        Some(RelativePosition::Right) => *right,
                        Some(RelativePosition::Left) => *left,
                        None => *selected,
                    }
                }
            ));
            eval_ frp.select(out.selected.emit(true));
            eval_ frp.deselect(out.selected.emit(false));
            animations.color.target <+ selected_color.sample(&frp.select);
            animations.separator_color.target <+ left_deselected.sample(&frp.select);
            eval frp.deselect(((old, new)) model.deselect(*old, *new));
            animations.color.target <+ deselected_color.sample(&frp.deselect);
            animations.separator_color.target <+ deselected_color.sample(&frp.deselect);

            not_selected <- out.selected.map(|selected| !selected);
            mouse_over_if_not_selected <- model.overlay.events_deprecated.mouse_over.gate(&not_selected);
            mouse_out_if_not_selected  <- model.overlay.events_deprecated.mouse_out.gate(&not_selected);
            animations.color.target <+ hover_color.sample(&mouse_over_if_not_selected);
            animations.color.target <+ deselected_color.sample(&mouse_out_if_not_selected);

            eval_ model.overlay.events_deprecated.mouse_down_primary(out.clicked.emit(()));
            updated_width <- model.label.width.all_with(&init, f!((w, _) model.update_label_width(*w)));
            eval updated_width((w) out.width.emit(*w));
        }

        // === Animations ===

        frp::extend! { network
            eval animations.color.value((value) model.set_color(*value));
            eval animations.separator_color.value((value) model.set_separator_color(*value));
        }
        init.emit(());

        Self { model, frp }
    }
}

impl display::Object for Breadcrumb {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
