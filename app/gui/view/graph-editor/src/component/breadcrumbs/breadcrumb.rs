//! This module provides a clickable view for a single breadcrumb.

use crate::prelude::*;
use ensogl::display::shape::*;

use crate::component::breadcrumbs;
use crate::component::breadcrumbs::project_name::LINE_HEIGHT;
use crate::MethodPointer;

use super::RelativePosition;
use super::GLYPH_WIDTH;
use super::HORIZONTAL_MARGIN;
use super::TEXT_SIZE;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::ObjectOps;
use ensogl::DEPRECATED_Animation;
use ensogl_component::text;
use ensogl_hardcoded_theme as theme;
use nalgebra::Vector2;
use std::f32::consts::PI;



// =================
// === Constants ===
// =================

/// Breadcrumb vertical margin.
pub const VERTICAL_MARGIN: f32 = 0.0;
/// Breadcrumb left margin.
pub const LEFT_MARGIN: f32 = 0.0;
/// Breadcrumb right margin.
pub const RIGHT_MARGIN: f32 = 0.0;
const ICON_LEFT_MARGIN: f32 = 0.0;
const ICON_RIGHT_MARGIN: f32 = HORIZONTAL_MARGIN;
const ICON_RADIUS: f32 = 6.0;
const ICON_SIZE: f32 = ICON_RADIUS * 2.0;
const ICON_RING_WIDTH: f32 = 1.5;
const ICON_ARROW_SIZE: f32 = 4.0;
const SEPARATOR_SIZE: f32 = 6.0;
/// Breadcrumb padding.
pub const PADDING: f32 = 1.0;
const SEPARATOR_MARGIN: f32 = 10.0;



// ==================
// === Background ===
// ==================

/// A transparent "background" of single breadcrumb, set for capturing mouse events.
pub mod background {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style) {
            let bg_color = color::Rgba::new(0.0,0.0,0.0,0.000_001);
            Plane().fill(bg_color).into()
        }
    }
}



// ============
// === Icon ===
// ============

mod icon {
    use super::*;

    ensogl::shape! {
        pointer_events = false;
        alignment = center;
        (style: Style, red: f32, green: f32, blue: f32, alpha: f32) {
            let outer_circle  = Circle((ICON_RADIUS).px());
            let inner_circle  = Circle((ICON_RADIUS - ICON_RING_WIDTH).px());
            let ring          = outer_circle - inner_circle;
            let size          = ICON_ARROW_SIZE;
            let arrow         = Triangle(size.px(),size.px()).rotate((PI/2.0).radians());
            let arrow         = arrow.translate_x(0.5.px());
            let shape         = ring + arrow;
            let color         = format!("vec4({red},{green},{blue},{alpha})");
            let color : Var<color::Rgba> = color.into();
            shape.fill(color).into()
        }
    }
}



// =================
// === Separator ===
// =================

mod separator {
    use super::*;

    ensogl::shape! {
        pointer_events = false;
        alignment = center;
        (style: Style, red: f32, green: f32, blue: f32, alpha: f32) {
            let size     = SEPARATOR_SIZE;
            let angle    = PI/2.0;
            let triangle = Triangle(size.px(),size.px()).rotate(angle.radians());
            let color    = format!("vec4({red},{green},{blue},{alpha})");
            let color : Var<color::Rgba> = color.into();
            triangle.fill(color).into()
        }
    }
}



// ==================
// === Animations ===
// ==================

/// ProjectName's animations handlers.
#[derive(Debug, Clone, CloneRef)]
pub struct Animations {
    color:           DEPRECATED_Animation<Vector4<f32>>,
    separator_color: DEPRECATED_Animation<Vector4<f32>>,
    fade_in:         DEPRECATED_Animation<f32>,
}

impl Animations {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        let color = DEPRECATED_Animation::new(network);
        let fade_in = DEPRECATED_Animation::new(network);
        let separator_color = DEPRECATED_Animation::new(network);
        Self { color, separator_color, fade_in }
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
    /// Triggers the fade in animation, which only makes sense during the breadcrumb creation.
    pub fade_in:  frp::Source,
}

impl FrpInputs {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! {network
            select   <- source();
            deselect <- source();
            fade_in  <- source();
        }
        Self { select, deselect, fade_in }
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
}

impl FrpOutputs {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            clicked     <- source();
            size        <- source();
            selected    <- source();
            is_selected <- selected.sampler();
        }
        Self { clicked, size, selected, is_selected }
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
#[derive(Debug)]
#[allow(missing_docs)]
pub struct BreadcrumbInfo {
    pub method_pointer: MethodPointer,
    pub expression_id:  ast::Id,
}



// =======================
// === BreadcrumbModel ===
// =======================

/// Breadcrumbs model.
#[derive(Debug, Clone, CloneRef)]
pub struct BreadcrumbModel {
    display_object:    display::object::Instance,
    view:              background::View,
    separator:         separator::View,
    icon:              icon::View,
    label:             text::Text,
    animations:        Animations,
    style:             StyleWatch,
    /// Breadcrumb information such as name and expression ID.
    pub info:          Rc<BreadcrumbInfo>,
    relative_position: Rc<Cell<Option<RelativePosition>>>,
    outputs:           FrpOutputs,
}

impl BreadcrumbModel {
    /// Constructor.
    #[profile(Detail)]
    pub fn new(
        app: &Application,
        frp: &Frp,
        method_pointer: &MethodPointer,
        expression_id: &ast::Id,
    ) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let view = background::View::new();
        let icon = icon::View::new();
        let separator = separator::View::new();
        let label = app.new_view::<text::Text>();
        let expression_id = *expression_id;
        let method_pointer = method_pointer.clone();
        let info = Rc::new(BreadcrumbInfo { method_pointer, expression_id });
        let animations = Animations::new(&frp.network);
        let relative_position = default();
        let outputs = frp.outputs.clone_ref();

        ensogl::shapes_order_dependencies! {
            scene => {
                background -> icon;
                background -> separator;
            }
        }

        scene.layers.panel.add(&view);
        scene.layers.panel.add(&icon);
        scene.layers.panel.add(&separator);

        scene.layers.main.remove(&label);
        label.add_to_scene_layer(&scene.layers.panel_text);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        //         system (#795)
        let style = StyleWatch::new(&scene.style_sheet);
        Self {
            display_object,
            view,
            separator,
            icon,
            label,
            animations,
            style,
            info,
            relative_position,
            outputs,
        }
        .init()
    }

    fn init(self) -> Self {
        self.add_child(&self.view);
        self.view.add_child(&self.separator);
        self.separator.add_child(&self.icon);
        self.icon.add_child(&self.label);

        let styles = &self.style;
        let full_color = styles.get_color(theme::graph_editor::breadcrumbs::full);
        let transparent_color = styles.get_color(theme::graph_editor::breadcrumbs::transparent);

        let color = if self.is_selected() { full_color } else { transparent_color };

        self.label.set_property_default(color);
        self.label.set_property_default(text::formatting::Size::from(TEXT_SIZE));
        self.label.set_single_line_mode(true);
        self.label.set_x(ICON_RADIUS + ICON_RIGHT_MARGIN);
        self.label.set_y(TEXT_SIZE / 2.0);
        self.label.set_content(&self.info.method_pointer.name);

        let width = self.width();
        let height = self.height();
        let offset = SEPARATOR_MARGIN + SEPARATOR_SIZE / 2.0;

        self.view.set_size(Vector2::new(width, height));
        self.fade_in(0.0);
        let separator_size = (SEPARATOR_SIZE + PADDING * 2.0).max(0.0);
        let icon_size = (ICON_SIZE + PADDING * 2.0).max(0.0);
        self.separator.set_size(Vector2::new(separator_size, separator_size));
        self.separator.set_x((offset - width / 2.0).round());
        self.icon.set_size(Vector2::new(icon_size, icon_size));
        let x_position = offset + PADDING + ICON_SIZE / 2.0 + LEFT_MARGIN + ICON_LEFT_MARGIN;
        self.icon.set_x(x_position.round());

        self
    }

    fn label_width(&self) -> f32 {
        self.info.method_pointer.name.len() as f32 * GLYPH_WIDTH
    }

    /// Get the width of the view.
    pub fn width(&self) -> f32 {
        let separator_width = SEPARATOR_MARGIN * 2.0 + SEPARATOR_SIZE;
        let icon_width = ICON_LEFT_MARGIN + ICON_SIZE + ICON_RIGHT_MARGIN;
        let label_width = self.label_width();
        let margin_and_padding = LEFT_MARGIN + RIGHT_MARGIN + PADDING * 2.0;
        let width = separator_width + icon_width + label_width + margin_and_padding;
        width.ceil()
    }

    /// Get the height of the view.
    pub fn height(&self) -> f32 {
        LINE_HEIGHT + breadcrumbs::VERTICAL_MARGIN * 2.0
    }

    fn fade_in(&self, value: f32) {
        let width = self.width();
        let height = self.height();
        let x_position = width * value / 2.0;
        let y_position = -height / 2.0 - VERTICAL_MARGIN - PADDING;
        self.view.set_position(Vector3(x_position.round(), y_position.round(), 0.0));
    }

    fn set_color(&self, value: Vector4<f32>) {
        let color = color::Rgba::from(value);
        self.label.set_property(.., color);
        self.icon.red.set(color.red);
        self.icon.green.set(color.green);
        self.icon.blue.set(color.blue);
        self.icon.alpha.set(color.alpha);
    }

    fn set_separator_color(&self, value: Vector4<f32>) {
        let color = color::Rgba::from(value);
        self.separator.red.set(color.red);
        self.separator.green.set(color.green);
        self.separator.blue.set(color.blue);
        self.separator.alpha.set(color.alpha);
    }

    fn select(&self) {
        let styles = &self.style;
        let selected_color = styles.get_color(theme::graph_editor::breadcrumbs::selected);
        let left_deselected = styles.get_color(theme::graph_editor::breadcrumbs::deselected::left);

        self.animations.color.set_target_value(selected_color.into());
        self.animations.separator_color.set_target_value(left_deselected.into());
    }

    fn deselect(&self, old: usize, new: usize) {
        let left = RelativePosition::Left;
        let right = RelativePosition::Right;
        self.relative_position
            .set((new > old).as_option().map(|_| Some(left)).unwrap_or(Some(right)));
        let color = self.deselected_color().into();
        self.animations.color.set_target_value(color);
        self.animations.separator_color.set_target_value(color);
    }

    fn deselected_color(&self) -> color::Rgba {
        let styles = &self.style;
        let selected_color = styles.get_color(theme::graph_editor::breadcrumbs::selected);
        let left_deselected = styles.get_color(theme::graph_editor::breadcrumbs::deselected::left);
        let right_deselected =
            styles.get_color(theme::graph_editor::breadcrumbs::deselected::right);

        match self.relative_position.get() {
            Some(RelativePosition::Right) => right_deselected,
            Some(RelativePosition::Left) => left_deselected,
            None => selected_color,
        }
    }

    fn is_selected(&self) -> bool {
        self.outputs.is_selected.value()
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
    pub fn new(app: &Application, method_pointer: &MethodPointer, expression_id: &ast::Id) -> Self {
        let frp = Frp::new();
        let model = Rc::new(BreadcrumbModel::new(app, &frp, method_pointer, expression_id));
        let network = &frp.network;
        let scene = &app.display.default_scene;

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        //         system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let hover_color = styles.get_color(theme::graph_editor::breadcrumbs::hover);

        frp::extend! { network
            eval_ frp.fade_in(model.animations.fade_in.set_target_value(1.0));
            eval_ frp.select({
                model.outputs.selected.emit(true);
                model.select();
            });
            eval frp.deselect(((old,new)) {
                model.outputs.selected.emit(false);
                model.deselect(*old,*new);
            });
            not_selected <- frp.outputs.selected.map(|selected| !selected);
            mouse_over_if_not_selected <- model.view.events_deprecated.mouse_over.gate(&not_selected);
            mouse_out_if_not_selected  <- model.view.events_deprecated.mouse_out.gate(&not_selected);
            eval_ mouse_over_if_not_selected(
                model.animations.color.set_target_value(hover_color.into())
            );
            eval_ mouse_out_if_not_selected(
                model.animations.color.set_target_value(model.deselected_color().into())
            );
            eval_ model.view.events_deprecated.mouse_down_primary(frp.outputs.clicked.emit(()));
        }


        // === Animations ===

        frp::extend! {network
            eval model.animations.fade_in.value((value) model.fade_in(*value));
            eval model.animations.color.value((value) model.set_color(*value));
            eval model.animations.separator_color.value((value) model.set_separator_color(*value));
        }

        Self { model, frp }
    }
}

impl display::Object for Breadcrumb {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
