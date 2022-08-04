//! Structures related to a single [`crate::GridView`] entry.

use crate::prelude::*;

use crate::selectable::highlight;
use crate::Col;
use crate::Row;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::geometry::compound::sprite;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::Attribute;



// ===============
// === Contour ===
// ===============

/// A structure describing entry contour.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Contour {
    pub size:           Vector2,
    pub corners_radius: f32,
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! { <Model: (frp::node::Data), Params: (frp::node::Data)>
    Input {
        set_model(Model),
        set_size(Vector2),
        set_params(Params),
        set_location((Row, Col)),
        /// True if the entry is currently selected.
        ///
        /// This flag is set only in [selectable](crate::selectable) grid views.
        set_selected(bool),
        /// True is the entry is currently hovered by mouse.
        ///
        /// This flag is set only in [selectable](crate::selectable) grid views.
        set_hovered(bool),
    }
    Output {
        /// Disabled entries does not react for mouse events, and cannot be selected.
        disabled(bool),
        /// Entry's contour. Defines what part of the entry will react for mouse events, and also
        /// defines the shape of the selection/hover highlight (in case of
        /// [selectable](crate::selectable) grid views.).
        contour(Contour),
        selection_highlight_color(color::Rgba),
        hover_highlight_color(color::Rgba)
    }
}

/// FRP Api of a specific Entry.
pub type EntryFrp<E> = Frp<<E as Entry>::Model, <E as Entry>::Params>;



// =============
// === Entry ===
// =============

/// The abstraction of Entry for [`crate::GridView`].
///
/// The entry may be any [`display::Object`] which can provide the [`EntryFRP`] API.
pub trait Entry: CloneRef + Debug + display::Object + 'static {
    /// The model of this entry. The entry should be a representation of data from the Model.
    /// For example, the entry being just a caption can have [`String`] as its model - the text to
    /// be displayed.
    type Model: Clone + Debug + Default;

    /// A type parametrizing the various aspects of the entry, independed of the Model (for example
    /// the text color). The parameters are set in [`crate::GridView`] and shared between all
    /// entries.
    type Params: Clone + Debug + Default;

    /// An Entry constructor.
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self;

    /// FRP endpoints getter.
    fn frp(&self) -> &EntryFrp<Self>;
}



// ==============
// === Shapes ===
// ==============

// === ShapeWithEntryContour ===

/// The trait implemented by all shapes sharing the contour of an entry.
pub trait ShapeWithEntryContour {
    /// Padding added to the shape to avoid antialiasing issues.
    const PADDING_PX: f32 = 5.0;

    /// Get the size parameter.
    fn size(&self) -> &DynamicParam<sprite::Size>;

    /// Get the corner radius parameter.
    fn corner_radius(&self) -> &DynamicParam<Attribute<f32>>;

    /// Update shape's contour.
    fn set_contour(&self, contour: Contour) {
        let padding = Vector2(Self::PADDING_PX, Self::PADDING_PX) * 2.0;
        self.size().set(contour.size + padding);
        self.corner_radius().set(contour.corners_radius);
    }
}

macro_rules! implement_shape_with_entry_contour {
    () => {
        impl ShapeWithEntryContour for View {
            fn size(&self) -> &DynamicParam<sprite::Size> {
                &self.size
            }

            fn corner_radius(&self) -> &DynamicParam<Attribute<f32>> {
                &self.corner_radius
            }
        }
    };
}


// === overlay ===

/// The entry overlay used for catching mouse events over an entry.
pub mod overlay {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style, corner_radius: f32) {
            let shape_width  : Var<Pixels> = "input_size.x".into();
            let shape_height : Var<Pixels> = "input_size.y".into();
            let width = shape_width - 2.0.px() * View::PADDING_PX;
            let height = shape_height - 2.0.px() * View::PADDING_PX;
            Rect((width, height)).corners_radius(corner_radius.px()).fill(HOVER_COLOR).into()
        }
    }

    implement_shape_with_entry_contour!();
}


// === shape ===

/// The shape having an entry contour filled with color. It's a helper which may be used in
/// entries implementations - for example [crate::simple::Entry`].
pub mod shape {
    use super::*;

    ensogl_core::define_shape_system! {
        below = [overlay, highlight::shape];
        (style:Style, corner_radius: f32, color: Vector4) {
            let shape_width  : Var<Pixels> = "input_size.x".into();
            let shape_height : Var<Pixels> = "input_size.y".into();
            let width = shape_width - 2.0.px() * View::PADDING_PX;
            let height = shape_height - 2.0.px() * View::PADDING_PX;
            Rect((width, height)).corners_radius(corner_radius.px()).fill(color).into()
        }
    }

    implement_shape_with_entry_contour!();
}
