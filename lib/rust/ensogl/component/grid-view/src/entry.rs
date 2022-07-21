//! A module with an [`Entry`] abstraction for [`crate::GridView`]. `GridView` can be parametrized
//! by any entry with the specified API.

use crate::prelude::*;

use crate::Col;
use crate::Row;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::geometry::compound::sprite;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::Attribute;


#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Contour {
    pub size:          Vector2,
    pub corner_radius: f32,
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
        set_selected(bool),
        set_hovered(bool),
    }
    Output {
        disabled(bool),
        contour(Contour),
        selection_highlight_color(color::Rgba),
        hover_highlight_color(color::Rgba)
    }
}

/// FRP Api of a specific Entry.
pub type EntryFrp<E> = Frp<<E as Entry>::Model, <E as Entry>::Params>;



// =============
// === Trait ===
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

pub trait ShapeWithEntryContour {
    const PADDING_PX: f32 = 5.0;

    fn size(&self) -> &DynamicParam<sprite::Size>;

    fn corner_radius(&self) -> &DynamicParam<Attribute<f32>>;

    fn set_contour(&self, contour: Contour) {
        let padding = Vector2(Self::PADDING_PX, Self::PADDING_PX) * 2.0;
        self.size().set(contour.size + padding);
        self.corner_radius().set(contour.corner_radius);
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

pub mod overlay {
    use super::*;

    /// A padding added to the background rectangle to avoid antialiasing glitches.
    pub const PADDING_PX: f32 = 5.0;

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


pub mod shape {
    use super::*;

    /// A padding added to the background rectangle to avoid antialiasing glitches.
    pub const PADDING_PX: f32 = 5.0;

    ensogl_core::define_shape_system! {
        below = [overlay];
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
