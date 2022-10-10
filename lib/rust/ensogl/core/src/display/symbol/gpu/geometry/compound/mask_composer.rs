//! Definition of [`MaskComposer`] used to compose color and id layers with a given mask.

use crate::prelude::*;

use crate::display::symbol::gpu::geometry::compound::screen::Screen;
use crate::display::symbol::material::Material;
use crate::system::gpu::data::texture;



/// A geometry which always covers the whole screen which when rendered outputs an image by
/// composing three input textures: mask, color, and id.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct MaskComposer {
    screen: Screen,
}

impl MaskComposer {
    /// Constructor.
    pub fn new(mask: impl AsRef<str>, color: impl AsRef<str>, id: impl AsRef<str>) -> Self {
        let screen = Screen::new(Self::surface_material(mask, color, id));
        Self { screen }
    }

    fn surface_material(
        mask: impl AsRef<str>,
        color: impl AsRef<str>,
        id: impl AsRef<str>,
    ) -> Material {
        let mask = mask.as_ref();
        let color = color.as_ref();
        let id = id.as_ref();
        let mut material = Material::new();
        let shader = iformat!(
            "
            vec4 sample_mask  = texture(input_{mask},input_uv);
            vec4 sample_color = texture(input_{color},input_uv);
            vec4 sample_id    = texture(input_{id},input_uv);
            output_id         = (sample_mask.a > 0.5) ? sample_id : vec4(0.0,0.0,0.0,0.0);
            output_color      = sample_color;
            // We multiply all components by alpha because we store them in the premultiplied form.
            output_color *= sample_mask.a;
            "
        );
        material.add_input_def::<texture::FloatSampler>(mask);
        material.add_input_def::<texture::FloatSampler>(color);
        material.add_input_def::<texture::FloatSampler>(id);
        material.add_output("id", Vector4::<f32>::new(0.0, 0.0, 0.0, 0.0));
        material.set_main(shader);
        material
    }
}
