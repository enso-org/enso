//! Definition of [`MaskComposer`] used to compose color and id layers with a given mask.

use crate::prelude::*;

use crate::display::symbol::gpu::geometry::compound::screen::Screen;
use crate::display::symbol::material::Material;
use crate::system::gpu::data::texture;



/// A geometry which always covers the whole screen which when rendered outputs an image by
/// composing three input textures: mask, color, and id.
#[derive(Clone, CloneRef, Debug, Deref)]
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
        let shader = format!(
            "
            vec4 sample_mask  = texelFetch(input_{mask},ivec2(gl_FragCoord.xy), 0);
            // When totally outside of mask, avoid the cost of sampling two additional textures.
            if (sample_mask.a < 0.01) discard;
            vec4 sample_color = texelFetch(input_{color},ivec2(gl_FragCoord.xy), 0);
            vec4 sample_id    = texelFetch(input_{id},ivec2(gl_FragCoord.xy), 0);
            output_id         = (sample_mask.a > 0.5) ? sample_id : vec4(0.0,0.0,0.0,0.0);
            // We multiply all components by alpha because we store them in the premultiplied form.
            output_color      = sample_color * sample_mask.a;
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


/// A geometry which always covers the whole screen which renders its two input textures without
/// modification: color and id. Useful in combination with different blend modes.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct OverlayComposer {
    screen: Screen,
}

impl OverlayComposer {
    /// Constructor.
    pub fn new(color: impl AsRef<str>, id: impl AsRef<str>) -> Self {
        let screen = Screen::new(Self::surface_material(color, id));
        Self { screen }
    }

    fn surface_material(color: impl AsRef<str>, id: impl AsRef<str>) -> Material {
        let color = color.as_ref();
        let id = id.as_ref();
        let mut material = Material::new();
        let shader = format!(
            "
            output_color = texelFetch(input_{color},ivec2(gl_FragCoord.xy), 0);
            output_id = texelFetch(input_{id},ivec2(gl_FragCoord.xy), 0);
            "
        );
        material.add_input_def::<texture::FloatSampler>(color);
        material.add_input_def::<texture::FloatSampler>(id);
        material.add_output("id", Vector4::<f32>::new(0.0, 0.0, 0.0, 0.0));
        material.set_main(shader);
        material
    }
}
