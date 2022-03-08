//! Render pass definition trait and render pass instance definition.

use crate::prelude::*;
use crate::system::gpu::*;

use crate::system::gpu::data::texture::class::TextureOps;



// ==================
// === Definition ===
// ==================

/// Render pass definition. When used, the [`Composer`] will clone it and then will call the
/// [`init`] method before it's first usage. It will happen everytime the [`Composer`] will be
/// re-initialized (e.g. after changing scene size). Then, the function [`run`] will be called for
/// every registered pass.
#[allow(missing_docs)]
pub trait Definition: CloneBoxedForDefinition + Debug + 'static {
    fn initialize(&mut self, _instance: &Instance) {}
    fn run(&mut self, _instance: &Instance);
}

clone_boxed!(Definition);



// ================
// === Instance ===
// ================

/// Instance of a render pass. Every render pass will be initialized by the [`Composer`] before its
/// first run (see the [`Definition::run`] method. During such initialization a new [`Instance`]
/// will be created. Please note that a new instance will be generated everytime a pass will be
/// instantiated (e.g. after changing the scene size).
///
/// The main purpose of this structure is to provide passes with common information and utilities.
/// For example, it streamlines the creation of new framebuffers and textures.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Instance {
    pub variables: UniformScope,
    pub context:   Context,
    pub width:     i32,
    pub height:    i32,
}

impl Instance {
    /// Constructor
    #[allow(clippy::borrowed_box)]
    pub fn new(context: &Context, variables: &UniformScope, width: i32, height: i32) -> Self {
        let variables = variables.clone_ref();
        let context = context.clone();
        Self { variables, context, width, height }
    }

    /// Create a new texture covering the whole screen and register it in the global uniform scope
    /// with the name provided as the configuration argument.
    pub fn new_screen_texture(&self, output: &OutputDefinition) -> AnyTextureUniform {
        let context = &self.context;
        let variables = &self.variables;
        let name = format!("pass_{}", output.name);
        let args = (self.width, self.height);
        let format = output.internal_format;
        let item_type = output.item_type;
        let params = Some(output.texture_parameters);
        uniform::get_or_add_gpu_texture_dyn(
            context, variables, &name, format, item_type, args, params,
        )
    }

    /// Create a new framebuffer from the provided textures.
    pub fn new_framebuffer(&self, textures: &[&AnyTextureUniform]) -> Framebuffer {
        let context = self.context.clone();
        let native = self.context.create_framebuffer().unwrap();
        let target = Context::FRAMEBUFFER;
        let draw_buffers = js_sys::Array::new();
        context.bind_framebuffer(target, Some(&native));
        for (index, texture) in textures.iter().enumerate() {
            let texture_target = Context::TEXTURE_2D;
            let attachment_point = Context::COLOR_ATTACHMENT0 + index as u32;
            let gl_texture = texture.gl_texture();
            let gl_texture = Some(&gl_texture);
            let level = 0;
            draw_buffers.push(&attachment_point.into());
            context.framebuffer_texture_2d(
                target,
                attachment_point,
                texture_target,
                gl_texture,
                level,
            );
        }
        context.draw_buffers(&draw_buffers);
        context.bind_framebuffer(target, None);
        Framebuffer { context, native }
    }
}



// ========================
// === OutputDefinition ===
// ========================

/// Definition of pass output, a fullscreen texture of a specific name and format. It is used as
/// a configuration structure when creating new pass textures.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct OutputDefinition {
    pub name:               String,
    pub internal_format:    texture::AnyInternalFormat,
    pub item_type:          texture::AnyItemType,
    pub texture_parameters: texture::Parameters,
}

impl OutputDefinition {
    /// Constructor.
    pub fn new<Name: Str, F: Into<texture::AnyInternalFormat>, T: Into<texture::AnyItemType>>(
        name: Name,
        internal_format: F,
        item_type: T,
        texture_parameters: texture::Parameters,
    ) -> Self {
        let name = name.into();
        let internal_format = internal_format.into();
        let item_type = item_type.into();
        Self { name, internal_format, item_type, texture_parameters }
    }

    /// Constructor of the RGBA u8 output with default texture parameters. It is the most popular
    /// option and you should use it to render colors with your passes.
    pub fn new_rgba<Name: Str>(name: Name) -> Self {
        let internal_format = texture::Rgba;
        let item_type = texture::item_type::u8;
        let texture_parameters = default();
        OutputDefinition::new(name, internal_format, item_type, texture_parameters)
    }
}



// ===================
// === Framebuffer ===
// ===================

/// A native WebGL framebuffer object bound to the gl context.
#[derive(Debug, Clone)]
pub struct Framebuffer {
    context: Context,
    native:  web_sys::WebGlFramebuffer,
}

impl Deref for Framebuffer {
    type Target = web_sys::WebGlFramebuffer;
    fn deref(&self) -> &Self::Target {
        &self.native
    }
}

impl Framebuffer {
    /// Bind the framebuffer to the current WebGL context.
    pub fn bind(&self) {
        self.context.bind_framebuffer(Context::FRAMEBUFFER, Some(&self.native));
    }
}
