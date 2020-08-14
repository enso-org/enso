//! This module defines render composer, a render pipeline bound to a specific context.

use crate::prelude::*;

use crate::display::render::pipeline::*;
use crate::system::gpu::*;
use js_sys::Array;



// ======================
// === RenderComposer ===
// ======================

shared! { RenderComposer
/// Render composer is a render pipeline bound to a specific context.
#[derive(Debug)]
pub struct RenderComposerData {
    passes    : Vec<ComposerPass>,
    variables : UniformScope,
    context   : Context,
    width     : i32,
    height    : i32,
}

impl {
    /// Constructor
    pub fn new
    ( pipeline  : &RenderPipeline
    , context   : &Context
    , variables : &UniformScope
    , width     : i32
    , height    : i32
    ) -> Self {
        let passes    = default();
        let context   = context.clone();
        let variables = variables.clone_ref();
        let mut this  = Self {passes,variables,context,width,height};
        for pass in pipeline.passes_clone() { this.add(pass); };
        this
    }

    fn add(&mut self, pass:Box<dyn RenderPass>) {
        let pass = ComposerPass::new(&self.context,&self.variables,pass,self.width,self.height);
        self.passes.push(pass);
    }

    /// Run all the registered passes in this composer.
    pub fn run(&mut self) {
        for pass in &mut self.passes {
            pass.run(&self.context);
        }
    }
}}



// ====================
// === ComposerPass ===
// ====================

/// A `RenderPass` bound to a specific rendering context.
#[derive(Derivative)]
#[derivative(Debug)]
struct ComposerPass {
    #[derivative(Debug="ignore")]
    pass        : Box<dyn RenderPass>,
    outputs     : Vec<AnyTextureUniform>,
    framebuffer : Option<web_sys::WebGlFramebuffer>,
    variables   : UniformScope,
    context     : Context,
    width       : i32,
    height      : i32,
}

impl ComposerPass {
    /// Constructor
    #[allow(clippy::borrowed_box)]
    pub fn new
    ( context   : &Context
    , variables : &UniformScope
    , pass      : Box<dyn RenderPass>
    , width     : i32
    , height    : i32
    ) -> Self {
        let outputs      = default();
        let variables    = variables.clone_ref();
        let context      = context.clone();
        let is_not_empty = !pass.outputs().is_empty();
        let framebuffer  = is_not_empty.as_some_from(|| context.create_framebuffer().unwrap());
        let mut this     = Self {pass,outputs,framebuffer,variables,context,width,height};
        this.initialize();
        this
    }

    /// Run the pass.
    pub fn run(&mut self, context:&Context) {
        self.context.bind_framebuffer(Context::FRAMEBUFFER,self.framebuffer.as_ref());
        self.pass.run(context,&self.variables);
    }

    fn initialize(&mut self) {
        self.initialize_outputs();
        self.initialize_draw_buffers();
    }

    fn initialize_outputs(&mut self) {
        for output in &self.pass.outputs() {
            let name    = format!("pass_{}",output.name());
            let args    = (self.width,self.height);
            let texture = uniform::get_or_add_gpu_texture_dyn
                (&self.context,&self.variables,&name,output.internal_format,output.item_type,args,
                 Some(output.texture_parameters));
            self.add_output(texture);
        }
    }

    /// WebGL has to be informed to what attachments it is allowed to draw. This function enables
    /// every attachment bound to an output.
    fn initialize_draw_buffers(&mut self) {
        if !self.outputs.is_empty() {
            let draw_buffers = Array::new();
            self.outputs.iter().enumerate().for_each(|(i, _)| {
                let attachment_point = Context::COLOR_ATTACHMENT0 + i as u32;
                draw_buffers.push(&attachment_point.into());
            });
            self.context.draw_buffers(&draw_buffers);
        }
    }

    fn add_output(&mut self, texture:AnyTextureUniform) {
        let context          = &self.context;
        let target           = Context::FRAMEBUFFER;
        let texture_target   = Context::TEXTURE_2D;
        let index            = self.outputs.len() as u32;
        let attachment_point = Context::COLOR_ATTACHMENT0 + index;
        let gl_texture       = texture.gl_texture();
        let gl_texture       = Some(&gl_texture);
        let level            = 0;
        self.outputs.push(texture);
        context.bind_framebuffer(target,self.framebuffer.as_ref());
        context.framebuffer_texture_2d(target,attachment_point,texture_target,gl_texture,level);
    }
}
