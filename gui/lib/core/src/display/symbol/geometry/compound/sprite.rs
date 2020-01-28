//! This module defines sprites, rectangular planes. All planes share the same material, but can
//! differ in size and can have different attributes driving the look and feel of the material.
//! Sprites are very fast to render. You can expect even millions of sprites to be rendered 60 FPS.

use crate::prelude::*;

use crate::debug::Stats;
use crate::display::object::*;
use crate::display::symbol::material::Material;
use crate::display::symbol::Symbol;
use crate::display::world;
use crate::system::gpu::types::*;



// ==============
// === Sprite ===
// ==============

shared! { Sprite

/// Sprite is a simple rectangle object. In most cases, sprites always face the camera and can be
/// freely rotated only by their local z-axis. This implementation, however, implements sprites as
/// full 3D objects. We may want to fork this implementation in the future to create a specialized
/// 2d representation as well.
#[derive(Debug)]
pub struct SpriteData {
    symbol         : Symbol,
    instance_id    : AttributeInstanceIndex,
    display_object : DisplayObjectData,
    transform      : Attribute<Matrix4<f32>>,
    bbox           : Attribute<Vector2<f32>>,
    stats          : Stats,
}

impl {
    /// Constructor.
    pub fn new
    ( symbol      : &Symbol
    , instance_id : AttributeInstanceIndex
    , transform   : Attribute<Matrix4<f32>>
    , bbox        : Attribute<Vector2<f32>>
    , stats       : &Stats
    ) -> Self {
        stats.inc_sprite_count();
        let symbol         = symbol.clone_ref();
        let logger         = Logger::new(iformat!("Sprite{instance_id}"));
        let display_object = DisplayObjectData::new(logger);
        let stats          = stats.clone_ref();
        display_object.set_on_updated(enclose!((transform) move |t| {transform.set(t.matrix())}));
        Self {symbol,instance_id,display_object,transform,bbox,stats}
    }

    /// Modifies the position of the sprite.
    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.display_object.mod_position(f);
    }

    /// Sets the position of the sprite.
    pub fn set_position(&self, value:Vector3<f32>) {
        self.display_object.set_position(value)
    }

    /// Size accessor.
    pub fn size(&self) -> Attribute<Vector2<f32>> {
        self.bbox.clone_ref()
    }
}}

impl From<&Sprite> for DisplayObjectData {
    fn from(t:&Sprite) -> Self {
        t.rc.borrow().display_object.clone_ref()
    }
}

impl Drop for SpriteData {
    fn drop(&mut self) {
        self.stats.dec_sprite_count();
        self.bbox.set(Vector2::new(0.0,0.0));
        self.symbol.surface().instance_scope().dispose(self.instance_id);
        self.display_object.unset_parent();
    }
}



// ====================
// === SpriteSystem ===
// ====================

shared! { SpriteSystem

/// Creates a set of sprites. All sprites in the sprite system share the same material. Sprite
/// system is a very efficient way to display geometry. Sprites are rendered as instances of the
/// same mesh. Each sprite can be controlled by the instance and global attributes.
#[derive(Debug)]
pub struct SpriteSystemData {
    symbol         : Symbol,
    transform      : Buffer<Matrix4<f32>>,
    uv             : Buffer<Vector2<f32>>,
    size           : Buffer<Vector2<f32>>,
    stats          : Stats,
}

impl {
    /// Constructor.
    pub fn new() -> Self {
        let scene          = world::get_scene();
        let stats          = scene.stats();
        let symbol         = scene.new_symbol();
        let mesh           = symbol.surface();
        let point_scope    = mesh.point_scope();
        let instance_scope = mesh.instance_scope();
        let uv             = point_scope.add_buffer("uv");
        let transform      = instance_scope.add_buffer("transform");
        let size           = instance_scope.add_buffer("bounds");

        stats.inc_sprite_system_count();

        let this = Self {symbol,transform,uv,size,stats};
        this.init_attributes();
        this.init_shader();
        this
    }

    /// Creates a new sprite instance.
    pub fn new_instance(&self) -> Sprite {
        let instance_id = self.symbol.surface().instance_scope().add_instance();
        let transform   = self.transform.at(instance_id);
        let size        = self.size.at(instance_id);
        size.set(Vector2::new(1.0,1.0));
        let sprite = Sprite::new(&self.symbol,instance_id,transform,size,&self.stats);
        self.add_child(&sprite);
        sprite
    }

    /// Accessor.
    pub fn symbol(&self) -> Symbol {
        self.symbol.clone_ref()
    }

    /// Run the renderer.
    pub fn render(&self) {
        self.symbol.render();
    }

    /// Sets the geometry material for all sprites in this system.
    pub fn set_geometry_material<M:Into<Material>>(&mut self, material:M) {
        self.symbol.shader().set_geometry_material(material);
    }

    /// Sets the surface material for all sprites in this system.
    pub fn set_material<M:Into<Material>>(&mut self, material:M) {
        self.symbol.shader().set_material(material);
    }
}}


// === Initialization ===

impl SpriteSystemData {
    fn init_attributes(&self) {
        let mesh        = self.symbol.surface();
        let point_scope = mesh.point_scope();
        let p1_index    = point_scope.add_instance();
        let p2_index    = point_scope.add_instance();
        let p3_index    = point_scope.add_instance();
        let p4_index    = point_scope.add_instance();
        self.uv.at(p1_index).set(Vector2::new(0.0,0.0));
        self.uv.at(p2_index).set(Vector2::new(0.0,1.0));
        self.uv.at(p3_index).set(Vector2::new(1.0,0.0));
        self.uv.at(p4_index).set(Vector2::new(1.0,1.0));
    }

    fn init_shader(&self) {
        let shader            = self.symbol.shader();
        let surface_material  = Self::surface_material();
        let geometry_material = Self::geometry_material();
        shader.set_geometry_material (&geometry_material);
        shader.set_material          (&surface_material);
    }

    fn geometry_material() -> Material {
        let mut material = Material::new();
        material.add_input_def  :: <Vector2<f32>> ("bounds");
        material.add_input_def  :: <Vector2<f32>> ("uv");
        material.add_input_def  :: <Matrix4<f32>> ("transform");
        material.add_input_def  :: <Matrix4<f32>> ("view_projection");
        material.add_output_def :: <Vector3<f32>> ("local");
        material.add_output_def :: <i32>          ("instance_id");
        material.set_main("
                mat4 model_view_projection = input_view_projection * input_transform;
                input_local                = vec3((input_uv - 0.5) * input_bounds, 0.0);
                gl_Position                = model_view_projection * vec4(input_local,1.0);
                input_instance_id          = gl_InstanceID;
                ");
        material
    }

    fn surface_material() -> Material {
        let mut material = Material::new();
        // FIXME We need to use this output, as we need to declare the same amount of shader
        // FIXME outputs as the number of attachments to framebuffer. We should manage this more
        // FIXME intelligent. For example, we could allow defining output shader fragments,
        // FIXME which will be enabled only if pass of given attachment type was enabled.
        material.add_output ("id", Vector4::<u32>::new(0,0,0,0));
        material.set_main("output_color = vec4(0.0,0.0,0.0,1.0);");
        material
    }
}

impl From<&SpriteSystemData> for DisplayObjectData {
    fn from(t:&SpriteSystemData) -> Self {
        t.symbol.display_object()
    }
}

impl From<&SpriteSystem> for DisplayObjectData {
    fn from(t:&SpriteSystem) -> Self {
        t.rc.borrow().display_object()
    }
}
