//! This module defines sprites, rectangular planes. All planes share the same material, but can
//! differ in size and can have different attributes driving the look and feel of the material.
//! Sprites are very fast to render. You can expect even millions of sprites to be rendered 60 FPS.

use crate::prelude::*;

use crate::display::object::*;
use crate::display::symbol::geometry::primitive::mesh::InstanceId;
use crate::display::symbol::material::Material;
use crate::display::world::*;

use basegl_system_web::Logger;
use nalgebra::Vector2;
use nalgebra::Vector3;
use nalgebra::Matrix4;



// =================
// === SymbolRef ===
// =================

/// Reference to a specific symbol inside the `World` object.
#[derive(Clone,Debug)]
pub struct SymbolRef {
    world     : World,
    symbol_id : SymbolId,
}

impl SymbolRef {
    /// Constructor.
    pub fn new(world: World, symbol_id:SymbolId) -> Self {
        Self {world,symbol_id}
    }
}



// =================
// === SpriteRef ===
// =================

/// Reference to a specific sprite object inside a `SpriteSystem`.
#[derive(Clone,Debug)]
pub struct SpriteRef {
    symbol_ref  : SymbolRef,
    instance_id : InstanceId,
}

impl SpriteRef {
    /// Constructor.
    pub fn new(symbol_ref:SymbolRef, instance_id:InstanceId) -> Self {
        Self {symbol_ref,instance_id}
    }
}



// ==============
// === Sprite ===
// ==============

/// Sprite is a simple rectangle object. In most cases, sprites always face the camera and can be
/// freely rotated only by their local z-axis. This implementation, however, implements sprites as
/// full 3D objects. We may want to fork this implementation in the future to create a specialized
/// 2d representation as well.
#[derive(Clone,Debug)]
pub struct Sprite {
    rc: Rc<RefCell<SpriteData>>
}

// === Public API ===

impl Sprite {
    /// Modifies the position of the sprite.
    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow().display_object.mod_position(f);
    }

    /// Sets the position of the sprite.
    pub fn set_position(&self, value:Vector3<f32>) {
        self.rc.borrow().display_object.set_position(value)
    }

    /// Modifies the bounding box dimensions of the sprite.
    pub fn mod_bbox<F:FnOnce(&mut Vector2<f32>)>(&self, f:F) {
        self.rc.borrow().bbox.modify(f);
    }

    /// Sets the bounding box dimensions of the sprite.
    pub fn set_bbox(&self, value:Vector2<f32>) {
        self.rc.borrow().bbox.set(value);
    }

    /// Updates the sprite and all of its children.
    pub fn update(&self) {
        self.rc.borrow().update();
    }
}


// === Private API ===

impl Sprite {
    fn new
    (sprite:SpriteRef, transform:Attribute<Matrix4<f32>>, bbox:Attribute<Vector2<f32>>) -> Self {
        let data = SpriteData::new(sprite,transform,bbox);
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }
}

impl From<&Sprite> for DisplayObjectData {
    fn from(t:&Sprite) -> Self {
        t.rc.borrow().display_object.clone_ref()
    }
}



// ==================
// === SpriteData ===
// ==================

#[derive(Debug)]
struct SpriteData {
    sprite_ref     : SpriteRef,
    display_object : DisplayObjectData,
    _transform     : Attribute<Matrix4<f32>>,
    bbox           : Attribute<Vector2<f32>>,
}

impl SpriteData {
    pub fn new
    (sprite_ref:SpriteRef, _transform:Attribute<Matrix4<f32>>, bbox:Attribute<Vector2<f32>>) -> Self {
        let logger         = Logger::new(format!("Sprite{}",sprite_ref.instance_id));
        let display_object = DisplayObjectData::new(logger);
        let transform_cp   = _transform.clone();
        display_object.set_on_updated(move |t| {
            transform_cp.set(t.matrix().clone());
        });

        sprite_ref.symbol_ref.world.mod_stats(|stats| stats.inc_sprite_count());

        Self {sprite_ref,display_object,_transform,bbox}
    }
}

impl From<&SpriteData> for DisplayObjectData {
    fn from(t:&SpriteData) -> Self {
        t.display_object.clone_ref()
    }
}

impl<'t> Modify<&'t DisplayObjectData> for &'t SpriteData {
    fn modify<F:FnOnce(&'t DisplayObjectData)>(self, f:F) {
        f(&self.display_object)
    }
}

impl Drop for SpriteData {
    fn drop(&mut self) {
        self.sprite_ref.symbol_ref.world.mod_stats(|stats| stats.dec_sprite_count());

        let mut world = self.sprite_ref.symbol_ref.world.borrow_mut();
        let symbol    = &mut world.workspace[self.sprite_ref.symbol_ref.symbol_id];
        let mesh      = &mut symbol.surface;
        self.bbox.set(Vector2::new(0.0,0.0));
        mesh.scopes.instance.dispose(self.sprite_ref.instance_id);
        self.display_object.unset_parent();
    }
}



// ====================
// === SpriteSystem ===
// ====================

/// Creates a set of sprites. All sprites in the sprite system share the same material. Sprite
/// system is a very efficient way to display geometry. Sprites are rendered as instances of the
/// same mesh. Each sprite can be controlled by the instance and global attributes.
pub struct SpriteSystem {
    display_object    : DisplayObjectData,
    symbol_ref        : SymbolRef,
    transform         : Buffer<Matrix4<f32>>,
    _uv               : Buffer<Vector2<f32>>,
    bbox              : Buffer<Vector2<f32>>,
}

impl SpriteSystem {
    /// Constructor.
    pub fn new(world:&World) -> Self {
        let logger         = Logger::new("SpriteSystem");
        let display_object = DisplayObjectData::new(logger);
        let world_data     = &mut world.borrow_mut();
        let workspace      = &mut world_data.workspace;
        let symbol_id      = workspace.new_symbol();
        let symbol         = &mut workspace[symbol_id];
        let mesh           = &mut symbol.surface;
        let uv             = mesh.scopes.point.add_buffer("uv");
        let transform      = mesh.scopes.instance.add_buffer("transform");
        let bbox           = mesh.scopes.instance.add_buffer("bounds");

        let geometry_material = Self::geometry_material();
        let material          = Self::material();

        symbol.shader.set_geometry_material (&geometry_material);
        symbol.shader.set_material          (&material);

        let p1_index = mesh.scopes.point.add_instance();
        let p2_index = mesh.scopes.point.add_instance();
        let p3_index = mesh.scopes.point.add_instance();
        let p4_index = mesh.scopes.point.add_instance();

        uv.get(p1_index).set(Vector2::new(0.0, 0.0));
        uv.get(p2_index).set(Vector2::new(0.0, 1.0));
        uv.get(p3_index).set(Vector2::new(1.0, 0.0));
        uv.get(p4_index).set(Vector2::new(1.0, 1.0));

        world_data.stats.inc_sprite_system_count();

        let world      = world.clone_ref();
        let symbol_ref = SymbolRef::new(world,symbol_id);
        Self {display_object,symbol_ref,transform,_uv:uv,bbox}
    }

    /// Creates a new sprite instance.
    pub fn new_instance(&self) -> Sprite {
        let instance_id = {
            let world_data = &mut self.symbol_ref.world.borrow_mut();
            let symbol     = &mut world_data.workspace[self.symbol_ref.symbol_id];
            symbol.surface.instance.add_instance()
        };
        let transform    = self.transform.get(instance_id);
        let bbox         = self.bbox.get(instance_id);
        let sprite_ref   = SpriteRef::new(self.symbol_ref.clone(),instance_id);
        bbox.set(Vector2::new(1.0,1.0));
        let sprite = Sprite::new(sprite_ref,transform,bbox);
        self.add_child(&sprite);
        sprite
    }

    fn geometry_material() -> Material {
        let mut material = Material::new();
        material.add_input  ("bounds"          , Vector2::<f32>::zeros());
        material.add_input  ("uv"              , Vector2::<f32>::zeros());
        material.add_input  ("transform"       , Matrix4::<f32>::identity());
        material.add_input  ("view_projection" , Matrix4::<f32>::identity());
        material.add_output ("local"           , Vector3::<f32>::zeros());
        material.set_main("
                mat4 model_view_projection = input_view_projection * input_transform;
                input_local                = vec3((input_uv - 0.5) * input_bounds, 0.0);
                gl_Position                = model_view_projection * vec4(input_local,1.0);
                ");
        material
    }

    fn material() -> Material {
        let mut material = Material::new();
        material.set_main("output_color = vec4(1.0,1.0,1.0,1.0);");
        material
    }
}

impl From<&SpriteSystem> for DisplayObjectData {
    fn from(t:&SpriteSystem) -> Self {
        t.display_object.clone_ref()
    }
}

impl<'t> Modify<&'t DisplayObjectData> for &'t SpriteSystem {
    fn modify<F:FnOnce(&'t DisplayObjectData)>(self, f:F) {
        f(&self.display_object)
    }
}


// === Setters ===

impl SpriteSystem {
    /// Sets the material for all sprites in this system.
    pub fn set_material<M:Into<Material>>(&mut self, material:M) {
        let world_data = &mut self.symbol_ref.world.borrow_mut();
        let symbol     = &mut world_data.workspace[self.symbol_ref.symbol_id];
        symbol.shader.set_material(material);
    }
}
