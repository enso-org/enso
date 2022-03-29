//! This module defines sprites, rectangular planes. All planes share the same material, but can
//! differ in size and can have different attributes driving the look and feel of the material.
//! Sprites are very fast to render. You can expect even millions of sprites to be rendered 60 FPS.

use crate::display::traits::*;
use crate::prelude::*;
use crate::system::gpu::types::*;

use crate::debug::Stats;
use crate::display;
use crate::display::layout::alignment;
use crate::display::layout::Alignment;
use crate::display::scene::Scene;
use crate::display::symbol::material::Material;
use crate::display::symbol::Symbol;
use crate::display::symbol::SymbolId;



// =================
// === Constants ===
// =================

const DEFAULT_SPRITE_SIZE: (f32, f32) = (10.0, 10.0);



// ===================
// === SpriteStats ===
// ===================

/// Wrapper for `Stats` which counts the number of sprites.
#[derive(Debug, Shrinkwrap)]
pub struct SpriteStats {
    stats: Stats,
}

impl SpriteStats {
    /// Constructor.
    pub fn new(stats: &Stats) -> Self {
        stats.inc_sprite_count();
        let stats = stats.clone_ref();
        Self { stats }
    }
}

impl Drop for SpriteStats {
    fn drop(&mut self) {
        self.stats.dec_sprite_count();
    }
}



// ===================
// === SpriteGuard ===
// ===================

/// Lifetime guard for `Sprite`. After sprite is dropped, it is removed from the sprite system.
/// Note that the removal does not involve many changes to buffers. What really happens is setting
/// the sprite dimensions to zero and marking it index as a free for future reuse.
#[derive(Debug)]
pub struct SpriteGuard {
    instance_id:    attribute::InstanceIndex,
    symbol:         Symbol,
    size:           Attribute<Vector2<f32>>,
    display_object: display::object::Instance,
}

impl SpriteGuard {
    fn new(
        instance_id: attribute::InstanceIndex,
        symbol: &Symbol,
        size: &Attribute<Vector2<f32>>,
        display_object: &display::object::Instance,
    ) -> Self {
        let symbol = symbol.clone_ref();
        let size = size.clone_ref();
        let display_object = display_object.clone_ref();
        Self { instance_id, symbol, size, display_object }
    }
}

impl Drop for SpriteGuard {
    fn drop(&mut self) {
        self.size.set(zero());
        self.symbol.surface().instance_scope().dispose(self.instance_id);
        self.display_object.unset_parent();
    }
}



// ============
// === Size ===
// ============

/// Smart wrapper for size attribute of sprite. The size attribute is set to zero in order to hide
/// the sprite. This wrapper remembers the real size when the sprite is hidden and allows changing
/// it without making the sprite appear on the screen.
#[derive(Debug, Clone, CloneRef)]
pub struct Size {
    hidden: Rc<Cell<bool>>,
    value:  Rc<Cell<Vector2<f32>>>,
    attr:   Attribute<Vector2<f32>>,
}

// === Setters ===

impl HasItem for Size {
    type Item = Vector2;
}
impl CellGetter for Size {
    fn get(&self) -> Vector2 {
        self.value.get()
    }
}
impl CellSetter for Size {
    fn set(&self, v: Vector2) {
        self.value.set(v);
        if !self.hidden.get() {
            self.attr.set(v)
        }
    }
}


// === Private API ===

impl Size {
    fn new(attr: Attribute<Vector2<f32>>) -> Self {
        let hidden = Rc::new(Cell::new(true));
        let value = Rc::new(Cell::new(zero()));
        Self { hidden, value, attr }
    }

    fn hide(&self) {
        self.hidden.set(true);
        self.attr.set(zero());
    }

    fn show(&self) {
        self.hidden.set(false);
        self.attr.set(self.value.get());
    }
}



// ==============
// === Sprite ===
// ==============

/// Sprite is a simple rectangle object. In most cases, sprites always face the camera and can be
/// freely rotated only by their local z-axis. This implementation, however, implements sprites as
/// full 3D objects. We may want to fork this implementation in the future to create a specialized
/// 2d representation as well.
#[derive(Debug, Clone, CloneRef)]
#[allow(missing_docs)]
pub struct Sprite {
    pub symbol:      Symbol,
    pub instance_id: attribute::InstanceIndex,
    pub size:        Size,
    display_object:  display::object::Instance,
    transform:       Attribute<Matrix4<f32>>,
    stats:           Rc<SpriteStats>,
    guard:           Rc<SpriteGuard>,
}

impl Sprite {
    /// Constructor.
    pub fn new(
        symbol: &Symbol,
        instance_id: attribute::InstanceIndex,
        transform: Attribute<Matrix4<f32>>,
        size: Attribute<Vector2<f32>>,
        stats: &Stats,
    ) -> Self {
        let symbol = symbol.clone_ref();
        let logger = Logger::new(iformat!("Sprite{instance_id}"));
        let display_object = display::object::Instance::new(logger);
        let stats = Rc::new(SpriteStats::new(stats));
        let guard = Rc::new(SpriteGuard::new(instance_id, &symbol, &size, &display_object));
        let size = Size::new(size);
        let default_size = Vector2(DEFAULT_SPRITE_SIZE.0, DEFAULT_SPRITE_SIZE.1);
        size.set(default_size);
        Self { symbol, instance_id, size, display_object, transform, stats, guard }.init()
    }

    /// Init display object bindings. In particular defines the behavior of the show and hide
    /// callbacks.
    fn init(self) -> Self {
        let size = &self.size;
        let transform = &self.transform;
        self.display_object.set_on_updated(f!((t) transform.set(t.matrix())));
        self.display_object.set_on_hide(f_!(size.hide()));
        self.display_object.set_on_show(f__!(size.show()));
        self
    }

    /// Get the symbol id.
    pub fn symbol_id(&self) -> SymbolId {
        self.symbol.id
    }

    /// Check if given pointer-event-target means this object.
    pub fn is_this_target(&self, target: display::scene::PointerTarget) -> bool {
        match target {
            display::scene::PointerTarget::Background => false,
            display::scene::PointerTarget::Symbol { symbol_id, instance_id } =>
                self.symbol_id() == symbol_id && self.instance_id == instance_id,
        }
    }
}

impl display::Object for Sprite {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ====================
// === SpriteSystem ===
// ====================

/// Creates a set of sprites. All sprites in the sprite system share the same material. Sprite
/// system is a very efficient way to display geometry. Sprites are rendered as instances of the
/// same mesh. Each sprite can be controlled by the instance and global attributes.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct SpriteSystem {
    pub symbol: Symbol,
    transform:  Buffer<Matrix4<f32>>,
    uv:         Buffer<Vector2<f32>>,
    size:       Buffer<Vector2<f32>>,
    alignment:  Uniform<Vector2<f32>>,
    stats:      Stats,
}

impl SpriteSystem {
    /// Constructor.
    pub fn new<'t, S>(scene: S) -> Self
    where S: Into<&'t Scene> {
        let scene = scene.into();
        let stats = scene.stats.clone_ref();
        let symbol = scene.new_symbol();
        let mesh = symbol.surface();
        let point_scope = mesh.point_scope();
        let instance_scope = mesh.instance_scope();
        let uv = point_scope.add_buffer("uv");
        let transform = instance_scope.add_buffer("transform");
        let size = instance_scope.add_buffer("size");
        let initial_alignment = Self::uv_offset(Alignment::center());
        let alignment = symbol.variables().add_or_panic("alignment", initial_alignment);

        stats.inc_sprite_system_count();

        let this = Self { symbol, transform, uv, size, alignment, stats };
        this.init_attributes();
        this.init_shader();
        this
    }

    /// Creates a new sprite instance.
    pub fn new_instance(&self) -> Sprite {
        let instance_id = self.symbol.surface().instance_scope().add_instance();
        let transform = self.transform.at(instance_id);
        let size = self.size.at(instance_id);
        let sprite = Sprite::new(&self.symbol, instance_id, transform, size, &self.stats);
        self.add_child(&sprite);
        sprite
    }

    /// Hide the symbol. Hidden symbols will not be rendered.
    pub fn hide(&self) {
        self.symbol.hide();
    }

    /// Show the symbol. It will be rendered on next render call.
    pub fn show(&self) {
        self.symbol.show();
    }

    /// Accessor.
    pub fn symbol(&self) -> Symbol {
        self.symbol.clone_ref()
    }

    /// Set alignment of sprites.
    pub fn set_alignment(&self, alignment: Alignment) {
        self.alignment.set(Self::uv_offset(alignment));
    }

    /// Run the renderer.
    pub fn render(&self) {
        self.symbol.render();
    }

    /// Sets the geometry material for all sprites in this system.
    pub fn set_geometry_material<M: Into<Material>>(&self, material: M) {
        self.symbol.shader().set_geometry_material(material);
    }

    /// Sets the surface material for all sprites in this system.
    pub fn set_material<M: Into<Material>>(&self, material: M) {
        self.symbol.shader().set_material(material);
    }
}


// === Initialization ===

impl SpriteSystem {
    fn init_attributes(&self) {
        let mesh = self.symbol.surface();
        let point_scope = mesh.point_scope();
        let p1_index = point_scope.add_instance();
        let p2_index = point_scope.add_instance();
        let p3_index = point_scope.add_instance();
        let p4_index = point_scope.add_instance();
        self.uv.at(p1_index).set(Vector2::new(0.0, 0.0));
        self.uv.at(p2_index).set(Vector2::new(0.0, 1.0));
        self.uv.at(p3_index).set(Vector2::new(1.0, 0.0));
        self.uv.at(p4_index).set(Vector2::new(1.0, 1.0));
    }

    fn init_shader(&self) {
        let shader = self.symbol.shader();
        let surface_material = Self::surface_material();
        let geometry_material = Self::geometry_material();
        shader.set_geometry_material(&geometry_material);
        shader.set_material(&surface_material);
    }

    fn geometry_material() -> Material {
        let mut material = Material::new();
        material.add_input_def::<Vector2<f32>>("size");
        material.add_input_def::<Vector2<f32>>("uv");
        material.add_input_def::<Matrix4<f32>>("transform");
        material.add_input_def::<Matrix4<f32>>("view_projection");
        material.add_input_def::<Vector2<f32>>("alignment");
        material.add_output_def::<Vector3<f32>>("local");
        material.add_output_def::<i32>("instance_id");
        material.set_main(
            "
                mat4 model_view_projection = input_view_projection * input_transform;
                input_local                = vec3((input_uv - input_alignment) * input_size, 0.0);
                gl_Position                = model_view_projection * vec4(input_local,1.0);
                input_local.z              = gl_Position.z;
                input_instance_id          = gl_InstanceID;
                ",
        );
        material
    }

    fn surface_material() -> Material {
        let mut material = Material::new();
        // FIXME We need to use this output, as we need to declare the same amount of shader
        // FIXME outputs as the number of attachments to framebuffer. We should manage this more
        // FIXME intelligent. For example, we could allow defining output shader fragments,
        // FIXME which will be enabled only if pass of given attachment type was enabled.
        material.add_output("id", Vector4::<f32>::new(0.0, 0.0, 0.0, 0.0));
        material.set_main("output_color = vec4(0.0,0.0,0.0,1.0); output_id=vec4(0.0,0.0,0.0,0.0);");
        material
    }

    fn uv_offset(alignment: Alignment) -> Vector2<f32> {
        let x = match alignment.horizontal {
            alignment::Horizontal::Left => 0.0,
            alignment::Horizontal::Center => 0.5,
            alignment::Horizontal::Right => 1.0,
        };
        let y = match alignment.vertical {
            alignment::Vertical::Top => 1.0,
            alignment::Vertical::Center => 0.5,
            alignment::Vertical::Bottom => 0.0,
        };
        Vector2::new(x, y)
    }
}

impl display::Object for SpriteSystem {
    fn display_object(&self) -> &display::object::Instance {
        self.symbol.display_object()
    }
}
