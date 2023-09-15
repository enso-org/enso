//! This module defines sprites, rectangular planes. All planes share the same material, but can
//! differ in size and can have different attributes driving the look and feel of the material.
//! Sprites are very fast to render. You can expect even millions of sprites to be rendered 60 FPS.

use crate::prelude::*;
use crate::system::gpu::types::*;

use crate::debug::Stats;
use crate::display;
use crate::display::attribute::EraseOnDrop;
use crate::display::layout::alignment;
use crate::display::symbol::material::Material;
use crate::display::symbol::Symbol;
use crate::display::symbol::SymbolId;
use crate::display::symbol::SymbolInstance;
use crate::display::world;



// =================
// === Constants ===
// =================

const DEFAULT_SPRITE_SIZE: (f32, f32) = (10.0, 10.0);



// ===================
// === SpriteStats ===
// ===================

/// Wrapper for `Stats` which counts the number of sprites.
#[derive(Debug, Deref)]
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



// ============
// === Size ===
// ============

/// Smart wrapper for size attribute of sprite. The size attribute is set to zero in order to hide
/// the sprite. This wrapper remembers the real size when the sprite is hidden and allows changing
/// it without making the sprite appear on the screen.
#[derive(Debug, Clone)]
pub struct Size {
    hidden: Cell<bool>,
    value:  Cell<Vector2<f32>>,
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
        let hidden = Cell::new(true);
        let value = Cell::new(zero());
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

/// A sprite is a simple rectangle object. In most cases, sprites always face the camera and can be
/// freely rotated only by their local z-axis. This implementation, however, implements sprites as
/// full 3D objects. We may want to fork this implementation in the future to create a specialized
/// 2d representation as well.
///
/// This type is "raw": It implements the GPU side of a sprite. For a raw sprite to be fit into a
/// display object hierarchy, it should be controlled by the event handlers of a display object.
/// [`Sprite`] does this in the simplest way, which is not compatible with layers. [`ShapeSystem`]
/// does so with an indirection allowing one display object to be associated with different
/// [`Sprite`]s, for full layer support.
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct RawSprite {
    model: Rc<SpriteModel>,
}

/// Internal representation of [`RawSprite`].
#[derive(Debug, Deref)]
#[allow(missing_docs)]
pub struct SpriteModel {
    #[deref]
    pub instance:  SymbolInstance,
    pub symbol:    Symbol,
    size:          Size,
    transform:     Attribute<Matrix4<f32>>,
    stats:         SpriteStats,
    erase_on_drop: EraseOnDrop<Attribute<Vector2<f32>>>,
}

impl SpriteModel {
    /// Constructor.
    pub fn new(
        symbol: &Symbol,
        instance: SymbolInstance,
        transform: Attribute<Matrix4<f32>>,
        size: Attribute<Vector2<f32>>,
        stats: &Stats,
    ) -> Self {
        let symbol = symbol.clone_ref();
        let stats = SpriteStats::new(stats);
        let erase_on_drop = EraseOnDrop::new(size.clone_ref());
        let size = Size::new(size);
        let default_size = Vector2(DEFAULT_SPRITE_SIZE.0, DEFAULT_SPRITE_SIZE.1);
        size.set(default_size);
        Self { symbol, instance, size, transform, stats, erase_on_drop }
    }
}

impl RawSprite {
    /// Constructor.
    pub fn new(
        symbol: &Symbol,
        instance: SymbolInstance,
        transform: Attribute<Matrix4<f32>>,
        size: Attribute<Vector2<f32>>,
        stats: &Stats,
    ) -> Self {
        let model = SpriteModel::new(symbol, instance, transform, size, stats);
        Self { model: Rc::new(model) }
    }

    /// Get the symbol id.
    pub fn symbol_id(&self) -> SymbolId {
        self.symbol.id
    }

    /// Check if given pointer-event-target means this object.
    pub fn is_this_target(&self, target: display::scene::PointerTargetId) -> bool {
        match target {
            display::scene::PointerTargetId::Background => false,
            display::scene::PointerTargetId::Symbol { id } => self.global_instance_id == id,
        }
    }

    /// Turn on drawing the sprite.
    pub fn show(&self) {
        self.size.show();
    }

    /// Turn off drawing the sprite.
    pub fn hide(&self) {
        self.size.hide();
    }

    /// Set the sprite's transformation matrix.
    pub fn set_transform(&self, transform: Matrix4<f32>) {
        self.transform.set(transform);
    }

    /// Set the sprite's dimensions.
    pub fn set_size(&self, size: Vector2<f32>) {
        self.size.set(size);
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
    pub symbol:      Symbol,
    transform:       Buffer<Matrix4<f32>>,
    uv:              Buffer<Vector2<f32>>,
    size:            Buffer<Vector2<f32>>,
    alignment:       Uniform<Vector2<f32>>,
    alignment_value: Rc<Cell<alignment::Dim2>>,
    stats:           Stats,
}

impl SpriteSystem {
    /// Constructor.
    #[profile(Detail)]
    pub fn new(label: &'static str, alignment: alignment::Dim2) -> Self {
        let (stats, symbol) = world::with_context(|t| (t.stats.clone_ref(), t.new(label)));
        let mesh = symbol.surface();
        let point_scope = mesh.point_scope();
        let instance_scope = mesh.instance_scope();
        let uv = point_scope.add_buffer("uv");
        let transform = instance_scope.add_buffer("transform");
        let size = instance_scope.add_buffer("size");
        let alignment_value = Rc::new(Cell::new(alignment));
        let initial_alignment = alignment_value.get().normalized();
        let alignment = symbol.variables.borrow_mut().add_or_panic("alignment", initial_alignment);

        stats.inc_sprite_system_count();

        let this = Self { symbol, transform, uv, size, alignment, alignment_value, stats };
        this.init_attributes();
        this.init_shader();
        this
    }

    /// Creates a new sprite instance.
    pub fn new_instance(&self) -> RawSprite {
        self.new_instance_at(default())
    }

    /// Creates a new sprite instance in the specified buffer.
    pub fn new_instance_at(&self, buffer_partition: attribute::BufferPartitionId) -> RawSprite {
        let instance = self.symbol.new_instance(buffer_partition);
        let transform = self.transform.at(instance.instance_id);
        let size = self.size.at(instance.instance_id);
        RawSprite::new(&self.symbol, instance, transform, size, &self.stats)
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
    ///
    /// # Safety
    /// It is advised not to use this function. Use display object auto-layout instead. This
    /// function can be called only before creating sprites, as its changes will not be
    /// propagated to sprite instances.
    pub fn unsafe_set_alignment(&self, alignment: alignment::Dim2) {
        self.alignment_value.set(alignment);
        self.alignment.set(alignment.normalized());
    }

    /// Run the renderer.
    pub fn render(&self) {
        self.symbol.render();
    }

    /// Sets the geometry material for all sprites in this system.
    pub fn set_geometry_material<M: Into<Material>>(&self, material: M) {
        self.symbol.shader.borrow_mut().set_geometry_material(material);
    }

    /// Sets the surface material for all sprites in this system.
    pub fn set_material<M: Into<Material>>(&self, material: M) {
        self.symbol.shader.borrow_mut().set_material(material);
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
        let mut shader = self.symbol.shader.borrow_mut();
        let surface_material = Self::default_surface_material();
        let geometry_material = Self::default_geometry_material();
        shader.set_geometry_material(geometry_material);
        shader.set_material(surface_material);
    }

    /// The default geometry material for all sprites.
    pub fn default_geometry_material() -> Material {
        let mut material = Material::new();
        material.add_input_def::<Vector2<f32>>("size");
        material.add_input_def::<Vector2<f32>>("uv");
        material.add_input_def::<Matrix4<f32>>("transform");
        material.add_input_def::<Matrix4<f32>>("view_projection");
        material.add_input_def::<Vector2<f32>>("alignment");
        material.add_input_def::<i32>("global_instance_id");
        material.add_output_def::<Vector3<f32>>("local");
        material.set_main(
            "
                mat4 model_view_projection = input_view_projection * input_transform;
                input_local = vec3((input_uv - input_alignment) * input_size, 0.0);
                gl_Position = model_view_projection * vec4(input_local,1.0);
                input_local.z = gl_Position.z;
            ",
            // This is left here in case it will be needed. The `instance_id` is the same as the
            // built-in `gl_InstanceID` and can be implemented very efficiently:
            // input_instance_id = gl_InstanceID;
        );
        material
    }

    /// The default surface material for all sprites.
    pub fn default_surface_material() -> Material {
        let mut material = Material::new();
        // FIXME We need to use this output, as we need to declare the same amount of shader
        // FIXME outputs as the number of attachments to framebuffer. We should manage this more
        // FIXME intelligent. For example, we could allow defining output shader fragments,
        // FIXME which will be enabled only if pass of given attachment type was enabled.
        material.add_output("id", Vector4::<f32>::new(0.0, 0.0, 0.0, 0.0));
        material.set_main("output_color = vec4(0.0,0.0,0.0,1.0); output_id=vec4(0.0,0.0,0.0,0.0);");
        material
    }
}



// ==============
// === Sprite ===
// ==============

/// A [`RawSprite`] with an associated display object.
///
/// This simple construct allows configuring sprites via display objects without any other layer
/// such as a [`ShapeSystem`]; however, because the display object is permanently bound to an
/// instance from a particular [`SpriteSystem`], it is not possible to implement layer operations
/// for such objects.
#[derive(Debug, Clone, CloneRef, Deref, display::Object)]
pub struct Sprite {
    #[deref]
    sprite:         RawSprite,
    display_object: display::object::Instance,
}

impl Sprite {
    /// Create a display object and bind it to the sprite.
    pub fn new(sprite: RawSprite) -> Self {
        let display_object = default();
        let this = Self { sprite, display_object };
        this.init_display_object_events();
        this
    }

    fn init_display_object_events(&self) {
        let display_object = &self.display_object;
        let network = &self.display_object.network;
        let weak_display_object = display_object.downgrade();
        let sprite = &self.sprite;
        frp::extend! { network
            eval_ display_object.on_transformed ([sprite, weak_display_object] {
                if let Some(display_object) = weak_display_object.upgrade() {
                    sprite.set_transform(display_object.transformation_matrix());
                    sprite.set_size(display_object.computed_size());
                }
            });
            eval_ display_object.on_show(sprite.show());
            eval_ display_object.on_hide(sprite.hide());
        }
    }
}
