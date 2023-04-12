// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::dirty::traits::*;
use crate::prelude::*;
use crate::system::web::traits::*;

use crate::data::dirty;
use crate::debug;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2d;
use crate::display::scene;
use crate::display::style;
use crate::display::style::theme;
use crate::display::symbol;
use crate::display::symbol::RenderGroup;
use crate::display::symbol::Symbol;
use crate::display::symbol::SymbolId;
use crate::display::symbol::WeakSymbol;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::gpu::Context;
use crate::system::web;



// =============
// === Types ===
// =============

pub type Dirty = dirty::SharedSet<SymbolId>;


// ===============
// === RunMode ===
// ===============

/// The application run mode. It can be set to either [`Normal`] or [`ShaderExtraction`] mode. In
/// the latter case some warnings are suppressed. For example, there will be no warning that
/// precompiled shapes shaders were not found, as they are yet to be generated.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum RunMode {
    #[default]
    Normal,
    ShaderExtraction,
}

impl RunMode {
    /// Check whether the mode is set to [`Normal`].
    pub fn is_normal(self) -> bool {
        self == Self::Normal
    }

    /// Check whether the mode is set to [`ShaderExtraction`].
    pub fn is_shader_extraction(self) -> bool {
        self == Self::ShaderExtraction
    }
}


// ======================
// === SymbolRegistry ===
// ======================

// === Definition ===

/// Registry for all the created symbols. The `z_zoom_1` value describes the z-axis distance at
/// which the `zoom` value is `1.0`.
///
/// # Implementation Details
/// The `Symbol` references owned by the `SymbolRegistry` don't need to be fully-weak references,
/// but it's conceptually simpler than introducing a special-purpose type.
///
/// A weak reference type differs from a strong reference in two properties:
/// 1. If there are no non-weak references to an object, the object will be dropped.
/// 2. A cycle in the reference graph will only prevent an object from being dropped if all
///    references in the cycle are non-weak.
///
/// In this case, property 1 is sufficient. The way `Symbol` is implemented (with multiple
/// shared-reference fields, i.e. the `CloneRef` pattern), it would be marginally simpler to
/// implement a semi-weak type that satisfies property 1, but not property 2; however, a
/// general-purpose weak reference is used here because it's a well-known abstraction.
#[derive(Clone, CloneRef, Debug)]
pub struct SymbolRegistry {
    pub run_mode:       Rc<Cell<RunMode>>,
    symbols:            Rc<RefCell<WeakValueHashMap<SymbolId, WeakSymbol>>>,
    global_id_provider: symbol::GlobalInstanceIdProvider,
    pub dirty:          Dirty,
    view_projection:    Uniform<Matrix4<f32>>,
    z_zoom_1:           Uniform<f32>,
    pub display_mode:   Uniform<i32>,
    pub variables:      UniformScope,
    context:            Rc<RefCell<Option<Context>>>,
    pub stats:          Stats,
    next_id:            Rc<Cell<u32>>,
    pub style_sheet:    style::Sheet,
    pub theme_manager:  theme::Manager,
    pub layers:         scene::HardcodedLayers,
}

impl SymbolRegistry {
    /// Constructor.
    pub fn mk() -> Self {
        debug!("Initializing.");
        let run_mode = default();
        let dirty = Dirty::new(());
        let symbols = default();
        let variables = UniformScope::new();
        let view_projection = variables.add_or_panic("view_projection", Matrix4::<f32>::identity());
        let z_zoom_1 = variables.add_or_panic("z_zoom_1", 1.0);
        let display_mode = variables.add_or_panic("display_mode", 0);
        let context = default();
        let stats = debug::stats::Stats::new(web::window.performance_or_panic());
        let global_id_provider = default();
        let next_id = default();
        let style_sheet = style::Sheet::new();
        let theme_manager = theme::Manager::from(&style_sheet);
        let layers = scene::HardcodedLayers::new();
        Self {
            run_mode,
            symbols,
            global_id_provider,
            dirty,
            view_projection,
            z_zoom_1,
            display_mode,
            variables,
            context,
            stats,
            next_id,
            style_sheet,
            theme_manager,
            layers,
        }
    }

    /// Creates a new `Symbol`.
    #[allow(clippy::new_ret_no_self)]
    pub fn new(&self, label: &'static str) -> Symbol {
        let dirty = self.dirty.clone();
        let stats = &self.stats;
        let id_value = self.next_id.get();
        self.next_id.set(id_value + 1);
        let id = SymbolId::new(id_value);
        let on_mut = move || dirty.set(id);
        let symbol = Symbol::new(stats, label, id, &self.global_id_provider, on_mut);
        symbol.set_context(self.context.borrow().as_ref());
        self.symbols.borrow_mut().insert(id, symbol.clone_ref());
        symbol
    }

    pub fn get_symbol(&self, id: SymbolId) -> Option<Symbol> {
        self.symbols.borrow().get(&id)
    }

    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    pub fn set_context(&self, context: Option<&Context>) {
        *self.context.borrow_mut() = context.cloned();
        for symbol in self.symbols.borrow().values() {
            symbol.set_context(context)
        }
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&self) -> bool {
        if self.dirty.check_all() {
            debug_span!("Updating.").in_scope(|| {
                let symbols = self.symbols.borrow();
                for id in self.dirty.take().iter() {
                    if let Some(symbol) = symbols.get(id) {
                        symbol.update(&self.variables);
                    }
                }
                self.dirty.unset_all();
            });
            true
        } else {
            false
        }
    }

    /// Updates the view-projection matrix after camera movement.
    pub fn set_camera(&self, camera: &Camera2d) {
        self.view_projection.set(camera.view_projection_matrix());
        self.z_zoom_1.set(camera.z_zoom_1());
    }

    /// Rasterize selected symbols.
    pub fn render_symbols(&self, to_render: &RenderGroup) {
        let mut symbols = to_render.symbols.borrow_mut();
        let symbols = symbols.get_or_insert_with(|| {
            let id_to_symbol = self.symbols.borrow();
            to_render
                .ids
                .iter()
                .filter_map(|id| id_to_symbol.get(id))
                .map(|symbol| WeakSymbol::new(&symbol))
                .collect()
        });
        for symbol in symbols {
            if let Some(symbol) = symbol.view() {
                symbol.render();
            }
        }
    }

    pub fn all_ids(&self) -> Vec<SymbolId> {
        self.symbols.borrow().keys().copied().collect()
    }
}
