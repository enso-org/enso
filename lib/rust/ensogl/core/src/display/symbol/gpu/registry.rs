// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2d;
use crate::display::symbol;
use crate::display::symbol::RenderGroup;
use crate::display::symbol::Symbol;
use crate::display::symbol::SymbolId;
use crate::display::symbol::WeakSymbol;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::gpu::Context;



// =============
// === Types ===
// =============

pub type SymbolDirty = dirty::SharedSet<SymbolId, Box<dyn Fn()>>;



// ======================
// === SymbolRegistry ===
// ======================

// === Definition ===

/// Registry for all the created symbols. The `z_zoom_1` value describes the z-axis distance at
/// which the `zoom` value is `1.0`.
#[derive(Clone, CloneRef, Debug)]
pub struct SymbolRegistry {
    // Note: WeakSymbol in the Registry
    symbols:            Rc<RefCell<WeakValueHashMap<SymbolId, WeakSymbol>>>,
    global_id_provider: symbol::GlobalInstanceIdProvider,
    symbol_dirty:       SymbolDirty,
    logger:             Logger,
    view_projection:    Uniform<Matrix4<f32>>,
    z_zoom_1:           Uniform<f32>,
    variables:          UniformScope,
    context:            Rc<RefCell<Option<Context>>>,
    stats:              Stats,
    next_id:            Rc<Cell<u32>>,
}

// Note: WeakSymbol in the Registry
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The `Symbol` references owned by the `SymbolRegistry` don't need to be fully-weak references,
// but it's conceptually simpler than introducing a special-purpose type.
//
// A weak reference type differs from a strong reference in two properties:
// 1. If there are no non-weak references to an object, the object will be dropped.
// 2. A cycle in the reference graph will only prevent an object from being dropped if all
//    references in the cycle are non-weak.
//
// In this case, property 1 is sufficient. The way `Symbol` is implemented (with multiple
// shared-reference fields, i.e. the `CloneRef` pattern), it would be marginally simpler to
// implement a semi-weak type that satisfies property 1, but not property 2; however, a
// general-purpose weak reference is used here because it's a well-known abstraction.

impl SymbolRegistry {
    /// Constructor.
    pub fn mk<OnMut: Fn() + 'static, Log: AnyLogger>(
        variables: &UniformScope,
        stats: &Stats,
        logger: &Log,
        on_mut: OnMut,
    ) -> Self {
        let logger = Logger::new_sub(logger, "symbol_registry");
        debug!("Initializing.");
        let symbol_dirty = SymbolDirty::new(Box::new(on_mut));
        let symbols = default();
        let variables = variables.clone();
        let view_projection = variables.add_or_panic("view_projection", Matrix4::<f32>::identity());
        let z_zoom_1 = variables.add_or_panic("z_zoom_1", 1.0);
        let context = default();
        let stats = stats.clone_ref();
        let global_id_provider = default();
        let next_id = default();
        Self {
            symbols,
            global_id_provider,
            symbol_dirty,
            logger,
            view_projection,
            z_zoom_1,
            variables,
            context,
            stats,
            next_id,
        }
    }

    /// Creates a new `Symbol`.
    #[allow(clippy::new_ret_no_self)]
    pub fn new(&self) -> Symbol {
        let symbol_dirty = self.symbol_dirty.clone();
        let stats = &self.stats;
        let id_value = self.next_id.get();
        self.next_id.set(id_value + 1);
        let id = SymbolId::new(id_value);
        let on_mut = move || symbol_dirty.set(id);
        let symbol = Symbol::new(stats, id, &self.global_id_provider, on_mut);
        symbol.set_context(self.context.borrow().as_ref());
        self.symbols.borrow_mut().insert(id, symbol.clone_ref());
        symbol
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
    pub fn update(&self) {
        debug_span!("Updating.").in_scope(|| {
            let symbols = self.symbols.borrow();
            for id in self.symbol_dirty.take().iter() {
                if let Some(symbol) = symbols.get(id) {
                    symbol.update(&self.variables);
                }
            }
            self.symbol_dirty.unset_all();
        })
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
}
