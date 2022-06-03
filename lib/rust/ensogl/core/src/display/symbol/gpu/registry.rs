// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2d;
use crate::display::symbol;
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

impl SymbolRegistry {
    /// Constructor.
    pub fn mk<OnMut: Fn() + 'static, Log: AnyLogger>(
        variables: &UniformScope,
        stats: &Stats,
        logger: &Log,
        on_mut: OnMut,
    ) -> Self {
        let logger = Logger::new_sub(logger, "symbol_registry");
        debug!(logger, "Initializing.");
        let symbol_logger = Logger::new_sub(&logger, "symbol_dirty");
        let symbol_dirty = SymbolDirty::new(symbol_logger, Box::new(on_mut));
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
        debug!(self.logger, "Updating.", || {
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
    pub fn render_by_ids(&self, ids: &[SymbolId]) {
        let symbols = self.symbols.borrow();
        for id in ids {
            if let Some(symbol) = symbols.get(id) {
                symbol.render();
            }
        }
    }
}
