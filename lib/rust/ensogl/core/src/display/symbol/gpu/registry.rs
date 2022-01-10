#![allow(missing_docs)]

use crate::prelude::*;

use crate::data::dirty::traits::*;

use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2d;
use crate::display::symbol::Symbol;
use crate::display::symbol::SymbolId;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::gpu::shader::Context;

use data::opt_vec::OptVec;



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
    symbols:         Rc<RefCell<OptVec<Symbol>>>,
    symbol_dirty:    SymbolDirty,
    logger:          Logger,
    view_projection: Uniform<Matrix4<f32>>,
    z_zoom_1:        Uniform<f32>,
    variables:       UniformScope,
    context:         Rc<RefCell<Option<Context>>>,
    stats:           Stats,
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
        Self { symbols, symbol_dirty, logger, view_projection, z_zoom_1, variables, context, stats }
    }

    /// Creates a new `Symbol` instance and returns its id.
    pub fn new_get_id(&self) -> SymbolId {
        let symbol_dirty = self.symbol_dirty.clone();
        let variables = &self.variables;
        let logger = &self.logger;
        let stats = &self.stats;
        let index = self.symbols.borrow_mut().insert_with_ix_(|ix| {
            let id = SymbolId::new(ix as u32);
            let on_mut = move || symbol_dirty.set(id);
            let logger = Logger::new_sub(logger, format!("symbol_{}", ix));
            let symbol = Symbol::new(logger, stats, id, variables, on_mut);
            symbol.set_context(self.context.borrow().as_ref());
            symbol
        });
        SymbolId::new(index as u32)
    }

    /// Set the WebGL context. See the main architecture docs of this library to learn more.
    pub fn set_context(&self, context: Option<&Context>) {
        *self.context.borrow_mut() = context.cloned();
        for symbol in &*self.symbols.borrow() {
            symbol.set_context(context)
        }
    }

    /// Creates a new `Symbol` instance.
    #[allow(clippy::new_ret_no_self)]
    pub fn new(&self) -> Symbol {
        let ix = self.new_get_id();
        self.index(ix)
    }

    /// Get symbol by its ID.
    pub fn index(&self, id: SymbolId) -> Symbol {
        self.symbols.borrow()[(*id) as usize].clone_ref()
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&self) {
        debug!(self.logger, "Updating.", || {
            for id in self.symbol_dirty.take().iter() {
                self.symbols.borrow()[(**id) as usize].update()
            }
            self.symbol_dirty.unset_all();
        })
    }

    /// Updates the view-projection matrix after camera movement.
    pub fn set_camera(&self, camera: &Camera2d) {
        self.view_projection.set(camera.view_projection_matrix());
        self.z_zoom_1.set(camera.z_zoom_1());
    }

    /// Rasterize all symbols.
    pub fn render_all(&self) {
        for symbol in &*self.symbols.borrow() {
            symbol.render()
        }
    }

    /// Rasterize selected symbols.
    pub fn render_by_ids(&self, ids: &[SymbolId]) {
        let symbols = self.symbols.borrow();
        for id in ids {
            symbols[(**id) as usize].render();
        }
    }
}
