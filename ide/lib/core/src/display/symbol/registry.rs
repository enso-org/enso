#![allow(missing_docs)]

use crate::prelude::*;

use crate::closure;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2d;
use crate::display::symbol::Symbol;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::gpu::shader::Context;

use data::opt_vec::OptVec;
use nalgebra::Matrix4;



// ======================
// === SymbolRegistry ===
// ======================

// === Definition ===

/// Registry for all the created symbols.
#[derive(Debug)]
pub struct SymbolRegistry {
    pub symbols         : OptVec<Symbol>,
    pub symbol_dirty    : SymbolDirty,
    pub logger          : Logger,
    pub view_projection : Uniform<Matrix4<f32>>,
    variables           : UniformScope,
    context             : Context,
    stats               : Stats,
}


// === Types ===

pub type SymbolId    = usize;
pub type SymbolDirty = dirty::SharedSet<SymbolId,Box<dyn Fn()>>;


// === Callbacks ===

closure! {
fn mesh_on_change(dirty:SymbolDirty, ix:SymbolId) -> OnSymbolChange {
    || dirty.set(ix)
}}


// === Implementation ===

impl SymbolRegistry {

    /// Create new instance with the provided on-dirty callback.
    pub fn new<OnMut:Fn()+'static>(variables:&UniformScope, stats:&Stats, context:&Context, logger:Logger, on_mut:OnMut) -> Self {
        logger.info("Initializing.");
        let symbol_logger   = logger.sub("symbol_dirty");
        let symbol_dirty    = SymbolDirty::new(symbol_logger,Box::new(on_mut));
        let symbols         = default();
        let variables       = variables.clone();
        let view_projection = variables.add_or_panic("view_projection", Matrix4::<f32>::identity());
        let context         = context.clone();
        let stats           = stats.clone_ref();
        Self {symbols,symbol_dirty,logger,view_projection,variables,context,stats}
    }

    /// Creates a new `Symbol` instance.
    pub fn new_symbol(&mut self) -> SymbolId {
        let symbol_dirty = self.symbol_dirty.clone();
        let variables    = &self.variables;
        let logger       = &self.logger;
        let context      = &self.context;
        let stats        = &self.stats;
        self.symbols.insert_with_ix(|ix| {
            let on_mut = move || {symbol_dirty.set(ix)};
            let logger = logger.sub(format!("symbol{}",ix));
            Symbol::new(variables,logger,stats,context,on_mut)
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            for mesh_id in self.symbol_dirty.take().iter() {
                self.symbols[*mesh_id].update()
            }
            self.symbol_dirty.unset_all();
        })
    }

    pub fn render(&self, camera:&Camera2d) {
        let changed = camera.update();
        if changed {
            self.view_projection.set(camera.view_projection_matrix());
        }
        group!(self.logger, "Rendering.", {
            for symbol in &self.symbols {
                symbol.render();
            }
        })
    }
}

impl Index<usize> for SymbolRegistry {
    type Output = Symbol;
    fn index(&self, ix:usize) -> &Self::Output {
        self.symbols.index(ix)
    }
}

impl IndexMut<usize> for SymbolRegistry {
    fn index_mut(&mut self, ix:usize) -> &mut Self::Output {
        self.symbols.index_mut(ix)
    }
}
