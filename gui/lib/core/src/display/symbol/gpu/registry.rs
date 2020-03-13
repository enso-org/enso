#![allow(missing_docs)]

use crate::prelude::*;

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

use shapely::shared;



// === Types ===

pub type SymbolId    = usize;
pub type SymbolDirty = dirty::SharedSet<SymbolId,Box<dyn Fn()>>;





// ======================
// === SymbolRegistry ===
// ======================

// === Definition ===

shared! { SymbolRegistry

/// Registry for all the created symbols.
#[derive(Debug)]
pub struct SymbolRegistryData {
    symbols         : OptVec<Symbol>,
    symbol_dirty    : SymbolDirty,
    logger          : Logger,
    view_projection : Uniform<Matrix4<f32>>,
    variables       : UniformScope,
    context         : Context,
    stats           : Stats,
}

impl {

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
    pub fn new_symbol_by_id(&mut self) -> SymbolId {
        let symbol_dirty = self.symbol_dirty.clone();
        let variables    = &self.variables;
        let logger       = &self.logger;
        let context      = &self.context;
        let stats        = &self.stats;
        self.symbols.insert_with_ix(|ix| {
            let on_mut = move || {symbol_dirty.set(ix)};
            let logger = logger.sub(format!("symbol{}",ix));
            let id     = ix as i32;
            Symbol::new(logger,context,stats,id,variables,on_mut)
        })
    }

    /// Creates a new `Symbol` instance.
    pub fn new_symbol(&mut self) -> Symbol {
        let ix = self.new_symbol_by_id();
        self.index(ix)
    }

    pub fn index(&self, ix:usize) -> Symbol {
        self.symbols[ix].clone_ref()
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            for id in self.symbol_dirty.take().iter() {
                self.symbols[*id].update()
            }
            self.symbol_dirty.unset_all();
        })
    }

    /// Updates the view-projection matrix after camera movement.
    pub fn update_view_projection(&self, camera:&Camera2d) {
        self.view_projection.set(camera.view_projection_matrix());
    }

    pub fn render(&self) {
        for symbol in &self.symbols {
            symbol.render()
        }
    }
}}
