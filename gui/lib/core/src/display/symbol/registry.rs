#![allow(missing_docs)]

use crate::prelude::*;

use crate::closure;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::data::function::callback::*;
use crate::data::opt_vec::OptVec;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2D;
use crate::display::render::webgl::Context;
use crate::display::symbol;
use crate::promote;
use crate::promote_all;
use crate::promote_symbol_types;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::web::group;
use crate::system::web::Logger;
use eval_tt::*;
use nalgebra::Matrix4;


// ======================
// === SymbolRegistry ===
// ======================

// === Definition ===

/// Registry for all the created symbols.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct SymbolRegistry<OnMut> {
    pub symbols         : OptVec<Symbol<OnMut>>,
    pub symbol_dirty    : SymbolDirty<OnMut>,
    pub logger          : Logger,
    pub view_projection : Uniform<Matrix4<f32>>,
    pub zoom            : Uniform<f32>,
    variables           : UniformScope,
    context             : Context,
    stats               : Stats,
}


// === Types ===

pub type SymbolId             = usize;
pub type SymbolDirty<OnDirty> = dirty::SharedSet<SymbolId, OnDirty>;
promote_symbol_types!{ [OnSymbolChange] symbol }

#[macro_export]
/// Promote relevant types to parent scope. See `promote!` macro for more information.
macro_rules! promote_symbol_registry_types { ($($args:tt)*) => {
    crate::promote_symbol_types! { $($args)* }
    promote! { $($args)* [SymbolRegistry] }
};}


// === Callbacks ===

closure! {
fn mesh_on_change<C:Callback0> (dirty:SymbolDirty<C>, ix:SymbolId) -> OnSymbolChange {
    || dirty.set(ix)
}}


// === Implementation ===

impl<OnDirty:Callback0> SymbolRegistry<OnDirty> {

    /// Create new instance with the provided on-dirty callback.
    pub fn new(variables:&UniformScope, stats:&Stats, context:&Context, logger:Logger, on_mut:OnDirty) -> Self {
        logger.info("Initializing.");
        let symbol_logger   = logger.sub("symbol_dirty");
        let symbol_dirty    = SymbolDirty::new(symbol_logger, on_mut);
        let symbols         = default();
        let variables       = variables.clone();
        let view_projection = variables.add_or_panic("view_projection", Matrix4::<f32>::identity());
        let zoom            = variables.add_or_panic("zoom"           , 1.0);
        let context         = context.clone();
        let stats           = stats.clone_ref();
        Self {symbols,symbol_dirty,logger,view_projection,zoom,variables,context,stats}
    }

    /// Creates a new `Symbol` instance.
    pub fn new_symbol(&mut self) -> SymbolId {
        let symbol_dirty = self.symbol_dirty.clone();
        let variables    = &self.variables;
        let logger       = &self.logger;
        let context      = &self.context;
        let stats        = &self.stats;
        self.symbols.insert_with_ix(|ix| {
            let on_mut = mesh_on_change(symbol_dirty, ix);
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

    pub fn render(&self, camera:&Camera2D) {
        let changed = camera.update();
        if changed {
            self.view_projection.set(camera.view_projection_matrix());
            self.zoom.set(camera.zoom());
        }
        group!(self.logger, "Rendering.", {
            for symbol in &self.symbols {
                symbol.render();
            }
        })
    }
}

impl<OnDirty> Index<usize> for SymbolRegistry<OnDirty> {
    type Output = Symbol<OnDirty>;
    fn index(&self, ix:usize) -> &Self::Output {
        self.symbols.index(ix)
    }
}

impl<OnDirty> IndexMut<usize> for SymbolRegistry<OnDirty> {
    fn index_mut(&mut self, ix:usize) -> &mut Self::Output {
        self.symbols.index_mut(ix)
    }
}
