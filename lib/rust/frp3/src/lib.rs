#![allow(incomplete_features)]
#![feature(const_trait_impl)]
#![feature(coerce_unsized)]
#![feature(unsize)]
#![feature(pointer_byte_offsets)]
#![feature(layout_for_ptr)]
#![feature(specialization)]
#![feature(downcast_unchecked)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(test)]

mod bench;
mod macros;
mod rc_arena;
pub use macros::*;

pub mod nodes;

pub(self) mod data;
pub(self) mod tuple;

mod callstack;
mod runtime;

pub use data::AnyBehavior;
pub use data::AnyEvent;
pub use data::Data;
pub use runtime::Behavior;
pub use runtime::Network;
pub use runtime::Stream;

pub use callstack::Location;
pub use runtime::AsBehavior;
pub use runtime::AsStream;
pub use runtime::Metrics;
pub use runtime::Rt;

pub use enso_prelude as prelude;

pub use nodes::*;
