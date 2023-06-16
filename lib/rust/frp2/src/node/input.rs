//! FRP node input type implementation.

use crate::prelude::*;

use crate::node::Node;
use crate::runtime::NodeId;



// ============
// === Type ===
// ============

macro_rules! gen {
    () => { gen!{
        Listen          { listener: true,  sampler: false },
        ListenAndSample { listener: true,  sampler: true  },
        Sample          { listener: false, sampler: true  },
    }};

    ( $($variant: ident { listener: $is_listener:tt, sampler: $is_sampler:tt }),* $(,)? ) => {
        /// One of possible node input types. To learn more about node types, see docs of this
        /// crate.
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #[allow(missing_docs)]
        pub enum Type {
            $($variant(NodeId)),*
        }

        $(
            /// One of possible node input types. To learn more about node types, see docs of this
            /// crate.
            #[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, PartialEq)]
            #[repr(transparent)]
            pub struct $variant<T>(pub(crate) T);
        )*

        impl Type {
            /// Get the node id.
            #[inline(always)]
            pub fn node_id(self) -> NodeId {
                match self { $(Self::$variant(t) => t),* }
            }

            /// Check whether the input type is s sampler.
            #[inline(always)]
            pub fn is_sampler(self) -> bool {
                match self { $(Self::$variant(_) => $is_sampler),* }
            }

            /// Check whether the input type is a listener.
            #[inline(always)]
            pub fn is_listener(self) -> bool {
                match self { $(Self::$variant(_) => $is_listener),* }
            }
        }

        $(
            impl<T: Node> From<$variant<T>> for Type {
                #[inline(always)]
                fn from(t: $variant<T>) -> Self {
                    Self::$variant(t.0.id())
                }
            }
        )*
    };
}
gen!();
