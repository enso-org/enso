//! This module defines texture sampler types.

use crate::prelude::*;

use crate::system::gpu::types::*;



// ===============
// === Sampler ===
// ===============

macro_rules! define_samplers {
    ( $($name:ident => $expr:expr),* $(,)? ) => {
        crate::define_singleton_enum_gl! { [glsl::PrimType]
            /// Defines a type of sampler used to access the texture.
            AnySampler {
                $($name = $expr),*
            }
        }

        $(
            impl GpuDefault for $name {
                fn gpu_default() -> Self {
                    default()
                }
            }

            impl TryFrom<$name> for Glsl {
                type Error = glsl::NotGlslError;
                fn try_from(_:$name) -> Result<Self, Self::Error> {
                    Err(glsl::NotGlslError)
                }
            }
        )*
    }
}

/// Trait describing any texture sampler.
pub trait Sampler = Into<AnySampler> + PhantomInto<glsl::PrimType>;

define_samplers! {
    FloatSampler => Self::Sampler2d,
    IntSampler   => Self::ISampler2d,
    UIntSampler  => Self::USampler2d,
}
