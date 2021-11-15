//! `BufferUsage` specifies the intended usage pattern of the data store for optimization purposes.

use crate::prelude::*;

use crate::system::gpu::data::gl_enum::GlEnum;
use crate::system::gpu::shader::Context;



// ===================
// === BufferUsage ===
// ===================

crate::define_singleton_enum_gl! { [GlEnum]
    /// Specifies the intended usage pattern of the data store for optimization purposes.
    BufferUsage {

        /// The contents are intended to be specified once by the application, and used many times
        /// as the source for WebGL drawing and image specification commands.
        Static = GlEnum(Context::STATIC_DRAW),

        /// Default. The contents are intended to be respecified repeatedly by the application, and
        /// used many times as the source for WebGL drawing and image specification commands.
        Dynamic = GlEnum(Context::DYNAMIC_DRAW),

        /// The contents are intended to be specified once by the application, and used at most a
        /// few times as the source for WebGL drawing and image specification commands.
        Stream = GlEnum(Context::STREAM_DRAW),

        /// The contents are intended to be specified once by reading data from WebGL, and queried
        /// many times by the application.
        StaticRead = GlEnum(Context::STATIC_READ),

        /// The contents are intended to be respecified repeatedly by reading data from WebGL, and
        /// queried many times by the application.
        DynamicRead = GlEnum(Context::DYNAMIC_READ),

        /// The contents are intended to be specified once by reading data from WebGL, and queried
        /// at most a few times by the application
        StreamRead = GlEnum(Context::STREAM_READ),

        /// The contents are intended to be specified once by reading data from WebGL, and used many
        /// times as the source for WebGL drawing and image specification commands.
        StaticCopy = GlEnum(Context::STATIC_COPY),

        /// The contents are intended to be respecified repeatedly by reading data from WebGL, and
        /// used many times as the source for WebGL drawing and image specification commands.
        DynamicCopy = GlEnum(Context::DYNAMIC_COPY),

        /// The contents are intended to be specified once by reading data from WebGL, and used at
        /// most a few times as the source for WebGL drawing and image specification commands.
        StreamCopy = GlEnum(Context::STREAM_COPY),
    }
}

impl Default for BufferUsage {
    fn default() -> Self {
        BufferUsage::Dynamic
    }
}
