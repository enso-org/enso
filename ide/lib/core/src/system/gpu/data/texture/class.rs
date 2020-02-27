//! The core texture data type and related operations.

use crate::prelude::*;

use crate::system::gpu::Context;
use crate::system::gpu::data::buffer::item::JsBufferViewArr;
use crate::system::gpu::data::gl_enum::*;
use crate::system::gpu::data::gl_enum::traits::*;
use crate::system::gpu::data::texture::storage::*;
use crate::system::gpu::data::texture::types::*;

use web_sys::WebGlTexture;



// ===================
// === TextureUnit ===
// ===================

/// A texture unit representation in WebGl.
#[derive(Copy,Clone,Debug,Display,From,Into)]
pub struct TextureUnit(u32);



// ========================
// === TextureBindGuard ===
// ========================

/// Guard which unbinds texture in specific texture unit on drop.
#[derive(Debug)]
pub struct TextureBindGuard {
    context : Context,
    target  : u32,
    unit    : TextureUnit,
}

impl Drop for TextureBindGuard {
    fn drop(&mut self) {
        self.context.active_texture(Context::TEXTURE0 + self.unit.to::<u32>());
        self.context.bind_texture(self.target,None);
        self.context.active_texture(Context::TEXTURE0);
    }
}



// ===============
// === Texture ===
// ===============

/// Texture bound to GL context.
#[derive(Derivative)]
#[derivative(Clone(bound="StorageOf<Storage,InternalFormat,ItemType>:Clone"))]
#[derivative(Debug(bound="StorageOf<Storage,InternalFormat,ItemType>:Debug"))]
pub struct Texture<Storage,InternalFormat,ItemType>
where Storage: StorageRelation<InternalFormat,ItemType> {
    storage    : StorageOf<Storage,InternalFormat,ItemType>,
    gl_texture : WebGlTexture,
    context    : Context,
}


// === Traits ===

/// Reloading functionality for textured. It is also used for initial data population.
pub trait TextureReload {
    /// Loads or re-loads the texture data from provided source.
    fn reload(&self);
}


// === Type Level Utils ===

impl<S,I,T> Texture<S,I,T>
where S:StorageRelation<I,T>, I:InternalFormat, T:ItemType {
    /// Internal format instance of this texture.
    pub fn internal_format() -> AnyInternalFormat {
        <I>::default().into()
    }

    /// Format instance of this texture.
    pub fn format() -> AnyFormat {
        <I>::Format::default().into()
    }

    /// Internal format of this texture as `GlEnum`.
    pub fn gl_internal_format() -> i32 {
        let GlEnum(u) = Self::internal_format().into_gl_enum();
        u as i32
    }

    /// Format of this texture as `GlEnum`.
    pub fn gl_format() -> GlEnum {
        Self::format().into_gl_enum()
    }

    /// Element type of this texture as `GlEnum`.
    pub fn gl_elem_type() -> u32 {
        <T>::gl_enum().into()
    }

    /// Element type of this texture.
    pub fn item_type() -> AnyItemType {
        PhantomData::<T>.into()
    }
}


// === Getters ===

impl<S,I,T> Texture<S,I,T>
where S:StorageRelation<I,T> {
    /// Getter.
    pub fn gl_texture(&self) -> &WebGlTexture {
        &self.gl_texture
    }

    /// Getter.
    pub fn context(&self) -> &Context {
        &self.context
    }

    /// Getter.
    pub fn storage(&self) -> &StorageOf<S,I,T> {
        &self.storage
    }
}


// === Constructors ===

impl<S:StorageRelation<I,T>,I:InternalFormat,T:ItemType> Texture<S,I,T>
where Self:TextureReload {
    /// Constructor.
    pub fn new<P:Into<StorageOf<S,I,T>>>(context:&Context, provider:P) -> Self {
        let this = Self::new_uninitialized(context,provider);
        this.reload();
        this
    }
}


// === Destructos ===

impl<S,I,T> Drop for Texture<S,I,T>
where S:StorageRelation<I,T> {
    fn drop(&mut self) {
        self.context.delete_texture(Some(&self.gl_texture));
    }
}


// === Internal API ===

impl<S,I,T> Texture<S,I,T>
where S:StorageRelation<I,T> {
    /// New, uninitialized constructor. If you are not implementing a custom texture format, you
    /// should probably use `new` instead.
    pub fn new_uninitialized<X:Into<StorageOf<S,I,T>>>(context:&Context, storage:X) -> Self {
        let storage    = storage.into();
        let context    = context.clone();
        let gl_texture = context.create_texture().unwrap();
        Self {storage,gl_texture,context}
    }

    /// Sets the texture wrapping parameters.
    pub fn set_texture_parameters(context:&Context) {
        let target = Context::TEXTURE_2D;
        let wrap   = Context::CLAMP_TO_EDGE as i32;
        context.tex_parameteri(target,Context::TEXTURE_MIN_FILTER,Context::LINEAR as i32);
        context.tex_parameteri(target,Context::TEXTURE_WRAP_S,wrap);
        context.tex_parameteri(target,Context::TEXTURE_WRAP_T,wrap);
    }
}

impl<S,I,T> Texture<S,I,T>
where S : StorageRelation<I,T>,
      I : InternalFormat,
      T : ItemType + JsBufferViewArr {
    /// Reloads gpu texture with data from given slice.
    pub fn reload_from_memory(&self, data:&[T], width:i32, height:i32) {
        let target          = Context::TEXTURE_2D;
        let level           = 0;
        let border          = 0;
        let internal_format = Self::gl_internal_format();
        let format          = Self::gl_format().into();
        let elem_type       = Self::gl_elem_type();

        self.context.bind_texture(target,Some(&self.gl_texture));
        #[allow(unsafe_code)]
        unsafe {
            // We use unsafe array view which is used immediately, so no allocations should happen
            // until we drop the view.
            let view = data.js_buffer_view();
            let result = self.context
                .tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_array_buffer_view
                (target,level,internal_format,width,height,border,format,elem_type,Some(&view));
            result.unwrap();
        }

        Self::set_texture_parameters(&self.context);
    }
}


// === Instances ===

impl<S:StorageRelation<I,T>,I,T>
HasContent for Texture<S,I,T> {
    type Content = Texture<S,I,T>;
}

impl<S:StorageRelation<I,T>,I,T>
ContentRef for Texture<S,I,T> {
    fn content(&self) -> &Self::Content {
        self
    }
}

//impl<S:StorageRelation<I,T>,I,T>
//WithContent2 for Texture<S,I,T> {
//    type Content = Texture<S,I,T>;
//    fn with_content<F:FnOnce(&Self::Content)->R,R>(&self, f:F) -> R {
//        f(self)
//    }
//}



// ==================
// === TextureOps ===
// ==================

/// API of the texture. It is defined as trait and uses the `WithContent` mechanism in order for
/// uniforms to easily redirect the methods.
pub trait TextureOps {
    /// Bind texture to a specific unit.
    fn bind_texture_unit(&self, context:&Context, unit:TextureUnit) -> TextureBindGuard;

    /// Accessor.
    fn gl_texture(&self) -> WebGlTexture;

    /// Accessor.
    fn get_format(&self) -> AnyFormat;

    /// Accessor.
    fn get_item_type(&self) -> AnyItemType;
}

impl<P:WithContent<Content=Texture<S,I,T>>,S:StorageRelation<I,T>,I:InternalFormat,T:ItemType>
TextureOps for P {
    fn bind_texture_unit(&self, context:&Context, unit:TextureUnit) -> TextureBindGuard {
        self.with_content(|this| {
            let context = context.clone();
            let target  = Context::TEXTURE_2D;
            context.active_texture(Context::TEXTURE0 + unit.to::<u32>());
            context.bind_texture(target,Some(&this.gl_texture));
            context.active_texture(Context::TEXTURE0);
            TextureBindGuard {context,target,unit}
        })
    }

    fn gl_texture(&self) -> WebGlTexture {
        self.with_content(|this| { this.gl_texture.clone() })
    }

    fn get_format(&self) -> AnyFormat {
        self.with_content(|_| { <Texture<S,I,T>>::format() })
    }

    fn get_item_type(&self) -> AnyItemType {
        self.with_content(|_| { <Texture<S,I,T>>::item_type() })
    }
}
