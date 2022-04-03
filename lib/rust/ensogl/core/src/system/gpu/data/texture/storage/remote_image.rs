//! This module defines remote image texture storage. It is used to download image from a given URL.

use crate::system::gpu::data::texture::class::*;
use crate::system::gpu::data::texture::storage::*;
use crate::system::gpu::data::texture::types::*;

use crate::system::gpu::Context;
#[cfg(target_arch = "wasm32")]
use crate::system::web;

#[cfg(target_arch = "wasm32")]
use web::Closure;
#[cfg(target_arch = "wasm32")]
use web_sys::HtmlImageElement;



// ===================
// === RemoteImage ===
// ===================

/// Texture downloaded from URL. This source implies asynchronous loading.
#[derive(Debug)]
pub struct RemoteImageData {
    /// An url from where the texture is downloaded.
    pub url: String,
}


// === Instances ===

impl<I, T> StorageRelation<I, T> for RemoteImage {
    type Storage = RemoteImageData;
}

impl<S: Str> From<S> for RemoteImageData {
    fn from(s: S) -> Self {
        Self::new(s)
    }
}


// === API ===

impl RemoteImageData {
    fn new<S: Str>(url: S) -> Self {
        Self { url: url.into() }
    }
}

impl<I: InternalFormat, T: ItemType> Texture<RemoteImage, I, T> {
    /// Initializes default texture value. It is useful when the texture data needs to be downloaded
    /// asynchronously. This method creates a mock 1px x 1px texture and uses it as a mock texture
    /// until the download is complete.
    pub fn init_mock(&self) {
        let target = Context::TEXTURE_2D;
        let level = 0;
        let internal_format = Self::gl_internal_format();
        let format = Self::gl_format().into();
        let elem_type = Self::gl_elem_type();
        let width = 1;
        let height = 1;
        let border = 0;
        let color = vec![0, 0, 255, 255];
        self.context().bind_texture(*Context::TEXTURE_2D, Some(self.gl_texture()));
        self.context()
            .tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                *target,
                level,
                internal_format,
                width,
                height,
                border,
                format,
                elem_type,
                Some(&color),
            )
            .unwrap();
    }
}

impl<I: InternalFormat, T: ItemType> TextureReload for Texture<RemoteImage, I, T> {
    /// Loads or re-loads the texture data from the provided url.
    /// This action will be performed asynchronously.
    #[allow(trivial_casts)]
    #[cfg(target_arch = "wasm32")]
    fn reload(&self) {
        let url = &self.storage().url;
        let image = HtmlImageElement::new().unwrap();
        let no_callback = <Option<web::EventListenerHandle>>::None;
        let callback_ref = Rc::new(RefCell::new(no_callback));
        let image_ref = Rc::new(RefCell::new(image));
        let callback_ref2 = callback_ref.clone();
        let image_ref_opt = image_ref.clone();
        let context = self.context().clone();
        let gl_texture = self.gl_texture().clone();
        let parameters = *self.parameters();
        let callback: web::JsEventHandler = Closure::once(move |_| {
            let _keep_alive = callback_ref2;
            let image = image_ref_opt.borrow();
            let target = Context::TEXTURE_2D;
            let level = 0;
            let internal_format = Self::gl_internal_format();
            let format = Self::gl_format().into();
            let elem_type = Self::gl_elem_type();
            context.bind_texture(*target, Some(&gl_texture));
            context
                .tex_image_2d_with_u32_and_u32_and_html_image_element(
                    *target,
                    level,
                    internal_format,
                    format,
                    elem_type,
                    &image,
                )
                .unwrap();

            parameters.apply_parameters(&context);
        }) as web::JsEventHandler;
        let image = image_ref.borrow();
        request_cors_if_not_same_origin(&image, url);
        image.set_src(url);
        let handler = web::add_event_listener_with_bool(&image, "load", callback, true);
        *callback_ref.borrow_mut() = Some(handler);
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn reload(&self) {}
}

// === Utils ===

/// CORS = Cross Origin Resource Sharing. It's a way for the webpage to ask the image server for
/// permission to use the image. To do this we set the crossOrigin attribute to something and then
/// when the browser tries to get the image from the server, if it's not the same domain, the
/// browser will ask for CORS permission. The string we set `cross_origin` to is sent to the server.
/// The server can look at that string and decide whether or not to give you permission. Most
/// servers that support CORS don't look at the string, they just give permission to everyone.
///
/// **Note**
/// Why don't want to just always see the permission because asking for permission takes 2 HTTP
/// requests, so it's slower than not asking. If we know we're on the same domain or we know we
/// won't use the image for anything except img tags and or canvas2d then we don't want to set
/// crossDomain because it will make things slower.
#[cfg(target_arch = "wasm32")]
fn request_cors_if_not_same_origin(img: &HtmlImageElement, url_str: &str) {
    let url = web_sys::Url::new(url_str).unwrap();
    let origin = web::window.location().origin().unwrap();
    if url.origin() != origin {
        img.set_cross_origin(Some(""));
    }
}
