use crate::prelude::*;

use crate::HtmlCanvasElement;
use crate::Result;

/// An abstraction around CanvasRenderingContext2d.
#[derive(Clone, Debug)]
pub struct Canvas2d {
    inner:   HtmlCanvasElement,
    #[cfg(target_arch = "wasm32")]
    context: web_sys::CanvasRenderingContext2d,
}

pub trait Canvas2dExt {
    fn new(element: HtmlCanvasElement) -> Self;
    fn inner(&self) -> &HtmlCanvasElement;
    fn set_width(&self, value: u32) {}
    fn set_height(&self, value: u32) {}
    fn fill_rect(&self, x: f64, y: f64, w: f64, h: f64) {}
    fn set_fill_style(&self, value: &JsValue) {}
    fn draw_image(
        &self,
        image: &HtmlCanvasElement,
        sx: f64,
        sy: f64,
        sw: f64,
        sh: f64,
        dx: f64,
        dy: f64,
        dw: f64,
        dh: f64,
    ) -> Result<()> {
        Ok(())
    }
    fn translate(&self, x: f64, y: f64) -> Result<()> {
        Ok(())
    }
    fn set_font(&self, value: &str) {}
    fn set_text_align(&self, value: &str) {}
    fn fill_text(&self, text: &str, x: f64, y: f64) -> Result<()> {
        Ok(())
    }
    fn clear_rect(&self, x: f64, y: f64, w: f64, h: f64) {}
    fn scale(&self, x: f64, y: f64) -> Result<()> {
        Ok(())
    }
    fn width(&self) -> u32 {
        0
    }
    fn height(&self) -> u32 {
        0
    }
    fn set_line_width(&self, value: f64) {}
    fn move_to(&self, x: f64, y: f64) {}
    fn line_to(&self, x: f64, y: f64) {}
    fn stroke(&self) {}
    fn save(&self) {}
    fn restore(&self) {}
    fn set_stroke_style(&self, value: &JsValue) {}
    fn begin_path(&self) {}
}

impl Deref for Canvas2d {
    type Target = HtmlCanvasElement;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Canvas2dExt for Canvas2d {
    fn new(element: HtmlCanvasElement) -> Self {
        Self { inner: element }
    }
    fn inner(&self) -> &HtmlCanvasElement {
        &self.inner
    }
}

#[cfg(feature = "wasm32")]
impl Canvas2dExt for Canvas2d {
    fn new(element: HtmlCanvasElement) -> Self {
        let context = element.get_context("2d").unwrap().unwrap();
        let context: web_sys::CanvasRenderingContext2d = context.dyn_into().unwrap();
        Self { inner: element, context }
    }

    fn inner(&self) -> &HtmlCanvasElement {
        &self.inner
    }

    fn set_width(&self, value: u32) {
        self.inner.set_width(value);
    }

    fn set_height(&self, value: u32) {
        self.inner.set_height(value);
    }

    fn fill_rect(&self, x: f64, y: f64, w: f64, h: f64) {
        self.context.fill_rect(x, y, w, h);
    }

    fn set_fill_style(&self, value: &JsValue) {
        self.context.set_fill_style(value);
    }

    fn translate(&self, x: f64, y: f64) -> Result<()> {
        self.context.translate(x, y)?;
        Ok(())
    }

    fn draw_image(
        &self,
        image: &HtmlCanvasElement,
        sx: f64,
        sy: f64,
        sw: f64,
        sh: f64,
        dx: f64,
        dy: f64,
        dw: f64,
        dh: f64,
    ) -> Result<()> {
        self.context
            .draw_image_with_html_canvas_element_and_sw_and_sh_and_dx_and_dy_and_dw_and_dh(
                image, sx, sy, sw, sh, dx, dy, dw, dh,
            )?;
        Ok(())
    }

    fn set_font(&self, value: &str) {
        self.context.set_font(value);
    }

    fn set_text_align(&self, value: &str) {
        self.context.set_text_align(value);
    }

    fn fill_text(&self, text: &str, x: f64, y: f64) -> Result<()> {
        self.context.fill_text(text, x, y)?;
        Ok(())
    }
    fn clear_rect(&self, x: f64, y: f64, w: f64, h: f64) {
        self.context.clear_rect(x, y, w, h);
    }

    fn scale(&self, x: f64, y: f64) -> Result<()> {
        self.context.scale(x, y)
    }

    fn width(&self) -> u32 {
        self.inner.width()
    }

    fn height(&self) -> u32 {
        self.inner.height()
    }
    fn set_line_width(&self, value: f64) {
        self.context.set_line_width(value);
    }
    fn move_to(&self, x: f64, y: f64) {
        self.context.move_to(x, y);
    }
    fn line_to(&self, x: f64, y: f64) {
        self.context.line_to(x, y);
    }
    fn stroke(&self) {
        self.context.stroke();
    }
    fn save(&self) {
        self.context.save();
    }
    fn restore(&self) {
        self.context.restore();
    }
    fn set_stroke_style(&self, value: &JsValue) {
        self.context.set_stroke_style(value);
    }
    fn begin_path(&self) {
        self.context.begin_path();
    }
}
