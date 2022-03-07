#[warn(missing_docs)]
pub mod dom;
use std::any::TypeId;
use web::HtmlElement;
a
b
pub trait MouseTarget: Debug + 'static {
    fn mouse_down(&self) -> &frp::Source;
    fn mouse_up(&self) -> &frp::Source;
    fn mouse_over(&self) -> &frp::Source;
    fn mouse_out(&self) -> &frp::Source;
}
