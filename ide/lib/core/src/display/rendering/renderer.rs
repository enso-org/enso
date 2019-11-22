// use super::Camera;
use crate::prelude::*;

/// Base structure for our Renderers.
#[derive(Default, Debug)]
pub struct Renderer {}

impl Renderer {
    pub fn new() -> Self { default() }
}
