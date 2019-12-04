use crate::{api, api::IsParser};
use prelude::*;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "JS parser client has not been yet implemented!")]
    NotImplemented,
}

/// Wrapper over the JS-compiled parser.
///
/// Can only be used when targeting WebAssembly. Not yet implemented.
pub struct Client {}

impl Client {
    // avoid warnings when compiling natively and having this usage cfg-ed out
    #[cfg(not(target_arch = "wasm32"))]
    #[allow(dead_code)]
    pub fn new() -> Result<Client> {
        Err(Error::NotImplemented)
    }
}

impl IsParser for Client {
    fn parse(&mut self, _program: String) -> api::Result<api::Ast> {
        Err(api::interop_error(Error::NotImplemented))
    }
}
