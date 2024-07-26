use crate::prelude::*;

use mime::Mime;
use reqwest::header::HeaderMap;
use reqwest::header::HeaderName;
use reqwest::header::HeaderValue;
use reqwest::header::CONNECTION;
use reqwest::header::CONTENT_TYPE;



pub trait ClientBuilderExt: Sized {
    fn default_content_type(self, mime_type: Mime) -> Self;
    fn keep_alive(self, seconds: usize) -> Self;
    fn default_header(self, name: impl Into<HeaderName>, value: impl Into<HeaderValue>) -> Self;
}

impl ClientBuilderExt for reqwest::ClientBuilder {
    fn default_content_type(self, mime_type: Mime) -> Self {
        // We can safely unwrap, because we know that all mime types are in format that can be used
        // as HTTP header value.
        self.default_header(CONTENT_TYPE, HeaderValue::try_from(mime_type.as_ref()).unwrap())
    }

    fn keep_alive(self, seconds: usize) -> Self {
        let mut header = HeaderMap::new();
        // We can safely unwrap, because we know that all mime types are in format that can be used
        // as HTTP header value.
        header.insert(CONNECTION, HeaderValue::from_static("Keep-Alive"));
        header.insert(HeaderName::from_static("keep-alive"), HeaderValue::from(seconds));
        self.default_headers(header)
    }

    fn default_header(self, name: impl Into<HeaderName>, value: impl Into<HeaderValue>) -> Self {
        self.default_headers(HeaderMap::from_iter([(name.into(), value.into())]))
    }
}
