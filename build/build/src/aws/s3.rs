//! Code supporting our S3 operations.

use crate::prelude::*;

use aws_sdk_s3::model::ObjectCannedAcl;
use aws_sdk_s3::output::PutObjectOutput;
use aws_sdk_s3::types::ByteStream;
use bytes::Buf;
use enso_build_base::extensions::path::SplitFilename;
use mime::Mime;


// ==============
// === Export ===
// ==============

pub mod gui;



/// Construct client from the environment.
pub async fn client_from_env() -> aws_sdk_s3::Client {
    aws_sdk_s3::Client::new(&aws_config::load_from_env().await)
}

/// Everything we need to get/put files to S3.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BucketContext {
    #[derivative(Debug = "ignore")]
    pub client:     aws_sdk_s3::Client,
    pub bucket:     String,
    pub upload_acl: ObjectCannedAcl,
    /// Prefix that will be prepended to the object key.
    pub key_prefix: Option<String>,
}

impl BucketContext {
    pub fn key(&self, path: impl AsRef<Path>) -> String {
        let path = path.as_ref();
        let normalized = path_slash::PathExt::to_slash_lossy(path);
        if let Some(prefix) = &self.key_prefix {
            format!("{}/{}", prefix, normalized)
        } else {
            normalized.into()
        }
    }

    pub async fn get(&self, path: &str) -> Result<ByteStream> {
        trace!("Downloading {path} at {self:?}.");
        Ok(self
            .client
            .get_object()
            .bucket(&self.bucket)
            .key(self.key(path))
            .send()
            .await
            .with_context(|| {
                format!("Failed to download {} from S3 bucket {}.", self.key(path), self.bucket)
            })?
            .body)
    }

    pub async fn put(&self, path: &str, data: ByteStream) -> Result<PutObjectOutput> {
        trace!("Uploading {path} at {self:?}.");
        let mut request = self
            .client
            .put_object()
            .bucket(&self.bucket)
            .acl(self.upload_acl.clone())
            .key(self.key(path))
            .body(data);

        // Cloud requested us to set content encoding and type.
        let content_headers = ContentHeaders::from_path(path);
        request = content_headers.apply(request);

        request.send().await.with_context(|| {
            format!("Failed to upload {} to S3 bucket {}.", self.key(path), self.bucket)
        })
    }

    #[instrument(fields(path = %path.as_ref().display()))]
    pub async fn put_file(&self, path: impl AsRef<Path>) -> Result<PutObjectOutput> {
        let path = path.as_ref();
        let stream = ByteStream::from_path(path).await?;
        let path = path.file_name().with_context(|| format!("Path {:?} has no file name", path))?;
        self.put(path.as_str(), stream).await
    }

    pub async fn get_yaml<T: DeserializeOwned>(&self, path: &str) -> Result<T> {
        let text = self.get(path).await?.collect().await?;
        serde_yaml::from_reader(text.reader()).anyhow_err()
    }

    pub async fn put_yaml(&self, path: &str, data: &impl Serialize) -> Result<PutObjectOutput> {
        let buf = serde_yaml::to_string(data)?;
        self.put(path, ByteStream::from(buf.into_bytes())).await
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ContentEncoding {
    Gzip,
}

impl ContentEncoding {
    pub fn from_ext(ext: &str) -> Result<Self> {
        match ext {
            "gz" => Ok(Self::Gzip),
            _ => bail!("Cannot recognize content encoding from extension: {}", ext),
        }
    }
}

/// Describe additional content-related headers that we might want to set.
#[derive(Clone, Debug)]
pub struct ContentHeaders {
    /// Encoding of the content. Typically compression, if any.
    pub content_encoding: Option<ContentEncoding>,
    /// MIME type of the content.
    pub content_type:     Mime,
}

impl Default for ContentHeaders {
    fn default() -> Self {
        Self { content_encoding: None, content_type: mime::APPLICATION_OCTET_STREAM }
    }
}

impl ContentHeaders {
    pub fn new(content_type: Mime) -> Self {
        Self { content_type, ..default() }
    }

    pub fn content_encoding(&self) -> Option<&'static str> {
        self.content_encoding.as_ref().map(|enc| match enc {
            ContentEncoding::Gzip => "gzip",
        })
    }

    pub fn from_path(path: impl AsRef<Path>) -> Self {
        let Ok(SplitFilename{ extension: outermost_extension, stem}) = path.split_filename() else {
            // No extension, use defaults.
            return default()
        };

        let Ok(next_extension) = stem.try_extension() else {
            // Only one extension, use primary MIME.
            let content_type = new_mime_guess::from_ext(outermost_extension.as_str()).first_or_octet_stream();
            return Self::new(content_type)
        };

        if let Ok(content_encoding) = ContentEncoding::from_ext(outermost_extension.as_str()) {
            // Two extensions, use primary MIME and encoding.
            let content_type =
                new_mime_guess::from_ext(next_extension.as_str()).first_or_octet_stream();
            Self { content_encoding: Some(content_encoding), content_type }
        } else {
            // No encoding, use primary MIME.
            let content_type =
                new_mime_guess::from_ext(outermost_extension.as_str()).first_or_octet_stream();
            Self::new(content_type)
        }
    }

    pub fn apply(
        &self,
        mut request: aws_sdk_s3::client::fluent_builders::PutObject,
    ) -> aws_sdk_s3::client::fluent_builders::PutObject {
        if let Some(content_encoding) = self.content_encoding() {
            request = request.content_encoding(content_encoding);
        }
        request.content_type(&self.content_type.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deduce_content_headers() -> Result {
        fn case(path: &str, expected_encoding: Option<&str>, expected_type: &str) {
            let headers = ContentHeaders::from_path(path);
            assert_eq!(headers.content_encoding(), expected_encoding);
            assert_eq!(headers.content_type.to_string().as_str(), expected_type);
        }

        case("wasm_imports.js.gz", Some("gzip"), "application/javascript");
        case("index.js", None, "application/javascript");
        case("style.css", None, "text/css");
        case("ide.wasm", None, "application/wasm");
        case("ide.wasm.gz", Some("gzip"), "application/wasm");

        Ok(())
    }
}
