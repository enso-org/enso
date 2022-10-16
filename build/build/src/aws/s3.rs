use crate::prelude::*;

use aws_sdk_s3::model::ObjectCannedAcl;
use aws_sdk_s3::output::PutObjectOutput;
use aws_sdk_s3::types::ByteStream;
use bytes::Buf;


// ==============
// === Export ===
// ==============

pub mod gui;



pub async fn client_from_env() -> aws_sdk_s3::Client {
    aws_sdk_s3::Client::new(&aws_config::load_from_env().await)
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BucketContext {
    #[derivative(Debug = "ignore")]
    pub client:     aws_sdk_s3::Client,
    pub bucket:     String,
    pub upload_acl: ObjectCannedAcl,
    pub key_prefix: String,
}

impl BucketContext {}

impl BucketContext {
    pub async fn get(&self, path: &str) -> Result<ByteStream> {
        let key = format!("{}/{}", self.key_prefix, path);
        Ok(self
            .client
            .get_object()
            .bucket(&self.bucket)
            .key(format!("{}/{}", self.key_prefix, path))
            .send()
            .await
            .with_context(|| format!("Failed to download {} from S3 bucket {}.", key, self.bucket))?
            .body)
    }

    pub async fn put(&self, path: &str, data: ByteStream) -> Result<PutObjectOutput> {
        trace!("Uploading {path} at {self:?}.");
        let key = format!("{}/{}", self.key_prefix, path);
        self.client
            .put_object()
            .bucket(&self.bucket)
            .acl(self.upload_acl.clone())
            .key(&key)
            .body(data)
            .send()
            .await
            .with_context(|| format!("Failed to upload {} to S3 bucket {}.", key, self.bucket))
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
