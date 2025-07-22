use crate::prelude::*;

use crate::aws::s3::BucketContext;

use aws_config::meta::region::RegionProviderChain;



/// AWS Region of the `ensocdn` bucket.
pub const BUCKET_REGION: &str = "us-west-1";

/// The bucket where the GUI releases are stored.
pub const BUCKET: &str = "ensocdn";

/// As default but with the region resolution fallback.
///
/// We do know the region, so we should not require it. Still, it is allowed to overwrite it through
/// the environment.
pub async fn client_from_env() -> Result<aws_sdk_s3::Client> {
    let region = RegionProviderChain::default_provider().or_else(BUCKET_REGION);
    let config = aws_config::from_env().region(region).load().await;
    let client = aws_sdk_s3::Client::new(&config);
    Ok(client)
}

/// Construct a context for handling a given GUI version release.
///
/// Requires AWS credentials in the environment.
pub async fn context(version: &Version) -> Result<BucketContext> {
    Ok(BucketContext {
        client:     client_from_env().await?,
        bucket:     BUCKET.to_string(),
        upload_acl: aws_sdk_s3::model::ObjectCannedAcl::PublicRead,
        key_prefix: Some(format!("ide/{version}")),
    })
}
