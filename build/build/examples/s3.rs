use enso_build::prelude::*;

use aws_sdk_s3::model::ObjectCannedAcl;
use aws_sdk_s3::types::ByteStream;
use aws_sdk_s3::Client;
use enso_build::aws::BucketContext;
use enso_build::aws::EDITIONS_BUCKET_NAME;



#[tokio::main]
async fn main() -> Result {
    let config = dbg!(aws_config::load_from_env().await);
    let bucket_context = BucketContext {
        client:     Client::new(&config),
        bucket:     EDITIONS_BUCKET_NAME.to_string(),
        upload_acl: ObjectCannedAcl::PublicRead,
        key_prefix: "enso".into(),
    };

    // std::env::set_var("AWS_SECRET_ACCESS_KEY", std::env::var("AWS_SECRET_ACCESS_KEY")?.trim());

    let test_file = "test_file.exe";
    dbg!(
        bucket_context
            .put(test_file, ByteStream::from_path(&std::env::current_exe()?).await?)
            .await?
    );

    Ok(())
}
