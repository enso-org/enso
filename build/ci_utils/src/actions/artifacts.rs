use crate::prelude::*;

use crate::actions::artifacts::download::FileToDownload;
use crate::actions::artifacts::run_session::SessionClient;
use crate::actions::artifacts::upload::ArtifactUploader;
use crate::actions::artifacts::upload::FileToUpload;
use crate::actions::artifacts::upload::UploadOptions;

use anyhow::Context as Trait_anyhow_Context;
use flume::Sender;
use tempfile::tempdir;


// ==============
// === Export ===
// ==============

pub mod context;
pub mod download;
pub mod models;
pub mod raw;
pub mod run_session;
pub mod upload;



pub const API_VERSION: &str = "6.0-preview";


pub async fn execute_dbg<T: DeserializeOwned + Debug>(
    client: &reqwest::Client,
    reqeust: reqwest::RequestBuilder,
) -> Result<T> {
    let request = reqeust.build()?;
    dbg!(&request);
    let response = client.execute(request).await?;
    dbg!(&response);
    let text = response.text().await?;
    debug!("{}", &text);
    let deserialized = serde_json::from_str(&text)?;
    dbg!(&deserialized);
    Ok(deserialized)
}

pub fn discover_and_feed(root_path: impl AsRef<Path>, sender: Sender<FileToUpload>) -> Result {
    walkdir::WalkDir::new(&root_path).into_iter().try_for_each(|entry| {
        let entry = entry?;
        if entry.file_type().is_file() {
            let file = FileToUpload::new_relative(&root_path, entry.path())?;
            sender
                .send(file)
                .context("Stopping discovery in progress, because all listeners were dropped.")?;
        };
        Ok(())
    })
}

pub fn discover_recursive(
    root_path: impl Into<PathBuf>,
) -> impl Stream<Item = FileToUpload> + Send {
    let root_path = root_path.into();

    let (tx, rx) = flume::unbounded();
    tokio::task::spawn_blocking(move || discover_and_feed(root_path, tx));
    rx.into_stream()
}


pub fn upload(
    file_provider: impl Stream<Item = FileToUpload> + Send + 'static,
    artifact_name: impl Into<String>,
    options: UploadOptions,
) -> BoxFuture<'static, Result> {
    let artifact_name = artifact_name.into();
    let span = info_span!("Artifact upload", artifact_name);
    async move {
        let handler = ArtifactUploader::new(SessionClient::new_from_env()?, artifact_name).await?;
        let result = handler.upload_artifact_to_file_container(file_provider, &options).await;
        // We want to patch size even if there were some failures.
        handler.patch_artifact_size().await?;
        result
    }
    .instrument(span)
    .boxed()
}

pub fn upload_single_file(
    file: impl Into<PathBuf>,
    artifact_name: impl Into<String>,
) -> BoxFuture<'static, Result> {
    let file = file.into();
    let artifact_name = artifact_name.into();
    info!("Uploading file {} as artifact {artifact_name}.", file.display());
    single_file_provider(file)
        .and_then_async(move |stream| upload(stream, artifact_name, default()))
        .boxed()
}

pub async fn upload_directory_if_exists(
    dir: impl Into<PathBuf>,
    artifact_name: impl Into<String>,
) -> Result {
    let dir = dir.into();
    let artifact_name = artifact_name.into();
    info!("Uploading directory {} as artifact {artifact_name}.", dir.display());
    upload(single_dir_provider(&dir)?, artifact_name, default()).await
}

#[tracing::instrument(skip_all , fields(artifact_name = %artifact_name.as_ref(), target = %target.as_ref().display()), err)]
pub async fn download_single_file_artifact(
    artifact_name: impl AsRef<str>,
    target: impl AsRef<Path>,
) -> Result {
    let downloader =
        download::ArtifactDownloader::new(SessionClient::new_from_env()?, artifact_name.as_ref())
            .await?;
    match downloader.file_items().collect_vec().as_slice() {
        [item] => {
            let file = FileToDownload {
                target:                 target.as_ref().into(),
                remote_source_location: item.content_location.clone(),
            };
            downloader.download_file_item(&file).await?;
        }
        _ => bail!("The artifact {} does not contain only a single file.", artifact_name.as_ref()),
    };
    Ok(())
}

pub fn single_file_provider(
    path: impl Into<PathBuf>,
) -> Result<impl Stream<Item = FileToUpload> + 'static> {
    let file = FileToUpload::new_in_root(path)?;
    Ok(futures::stream::iter([file]))
}

/// Scan recursively directory with its subtree for files to upload to the artifact.
///
/// The directory name will be preserved in the artifact container.
///
/// # Example
/// ```
/// # use ide_ci::prelude::*;
/// # use tempfile::TempDir;
/// # use ide_ci::actions::artifacts::single_dir_provider;
/// # use std::path::Path;
///
/// # #[tokio::main]
/// # async fn main() -> Result {
/// //  temp_dir/
/// // ├── sibling/
/// // │   └── file
/// // └── uploaded_dir/
/// //     ├── file
/// //     └── subdir/
/// //         └── nested_file
/// let dir = TempDir::new()?;
/// let sibling = dir.path().join("sibling");
/// let file_in_sibling = sibling.join("file");
/// let uploaded = dir.path().join("uploaded_dir");
/// let file1 = uploaded.join("file");
/// let file2 = uploaded.join_iter(["subdir/nested_file"]);
/// ide_ci::fs::create_dir_all(&uploaded)?;
/// ide_ci::fs::create(&file1)?;
/// ide_ci::fs::create(&file2)?;
/// ide_ci::fs::create_dir_all(&sibling)?;
/// ide_ci::fs::create(&file_in_sibling)?;
///
/// let stream = single_dir_provider(&uploaded)?;
/// let mut found_files = stream.collect::<Vec<_>>().await;
/// // Make discovery order irrelevant.
/// found_files.sort_by(|a, b| a.local_path.cmp(&b.local_path));
///
/// assert_eq!(found_files.len(), 2);
/// assert_eq!(found_files[0].local_path, file1);
/// assert_eq!(found_files[0].remote_path, Path::new("uploaded_dir/file"));
/// assert_eq!(found_files[1].local_path, file2);
/// assert_eq!(found_files[1].remote_path, Path::new("uploaded_dir/subdir/nested_file"));
/// // Note that sibling directory has not been included.
/// # Ok(())
/// # }
/// ```
pub fn single_dir_provider(path: &Path) -> Result<impl Stream<Item = FileToUpload> + 'static> {
    // TODO not optimal, could discover files at the same time as handling them.
    let parent_path = path.try_parent()?;
    let files: Vec<FileToUpload> = walkdir::WalkDir::new(path)
        .into_iter()
        .filter(|res| match res {
            Ok(entry) => !entry.file_type().is_dir(),
            // ignore "not found" errors, just don't include the file in the final stream.
            Err(e) =>
                e.depth() != 0
                    || e.io_error().map_or(true, |e| e.kind() != std::io::ErrorKind::NotFound),
        })
        .map(|entry| FileToUpload::new_relative(parent_path, entry?.path()))
        .try_collect()?;
    info!("Discovered {} files under the {}.", files.len(), path.display());
    Ok(futures::stream::iter(files))
}

#[tracing::instrument(skip_all , fields(path = %path_to_upload.as_ref().display(), artifact = artifact_name.as_ref()), err)]
pub async fn upload_compressed_directory(
    path_to_upload: impl AsRef<Path> + Send,
    artifact_name: impl AsRef<str> + Send,
) -> Result {
    let artifact_name = artifact_name.as_ref();
    let tempdir = tempdir()?;
    let archive_path = tempdir.path().join(format!("{artifact_name}.tar.gz"));

    info!("Packing {} to {}", path_to_upload.as_ref().display(), archive_path.display());
    crate::archive::compress_directory_contents(&archive_path, path_to_upload).await?;

    info!("Starting upload of {artifact_name}.");
    upload_single_file(&archive_path, artifact_name).await?;
    info!("Completed upload of {artifact_name}.");
    Ok(())
}

#[tracing::instrument(skip_all , fields(path = %path_to_extract.as_ref().display(), artifact = artifact_name.as_ref()), err)]
pub async fn retrieve_compressed_directory(
    artifact_name: impl AsRef<str> + Send,
    path_to_extract: impl AsRef<Path> + Send,
) -> Result {
    let artifact_name = artifact_name.as_ref();
    let tempdir = tempdir()?;
    let archive_path = tempdir.path().join(format!("{artifact_name}.tar.gz"));

    download_single_file_artifact(&artifact_name, &archive_path).await?;
    crate::archive::extract_to(&archive_path, &path_to_extract).await?;
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::artifacts::models::CreateArtifactResponse;
    use reqwest::StatusCode;
    use wiremock::matchers::method;
    use wiremock::Mock;
    use wiremock::MockServer;
    use wiremock::ResponseTemplate;

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    #[ignore]
    async fn test_artifact_upload() -> Result {
        let mock_server = MockServer::start().await;

        let text = r#"{"containerId":11099678,"size":-1,"signedContent":null,"fileContainerResourceUrl":"https://pipelines.actions.githubusercontent.com/VYS7uSE1JB12MkavBOHvD6nounefzg1s5vHmQvfbiLmuvFuM6c/_apis/resources/Containers/11099678","type":"actions_storage","name":"SomeFile","url":"https://pipelines.actions.githubusercontent.com/VYS7uSE1JB12MkavBOHvD6nounefzg1s5vHmQvfbiLmuvFuM6c/_apis/pipelines/1/runs/75/artifacts?artifactName=SomeFile","expiresOn":"2022-01-29T04:07:24.5807079Z","items":null}"#;
        mock_server
            .register(
                Mock::given(method("POST"))
                    .respond_with(ResponseTemplate::new(StatusCode::CREATED).set_body_string(text)),
            )
            .await;

        mock_server
            .register(
                Mock::given(method("PUT"))
                    .respond_with(ResponseTemplate::new(StatusCode::NOT_FOUND)),
            )
            .await;

        crate::env::set_var("ACTIONS_RUNTIME_URL", mock_server.uri());
        crate::env::set_var("ACTIONS_RUNTIME_TOKEN", "password123");
        crate::env::set_var("GITHUB_RUN_ID", "12");

        let path_to_upload = "Cargo.toml";

        let file_to_upload = FileToUpload {
            local_path:  PathBuf::from(path_to_upload),
            remote_path: PathBuf::from(path_to_upload),
        };

        upload(futures::stream::once(ready(file_to_upload)), "MyCargoArtifact", default()).await?;
        // artifacts::upload_path(path_to_upload).await?;
        Ok(())
        //let client = reqwest::Client::builder().default_headers().
    }

    #[test]
    fn deserialize_response() -> Result {
        let text = r#"{"containerId":11099678,"size":-1,"signedContent":null,"fileContainerResourceUrl":"https://pipelines.actions.githubusercontent.com/VYS7uSE1JB12MkavBOHvD6nounefzg1s5vHmQvfbiLmuvFuM6c/_apis/resources/Containers/11099678","type":"actions_storage","name":"SomeFile","url":"https://pipelines.actions.githubusercontent.com/VYS7uSE1JB12MkavBOHvD6nounefzg1s5vHmQvfbiLmuvFuM6c/_apis/pipelines/1/runs/75/artifacts?artifactName=SomeFile","expiresOn":"2022-01-29T04:07:24.5807079Z","items":null}"#;
        let response = serde_json::from_str::<CreateArtifactResponse>(text)?;
        //
        // let patch_request = client.patch(artifact_url.clone())
        //     .query(&[("artifactName", artifact_name)])
        //     .header(reqwest::header::CONTENT_TYPE, "application/json")
        //     .json(&PatchArtifactSize {size: file.len()});

        let path = PathBuf::from("Cargo.toml");
        let artifact_path = path.file_name().unwrap(); // FIXME

        let client = reqwest::ClientBuilder::new().build()?;
        dbg!(artifact_path);
        client
            .patch(response.url)
            .query(&[("itemPath", artifact_path.to_str().unwrap())])
            .build()?;

        Ok(())
    }
}
