use crate::prelude::*;

use crate::actions::artifacts::models::PatchArtifactSizeResponse;
use crate::actions::artifacts::raw;
use crate::actions::artifacts::run_session::SessionClient;
use crate::global;

use anyhow::Context;
use reqwest::Client;
use std::sync::atomic::Ordering;



#[derive(Clone, Copy, Debug)]
pub struct UploadOptions {
    pub file_concurrency:  usize,
    pub chunk_size:        usize,
    // by default, file uploads will continue if there is an error unless specified differently in
    // the options
    pub continue_on_error: bool,
}

impl Default for UploadOptions {
    fn default() -> Self {
        UploadOptions {
            chunk_size:        8 * 1024 * 1024,
            file_concurrency:  10,
            continue_on_error: true,
        }
    }
}

#[derive(Debug)]
pub struct ArtifactUploader {
    pub client:        SessionClient,
    pub artifact_name: String,
    pub upload_url:    Url,
    pub total_size:    std::sync::atomic::AtomicUsize,
    pub cancel:        tokio_util::sync::CancellationToken,
}

impl ArtifactUploader {
    pub async fn new(client: SessionClient, artifact_name: impl Into<String>) -> Result<Self> {
        let artifact_name = artifact_name.into();
        let container = client.create_container(&artifact_name).await?;
        info!("Created a container {} for artifact '{}'.", container.container_id, artifact_name);
        Ok(Self {
            client,
            artifact_name,
            upload_url: container.file_container_resource_url,
            total_size: default(),
            cancel: default(),
        })
    }


    pub fn uploader(&self, options: &UploadOptions) -> FileUploader {
        FileUploader {
            url:           self.upload_url.clone(),
            client:        self.client.upload_client.clone(),
            artifact_name: PathBuf::from(&self.artifact_name),
            chunk_size:    options.chunk_size,
        }
    }

    /// Concurrently upload all of the files in chunks.
    pub async fn upload_artifact_to_file_container(
        &self,
        files_to_upload: impl Stream<Item = FileToUpload> + Send + 'static,
        options: &UploadOptions,
    ) -> Result {
        debug!(
            "File Concurrency: {}, and Chunk Size: {}.  URL: {}",
            options.file_concurrency, options.chunk_size, self.upload_url
        );

        let (work_tx, work_rx) = flume::unbounded();
        let (result_tx, result_rx) = flume::unbounded();

        tokio::task::spawn(async move {
            debug!("Spawned the file discovery worker.");
            files_to_upload
                .inspect(|f| debug!("File {} discovered for upload.", f.local_path.display()))
                .map(Ok)
                .forward(work_tx.into_sink())
                .await
                .unwrap();
            debug!("File discovery complete.");
        });

        for index in 0..options.file_concurrency {
            let span = debug_span!("Upload worker", index).entered();
            let worker_task = upload_worker(
                self.cancel.clone(),
                work_rx.clone(),
                self.uploader(options),
                result_tx.clone(),
            )
            .map(Result::Ok);
            debug!("Spawning the worker task.");
            global::spawn(format!("uploader {index}"), worker_task.instrument(span.exit()));
        }

        drop(result_tx);

        let results = result_rx.into_stream().collect::<Vec<_>>().await;
        let uploaded_size = results.iter().fold(0, |acc, r| acc + r.total_size);
        debug!("Uploaded in total {} bytes.", uploaded_size);
        self.total_size.fetch_add(uploaded_size, Ordering::SeqCst);
        let errors = results.into_iter().filter_map(|r| r.result.err()).collect_vec();
        if !errors.is_empty() {
            let mut error = anyhow!(
                "Not all file uploads were successful. Encountered {} errors: {:#?}",
                errors.len(),
                errors
            );
            for cause in errors {
                error = error.context(cause);
            }
            Err(error)
        } else {
            Ok(())
        }
    }

    pub async fn patch_artifact_size(&self) -> Result<PatchArtifactSizeResponse> {
        let total_size = self.total_size.load(Ordering::SeqCst);
        self.client.patch_artifact_size(&self.artifact_name, total_size).await
    }
}

pub async fn upload_worker(
    cancellation_token: tokio_util::sync::CancellationToken,
    job_receiver: flume::Receiver<FileToUpload>,
    uploader: FileUploader,
    result_sender: flume::Sender<UploadResult>,
) {
    debug!("Upload worker spawned.");
    let mut job_receiver = job_receiver.into_stream();
    loop {
        trace!("Waiting for input.");
        let mut on_cancelled = pin!(cancellation_token.cancelled().fuse());
        select! {
            _ = on_cancelled => {
                debug!("Upload worker has been cancelled.");
                break;
            },
            (job, tail) = job_receiver.into_future() => {
                job_receiver = tail;
                trace!("Got job: {job:?}.");
                match job {
                    Some(job) => {
                        let result = uploader.upload_file(&job).await;
                        result_sender.send(result).unwrap();
                    }
                    None => {
                        debug!("Upload worker completed all available work.");
                        break;
                    }
                }
                trace!("Job complete.");
            }
            complete => {
                trace!("Complete.");
                break;
            },
        }
    }
    debug!("Upload worker finished.");
}

#[derive_where(Debug)]
pub struct FileUploader {
    pub url:           Url,
    #[derive_where(skip)]
    pub client:        Client,
    pub artifact_name: PathBuf,
    pub chunk_size:    usize,
}

impl FileUploader {
    pub async fn upload_file(&self, file_to_upload: &FileToUpload) -> UploadResult {
        let uploading_res = raw::upload_file(
            &self.client,
            self.chunk_size,
            self.url.clone(),
            &file_to_upload.local_path,
            self.artifact_name.join(&file_to_upload.remote_path),
        )
        .await;
        match uploading_res {
            Ok(len) => UploadResult {
                result:                 Ok(()),
                total_size:             len,
                successful_upload_size: len,
            },
            Err(e) => UploadResult {
                result:                 Err(e),
                total_size:             0,
                successful_upload_size: 0,
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct FileToUpload {
    /// Absolute path in the local filesystem.
    pub local_path:  PathBuf,
    /// Relative path within the artifact container. Does not include the leading segment with the
    /// artifact name.
    pub remote_path: PathBuf,
}

impl FileToUpload {
    /// Create a new file to upload.
    ///
    /// The file will be uploaded directly to the root of the artifact container.
    ///
    /// # Example
    /// ```
    /// use ide_ci::actions::artifacts::upload::FileToUpload;
    /// use std::path::Path;
    ///
    /// let local_path = Path::new("/home/user/uploaded/file");
    /// let file = FileToUpload::new_in_root(local_path).unwrap();
    /// assert_eq!(file.local_path, local_path);
    /// assert_eq!(file.remote_path, Path::new("file"));
    /// ```
    pub fn new_in_root(path: impl Into<PathBuf>) -> Result<Self> {
        let local_path = path.into();
        let remote_path = local_path.file_name().map(into).ok_or_else(|| {
            anyhow!("Path {} does not contain a valid filename.", local_path.display())
        })?;
        Ok(Self { local_path, remote_path })
    }

    /// Create a new file to upload with a relative path.
    ///
    /// # Example
    /// ```
    /// use ide_ci::actions::artifacts::upload::FileToUpload;
    /// use std::path::Path;
    ///
    /// let root_path = Path::new("/home/user");
    /// let local_path = Path::new("/home/user/uploaded/subdir/file");
    /// let file = FileToUpload::new_relative(&root_path, &local_path).unwrap();
    /// assert_eq!(file.local_path, local_path);
    /// assert_eq!(file.remote_path, Path::new("uploaded/subdir/file"));
    /// ```
    pub fn new_relative(
        root_path: impl AsRef<Path>,
        local_path: impl Into<PathBuf>,
    ) -> Result<Self> {
        let local_path = local_path.into();
        Ok(FileToUpload {
            remote_path: local_path
                .strip_prefix(&root_path)
                .context(format!(
                    "Failed to strip prefix {} from path {}.",
                    root_path.as_ref().display(),
                    local_path.display()
                ))?
                .to_path_buf(),
            local_path,
        })
    }
}

#[derive(Debug)]
pub struct UploadResult {
    pub result:                 Result,
    pub successful_upload_size: usize,
    pub total_size:             usize,
}
