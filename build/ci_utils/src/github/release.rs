use crate::prelude::*;

use octocrab::models::ReleaseId;
use reqwest::Body;
use tracing::instrument;



#[context("Failed to upload the asset {}", asset.as_ref().display())]
#[instrument(skip_all, fields(source = %asset.as_ref().display(), %repo, %release))]
pub async fn upload_asset(
    repo: &(impl RepoPointer + Send + Sync + 'static),
    client: &reqwest::Client,
    release: ReleaseId,
    asset: impl AsRef<Path> + Send + Sync,
) -> Result {
    let upload_url = format!(
        "https://uploads.github.com/repos/{}/{}/releases/{}/assets",
        repo.owner(),
        repo.name(),
        release
    );
    let asset_path = asset.as_ref();
    let mime = new_mime_guess::from_path(asset_path).first_or_octet_stream();
    let file = tokio::fs::File::open(asset_path).await?;
    let file_size = file.metadata().await?.len();
    let file_contents_stream = tokio_util::io::ReaderStream::new(file);
    let body = Body::wrap_stream(file_contents_stream);
    let asset_name = asset_path.file_name().unwrap().to_string_lossy();
    let request = client
        .post(upload_url)
        .query(&[("name", asset_name.as_ref())])
        .header(reqwest::header::ACCEPT, "application/vnd.github.v3+json")
        .header(reqwest::header::CONTENT_TYPE, mime.to_string())
        .header(reqwest::header::CONTENT_LENGTH, file_size)
        .body(body)
        .build()?;

    dbg!(&request);
    let response = client.execute(request).await?;
    dbg!(&response);
    response.error_for_status()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use reqwest::header::HeaderMap;
    use reqwest::Body;

    #[tokio::test]
    #[ignore]
    pub async fn create_release() -> Result {
        let pat = std::env::var("GITHUB_TOKEN").unwrap();

        let octocrab = octocrab::Octocrab::builder().personal_token(pat.clone()).build()?;
        let repo = octocrab.repos("enso-org", "ci-build");
        let release = if let Ok(release) = repo.releases().get_latest().await {
            release
        } else {
            repo.releases()
                .create("release_tag_name")
                .name("release_name")
                .target_commitish("4ba61ded019a9d4919e2b22a0ed3746ff2a6c088")
                .send()
                .await?
        };


        dbg!(&release);

        let mut header_map = HeaderMap::new();
        header_map.append(reqwest::header::AUTHORIZATION, format!("Bearer {}", pat).parse()?);
        let client = reqwest::Client::builder()
            .user_agent("enso-build")
            .default_headers(header_map)
            .build()?;

        // TODO label?


        let upload_url_string = release.upload_url.to_string();
        dbg!(&upload_url_string);

        let upload_url = format!(
            "https://uploads.github.com/repos/{}/{}/releases/{}/assets",
            "enso-org", "ci-build", release.id
        );
        let file_to_upload = PathBuf::from(r"D:\mask_bin.png");
        let mime = new_mime_guess::from_path(&file_to_upload).first_or_octet_stream();
        let file = tokio::fs::File::open(&file_to_upload).await?;
        let file_size = file.metadata().await?.len();
        let file_contents_stream = tokio_util::io::ReaderStream::new(file);
        let body = Body::wrap_stream(file_contents_stream);
        let request = client
            .post(upload_url)
            .query(&[("name", "testowyasset2.png")])
            .header(reqwest::header::ACCEPT, "application/vnd.github.v3+json")
            .header(reqwest::header::CONTENT_TYPE, mime.to_string())
            .header(reqwest::header::CONTENT_LENGTH, file_size)
            .body(body)
            .build()?;

        dbg!(&request);
        let response = client.execute(request).await?;


        dbg!(&response);
        // debug!("{}", response.text().await?);
        response.error_for_status()?;
        Ok(())
    }
}
