use crate::prelude::*;

use crate::fs::tokio::copy_to_file;
use crate::fs::tokio::create_parent_dir_if_missing;
use crate::global::progress_bar;

use anyhow::Context;
use reqwest::Client;
use reqwest::IntoUrl;
use reqwest::RequestBuilder;
use reqwest::Response;
use tokio::io::AsyncBufRead;


// ==============
// === Export ===
// ==============

pub mod client;



pub async fn get(url: impl IntoUrl) -> Result<Response> {
    client::get(&Client::default(), url).await
}

pub async fn handle_error_response(response: Response) -> Result<Response> {
    if let Some(e) = response.error_for_status_ref().err() {
        let e = Err(e);
        match response.text().await {
            Ok(body) => e.context(format!("Error message body: {body}")),
            Err(body_error) =>
                e.context(format!("Failed to get error response body: {body_error}")),
        }
    } else {
        Ok(response)
    }
}

pub async fn execute(request_builder: RequestBuilder) -> Result<Response> {
    let request_clone = request_builder.try_clone();
    let inner = async move || handle_error_response(request_builder.send().await?).await;
    inner().await.with_context(|| format!("Failed to execute request: {request_clone:#?}",))
}

/// Get the the response body as a byte stream.
pub async fn download_stream(
    url: impl IntoUrl,
) -> Result<impl Stream<Item = reqwest::Result<Bytes>>> {
    Ok(handle_error_response(reqwest::get(url).await?).await?.bytes_stream())
}

/// Get the the response body as a byte stream.
pub async fn download_reader(url: impl IntoUrl) -> Result<impl AsyncBufRead + Unpin> {
    let response = get(url).await?;
    Ok(async_reader(response))
}

/// Get the the response body as a byte stream.
pub async fn download_file(url: impl IntoUrl, output: impl AsRef<Path>) -> Result {
    stream_response_to_file(reqwest::get(url).await?, output).await
}


#[tracing::instrument(name="Streaming http response to a file.", skip(output, response), fields(
    dest = %output.as_ref().display(),
    url  = %response.url()
), err)]
pub async fn stream_response_to_file(response: Response, output: impl AsRef<Path>) -> Result {
    trace!("Streamed response: {:#?}", response);
    let bar = response_progress_bar(&response);
    let response = handle_error_response(response).await?;
    let reader = async_reader(response);
    let reader = &mut bar.wrap_async_read(reader);
    copy_to_file(reader, output).await?;
    Ok(())
}

/// Creates a progress bar for the response, with the length from the `Content-Length` header.
///
/// The progress bar provides a visual indication of the download progress. Visual indication is
/// helpful for large files, as the build script does not print any output for a long time. Without
/// the progress bar, the user might think that the build script is stuck.
pub fn response_progress_bar(response: &Response) -> indicatif::ProgressBar {
    let url = response.url().to_string();
    let len = response.content_length();
    let draw_target = indicatif::ProgressDrawTarget::stderr();
    let bar = progress_bar(|| indicatif::ProgressBar::with_draw_target(len, draw_target));
    let style = indicatif::ProgressStyle::default_bar();
    bar.set_message(format!("Streaming response to file {url}."));
    bar.set_style(style);
    bar
}

pub fn async_reader(response: Response) -> impl AsyncBufRead + Unpin {
    tokio_util::io::StreamReader::new(response.bytes_stream().map_err(std::io::Error::other))
}

pub fn filename_from_content_disposition(value: &reqwest::header::HeaderValue) -> Result<&Path> {
    let regex = regex::Regex::new(r#"filename="?([^"]*)"?"#)?;
    let capture = regex
        .captures(value.to_str()?)
        .context("Field 'filename' not present in the header value.")?
        .get(1)
        .context("Missing capture group from regex.")?;
    Ok(Path::new(capture.as_str()))
}

pub fn filename_from_response(response: &Response) -> Result<&Path> {
    use reqwest::header::CONTENT_DISPOSITION;
    let disposition = response
        .headers()
        .get(CONTENT_DISPOSITION)
        .context(format!("No {CONTENT_DISPOSITION} header present in the response."))?;
    filename_from_content_disposition(disposition)
}

pub async fn stream_to_file(
    stream: impl Stream<Item = reqwest::Result<Bytes>>,
    output_path: impl AsRef<Path>,
) -> Result {
    debug!("Streaming download to file {}. ", output_path.as_ref().display());
    create_parent_dir_if_missing(&output_path).await?;
    let output = tokio::fs::OpenOptions::new().write(true).create(true).open(&output_path).await?;
    stream
        .map_err(anyhow::Error::from)
        // We must use fold (rather than foreach) to properly keep `output` alive long enough.
        .try_fold(output, |mut output, chunk| async move {
            output.write_all(&chunk.clone()).await?;
            Ok(output)
        })
        .await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    use reqwest::header::HeaderValue;

    #[test]
    fn test_parsing_content_disposition() {
        let check_parse = |value: &'static str, expected: &str| {
            let header_value = HeaderValue::from_static(value);
            assert_eq!(
                filename_from_content_disposition(&header_value).unwrap(),
                Path::new(expected)
            );
        };
        let check_no_parse = |value: &'static str| {
            let header_value = HeaderValue::from_static(value);
            assert!(filename_from_content_disposition(&header_value).is_err());
        };

        check_parse(r#"attachment; filename="filename.jpg""#, "filename.jpg");
        check_parse(r#"form-data; name="fieldName"; filename="filename.jpg""#, "filename.jpg");
        check_parse(r#"attachment; filename=manifest.yaml"#, "manifest.yaml");
        check_no_parse(r#"attachment"#);
        check_no_parse(r#"inline"#);
    }
}
