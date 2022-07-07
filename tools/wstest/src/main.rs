mod format;

use std::path::PathBuf;
use std::time::Duration;

use clap::Parser;
use clap::ValueHint;
use futures::SinkExt;
use futures::StreamExt;
use tokio::io::AsyncBufReadExt;
use tokio::io::BufReader;
use tokio::sync::mpsc;
use url::Url;
use websocket_lite::ClientBuilder;
use websocket_lite::Message;
use websocket_lite::Opcode;
use websocket_lite::Result;

#[derive(Parser, Debug)]
#[clap(version, about)]
struct Args {
    /// Url to connect to
    #[clap(value_hint = ValueHint::Url)]
    url:                 Url,
    /// Different url to receive responses from
    #[clap(long, value_hint = ValueHint::Url)]
    response_url:        Option<Url>,
    /// Path to a file with commands to send
    #[clap(long, value_name = "FILE", value_hint = ValueHint::FilePath)]
    input:               Option<PathBuf>,
    /// Time in milliseconds to wait before sending the next request after the response is received
    #[clap(long, value_name = "MILLISECONDS", default_value = "0")]
    wait_after_response: u64,
}

#[derive(Debug)]
struct ResponseReceived {}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let client = ClientBuilder::from_url(args.url).async_connect().await?;
    let (sink, mut stream) = client.split();

    if let Some(response_url) = args.response_url {
        let response_client = ClientBuilder::from_url(response_url).async_connect().await?;
        let (_, response_stream) = response_client.split::<Message>();
        stream = response_stream;
    }

    // synchronization between requests and responses
    let (tx, mut rx) = mpsc::channel(1);

    let send_loop = async {
        let mut sink_mut = sink;

        match args.input {
            Some(path_buf) => {
                let file = tokio::fs::File::open(path_buf.as_path()).await?;
                let mut lines = BufReader::new(file).lines();

                while let Some(line) = lines.next_line().await? {
                    let message = Message::text(line.as_str());
                    sink_mut.send(message).await?;
                    println!("{}", format::request(line.as_str()));

                    // wait for response
                    if let None = rx.recv().await {
                        break;
                    }

                    tokio::time::sleep(Duration::from_millis(args.wait_after_response)).await;
                }
            }
            None => {
                let mut lines = BufReader::new(tokio::io::stdin()).lines();

                while let Some(data) = lines.next_line().await? {
                    let message = Message::text(data);
                    sink_mut.send(message).await?;
                }
            }
        }

        Ok(()) as Result<()>
    };

    let recv_loop = async {
        let mut stream_mut = stream;
        loop {
            let (message, stream) = stream_mut.into_future().await;

            let message = if let Some(message) = message {
                message?
            } else {
                break;
            };

            if let Opcode::Text | Opcode::Binary = message.opcode() {
                println!("{}", format::response(message.as_text()));
            }

            // send acknowledgement to send_loop
            tx.send(ResponseReceived {}).await?;
            stream_mut = stream;
        }

        Ok(()) as Result<()>
    };

    tokio::select! {
        _ = send_loop => {}
        _ = recv_loop => {}
    }
    Ok(())
}
