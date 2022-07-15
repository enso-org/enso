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

static EXPECT_TEXT_RESPONSE: &str = "t|";
static EXPECT_BINARY_RESPONSE: &str = "b|";

#[derive(Parser, Debug)]
#[clap(version, about)]
struct Args {
    /// Text websocket to connect to
    #[clap(value_name = "URL", value_hint = ValueHint::Url)]
    text_socket: Url,

    /// File containing messages to initialize the main socket
    #[clap(long, value_hint = ValueHint::FilePath)]
    init_text_socket: Option<PathBuf>,

    /// File containing responses to ignore
    #[clap(long, value_hint = ValueHint::FilePath)]
    ignore_text_socket_responses: Option<PathBuf>,

    /// Binary socket to connect to
    #[clap(long, value_name = "URL", value_hint = ValueHint::Url)]
    binary_socket: Option<Url>,

    /// File containing messsages to initialize the binary socket
    #[clap(long, value_hint = ValueHint::FilePath)]
    init_binary_socket: Option<PathBuf>,

    /// Path to a file with commands to send
    #[clap(long, value_name = "FILE", value_hint = ValueHint::FilePath)]
    input: Option<PathBuf>,

    /// Input commands expect responses from a binary socket
    #[clap(long)]
    input_expects_binary_responses: bool,

    /// Number of warmup requests to send
    #[clap(long, default_value = "5")]
    warmup_iterations: usize,

    /// Number of benchmarked requests to send
    #[clap(long, default_value = "5")]
    benchmark_iterations: usize,

    /// Time in milliseconds to wait before sending input messages (after the init sequence is
    /// complete)
    #[clap(long, value_name = "MILLISECONDS", default_value = "0")]
    wait_after_init: u64,

    /// Time in milliseconds to wait before starting benchmark (after the warmup is complete)
    #[clap(long, value_name = "MILLISECONDS", default_value = "0")]
    wait_after_warmup: u64,

    /// Time in milliseconds to wait before sending the next request from the `input` file
    #[clap(long, value_name = "MILLISECONDS", default_value = "0")]
    wait_after_response: u64,
}

/// Synchronization messages used for establishing order between requests and responses
#[derive(Debug)]
enum SyncMessage {
    ResponseReceived,
}

/// Read file line by line
async fn read_lines(path_buf: PathBuf) -> Result<Vec<String>> {
    let file = tokio::fs::File::open(path_buf.as_path()).await?;
    let mut lines = BufReader::new(file).lines();

    let mut res = vec![];
    while let Some(line) = lines.next_line().await? {
        res.push(line);
    }

    Ok(res)
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // text socket connection
    let text_socket_client = ClientBuilder::from_url(args.text_socket).async_connect().await?;
    let (text_sink, text_stream) = text_socket_client.split();

    // binary socket connection
    let (mut binary_sink, mut binary_stream) = (None, None);
    if let Some(binary_socket) = args.binary_socket {
        let binary_client = ClientBuilder::from_url(binary_socket).async_connect().await?;
        let (sink, stream) = binary_client.split::<Message>();
        binary_sink = Some(sink);
        binary_stream = Some(stream);
    }

    // ignored responses
    let mut ignored_text_responses = regex::RegexSet::empty();
    if let Some(path_buf) = args.ignore_text_socket_responses {
        let ignored_lines = read_lines(path_buf).await?;
        ignored_text_responses = regex::RegexSet::new(ignored_lines)?;
    }

    // synchronization channels between requests and responses
    let (text_tx, mut text_rx) = mpsc::channel::<SyncMessage>(1);
    let (binary_tx, mut binary_rx) = mpsc::channel::<SyncMessage>(1);

    let send_loop = async {
        let mut sink_mut = text_sink;

        // initialize binary socket
        if let Some(mut sink_mut) = binary_sink {
            if let Some(path_buf) = args.init_binary_socket {
                let file = tokio::fs::File::open(path_buf.as_path()).await?;
                let mut lines = BufReader::new(file).lines();

                while let Some(line) = lines.next_line().await? {
                    let bytes = base64::decode(line)?;
                    let message = Message::binary(bytes);

                    sink_mut.send(message).await?;
                    println!("{}", format::init_request(format::MESSAGE_BINARY));

                    // wait for response
                    if let None = binary_rx.recv().await {
                        break;
                    }
                }
            }
        }

        // initialize text socket
        if let Some(path_buf) = args.init_text_socket {
            let file = tokio::fs::File::open(path_buf.as_path()).await?;
            let mut lines = BufReader::new(file).lines();

            while let Some(line) = lines.next_line().await? {
                let (prefix, message_text) = line.split_at(2);
                let message = Message::text(message_text);

                sink_mut.send(message).await?;
                println!("{}", format::init_request(message_text));

                // wait for response
                if prefix == EXPECT_TEXT_RESPONSE {
                    if let None = text_rx.recv().await {
                        break;
                    }
                } else if prefix == EXPECT_BINARY_RESPONSE {
                    if let None = binary_rx.recv().await {
                        break;
                    }
                }
            }
        }

        // wait after init
        tokio::time::sleep(Duration::from_millis(args.wait_after_init)).await;

        // send input messages
        match args.input {
            Some(path_buf) => {
                let lines = read_lines(path_buf).await?;

                // send warmup messages
                let warmup_input = lines.iter().cycle().take(args.warmup_iterations);
                for line in warmup_input {
                    let message = Message::text(line.as_str());

                    sink_mut.send(message).await?;
                    println!("{}", format::warmup_request(line.as_str()));

                    // wait for response
                    if args.input_expects_binary_responses {
                        if let None = binary_rx.recv().await {
                            break;
                        }
                    } else {
                        if let None = text_rx.recv().await {
                            break;
                        }
                    }

                    tokio::time::sleep(Duration::from_millis(args.wait_after_response)).await;
                }

                // wait after warmup
                tokio::time::sleep(Duration::from_millis(args.wait_after_warmup)).await;

                // send benchmark messages
                let bench_input = lines
                    .iter()
                    .cycle()
                    .take(args.warmup_iterations + args.benchmark_iterations)
                    .skip(args.warmup_iterations);
                for line in bench_input {
                    let message = Message::text(line.as_str());

                    sink_mut.send(message).await?;
                    println!("{}", format::bench_request(line.as_str()));

                    // wait for response
                    if args.input_expects_binary_responses {
                        if let None = binary_rx.recv().await {
                            break;
                        }
                    } else {
                        if let None = text_rx.recv().await {
                            break;
                        }
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

    // receive text messages
    let text_recv_loop = async {
        let mut stream_mut = text_stream;

        loop {
            let (message, stream) = stream_mut.into_future().await;

            let message = if let Some(message) = message {
                message?
            } else {
                break;
            };

            if let Opcode::Text = message.opcode() {
                if let Some(text_message) = message.as_text() {
                    // send acknowledgement
                    if ignored_text_responses.is_match(text_message) {
                        println!("{}", format::response_ignored(text_message));
                    } else {
                        println!("{}", format::response_text(text_message));
                        text_tx.send(SyncMessage::ResponseReceived).await?;
                    }
                }
            }

            stream_mut = stream;
        }

        Ok(()) as Result<()>
    };

    // receive binary messages
    let mut binary_recv = either::Left(futures::future::pending());
    if let Some(mut stream_mut) = binary_stream {
        let binary_recv_loop = async {
            loop {
                let (message, stream) = stream_mut.into_future().await;

                let message = if let Some(message) = message {
                    message?
                } else {
                    break;
                };

                if let Opcode::Binary = message.opcode() {
                    println!("{}", format::response_binary());
                    binary_tx.send(SyncMessage::ResponseReceived).await?;
                }

                stream_mut = stream;
            }
            Ok(()) as Result<()>
        };
        binary_recv = either::Right(binary_recv_loop);
    }
    let binary_recv_loop = async {
        match binary_recv {
            either::Left(l) => Ok(l.await),
            either::Right(r) => r.await,
        }
    };

    tokio::select! {
        _ = send_loop => {}
        _ = text_recv_loop => {}
        _ = binary_recv_loop => {}
    }
    Ok(())
}
