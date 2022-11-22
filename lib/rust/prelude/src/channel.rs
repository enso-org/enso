//! General-purpose code for dealing with futures channels, sinks and streams.

use crate::*;

use futures::channel::mpsc::UnboundedSender;
use futures::StreamExt;



/// Emit message using `UnboundedSender`. Does not care if there are listeners.
pub fn emit<T>(sender: &UnboundedSender<T>, message: T) {
    match sender.unbounded_send(message) {
        Ok(()) => {}
        Err(e) => {
            if e.is_full() {
                // Impossible, as per `futures` library docs.
                panic!("Unbounded channel should never be full.")
            } else if e.is_disconnected() {
                // It is ok for receiver to disconnect and ignore events. We
                // want to just "emit", we do not care if someone is listening.
            } else {
                // Never happens unless `futures` library changes API.
                panic!("Unrecognized error when sending event.")
            }
        }
    }
}

/// Process stream elements while handle exists.
///
/// This function will call `function` for each element received from `stream`, also providing
/// the strong version of `weak` as argument, until the stream ends or handle will no longer exists
/// (the `view` method of `WeakElement` returns `None`).
pub async fn process_stream_with_handle<Stream, Weak, Function, Future>(
    mut stream: Stream,
    weak: Weak,
    mut function: Function,
) where
    Stream: StreamExt + Unpin,
    Weak: WeakElement,
    Function: FnMut(Stream::Item, Weak::Strong) -> Future,
    Future: std::future::Future<Output = ()>,
{
    loop {
        let item_opt = stream.next().await;
        let handle_opt = weak.view();
        match (item_opt, handle_opt) {
            (Some(item), Some(handle)) => function(item, handle).await,
            _ => break,
        }
    }
}
