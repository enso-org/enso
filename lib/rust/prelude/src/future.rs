//! General purpose functions for dealing with `impl Future` values.

use futures::FutureExt;



/// Build a `Future` that immediately yields given value and is wrapped into `Pin<Box<…>>`.
pub fn ready_boxed<'a, T: 'a>(t: T) -> futures::future::LocalBoxFuture<'a, T> {
    futures::future::ready(t).boxed_local()
}
