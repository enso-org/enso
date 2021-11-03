//! Module defines the `OngoingCalls` structure.

use crate::prelude::*;

use futures::channel::oneshot;

use crate::common::error::NoSuchRequest;

/// Stores active requests, i.e. the requests that were sent to the peer but are still awaiting
/// their answer.
/// `Id` identifies the request.
/// `Reply` represents the answer.
#[derive(Debug)]
pub struct OngoingCalls<Id, Reply>
where Id: Hash + Eq {
    logger:        Logger,
    ongoing_calls: HashMap<Id, oneshot::Sender<Reply>>,
}

impl<Id, Reply> OngoingCalls<Id, Reply>
where Id: Copy + Debug + Display + Hash + Eq + Send + Sync + 'static
{
    /// Creates a new, empty ongoing request storage.
    pub fn new(parent: impl AnyLogger) -> OngoingCalls<Id, Reply> {
        OngoingCalls {
            logger:        Logger::new_sub(parent, "ongoing_calls"),
            ongoing_calls: HashMap::new(),
        }
    }

    /// Removes the request from the storage and returns it (if present).
    /// The removed request can be used to either feed the reply or cancel the future result.
    pub fn remove_request(&mut self, id: &Id) -> Option<oneshot::Sender<Reply>> {
        let ret = self.ongoing_calls.remove(id);
        if ret.is_some() {
            info!(self.logger, "Removing request {id}");
        } else {
            info!(self.logger, "Failed to remove non-present request {id}");
        }
        ret
    }

    /// Inserts a new request with given id and completer (i.e. the channel capable of accepting
    /// the peer's reply and completing the request).
    pub fn insert_request(&mut self, id: Id, completer: oneshot::Sender<Reply>) {
        info!(self.logger, "Storing a new request {id}");
        // There will be no previous request, since Ids are assumed to be unique.
        // Still, if there was, we can just safely drop it.
        self.ongoing_calls.insert(id, completer);
    }

    /// Creates a new request and inserts it into the storage.
    ///
    /// `f` is a function that must transform peer's reply into the request's returned value.
    /// Returns a `Future` that shall yield request result, once it is completed (or cancelled).
    pub fn open_new_request<F, R>(
        &mut self,
        id: Id,
        f: F,
    ) -> impl Future<Output = FallibleResult<R>>
    where
        F: FnOnce(Reply) -> FallibleResult<R>,
    {
        let (sender, receiver) = oneshot::channel::<Reply>();
        let ret = receiver.map(move |result_or_cancel| {
            let result = result_or_cancel?;
            f(result)
        });
        self.insert_request(id, sender);
        ret
    }

    /// Removes all awaiting requests. Their futures will signal cancellation.
    pub fn clear(&mut self) {
        info!(self.logger, "Clearing all the requests.");
        self.ongoing_calls.clear()
    }

    /// Passes peer's `reply` to complete request with given `id`.
    /// Fails, if such request was not present in the storage.
    pub fn complete_request(&mut self, id: Id, reply: Reply) -> FallibleResult {
        if let Some(request) = self.remove_request(&id) {
            // Explicitly ignore error. Can happen only if the other side already dropped future
            // with the call result. In such case no one needs to be notified and we are fine.
            let _ = request.send(reply);
            Ok(())
        } else {
            Err(NoSuchRequest(id).into())
        }
    }
}
