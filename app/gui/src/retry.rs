//! A utilities used for retrying various asynchronous operations.

use crate::prelude::*;

use enso_web::sleep;
use std::time::Duration;



// =======================
// === retry_operation ===
// =======================

// === RetryResult ===

/// A result of `retry_operation` method.
///
/// Similar to [`Result`] it returns a vector of all failures and it has additional variant
/// `OkAfterRetries` returning both result of successful call and all errors. It can be cast to
/// `Result<T, NonEmptyVec<E>` or `Result<T, E>` with the first error from the list.
#[derive(Clone, Debug, Eq, PartialEq)]
#[must_use]
pub enum RetryResult<T, E> {
    /// The operation was successful without any retries.
    Ok(T),
    /// The operation succeeded at some retry.
    OkAfterRetries(T, NonEmptyVec<E>),
    /// The operation and all retries failed.
    Err(NonEmptyVec<E>),
}

impl<T, E> From<RetryResult<T, E>> for Result<T, NonEmptyVec<E>> {
    fn from(value: RetryResult<T, E>) -> Self {
        match value {
            RetryResult::Ok(value) => Ok(value),
            RetryResult::OkAfterRetries(value, _) => Ok(value),
            RetryResult::Err(errors) => Err(errors),
        }
    }
}

impl<T, E> From<RetryResult<T, E>> for Result<T, E> {
    fn from(value: RetryResult<T, E>) -> Self {
        let result_with_vec: Result<T, NonEmptyVec<E>> = value.into();
        result_with_vec.map_err(|errors| errors.take_first())
    }
}


// === retry_operation ===

/// Run asynchronous operation retrying if not successful.
///
/// This function runs the `operation` and, if it returned [`Err`] wait some time and try again.
///
/// The waiting times are specified by `retry_times` argument. If the iterator yield no more
/// element, no retry is performed anymore and [`RetryResult::Err`] is returned.
///
/// If operation was successful only after some retry, [`RetryResult::OkAfterRetries`] is returned.
pub async fn retry_operation<Operation, T, E>(
    operation: impl FnMut() -> Operation,
    retry_times: impl IntoIterator<Item = Duration>,
    message_on_failure: &str,
) -> RetryResult<T, E>
where
    Operation: Future<Output = Result<T, E>>,
    E: Display,
{
    retry_operation_errors_cap(operation, retry_times, message_on_failure, usize::MAX).await
}


// === retry_operation_errors_cap ===

/// Similar to [`retry_operation`] but the number of errors is capped, preventing making huge
/// vectors in cases when, for example, we retry something indefinitely.
///
/// If cap is reached, the earlier errors will be kept and later will be discarded.
pub async fn retry_operation_errors_cap<Operation, T, E>(
    mut operation: impl FnMut() -> Operation,
    retry_times: impl IntoIterator<Item = Duration>,
    message_on_failure: &str,
    errors_cap: usize,
) -> RetryResult<T, E>
where
    Operation: Future<Output = Result<T, E>>,
    E: Display,
{
    let result = operation().await;
    result.log_err(message_on_failure);
    match result {
        Ok(result) => RetryResult::Ok(result),
        Err(first_error) => {
            let mut errors = NonEmptyVec::singleton(first_error);
            let mut retry_times = retry_times.into_iter();
            loop {
                match retry_times.next() {
                    Some(time) => {
                        error!("Retrying after {} seconds", time.as_secs_f32());
                        sleep(time).await;
                    }
                    None => break RetryResult::Err(errors),
                };
                let retry = operation().await;
                retry.log_err(message_on_failure);
                match retry {
                    Ok(result) => break RetryResult::OkAfterRetries(result, errors),
                    Err(error) if errors.len() < errors_cap => errors.push(error),
                    Err(_) => {}
                }
            }
        }
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn successful_operation() {
        let operation = || async { Ok(4) };
        let retry_times = iter::repeat_with(|| panic!("Should not ask for retry time."));
        let mut future =
            retry_operation(operation, retry_times, "Test operation failed. This cannot happen.")
                .boxed_local();
        let result: RetryResult<_, usize> = future.expect_ready();
        assert_eq!(result, RetryResult::Ok(4));
    }

    #[test]
    fn operation_successful_after_retry() {
        let mut call_index = 0;
        let operation = move || {
            call_index += 1;
            async move {
                if call_index >= 3 {
                    Ok(call_index)
                } else {
                    Err(call_index)
                }
            }
        };
        let retry_times = [10, 20, 30, 40, 50].into_iter().map(Duration::from_millis);
        let mut future =
            retry_operation(operation, retry_times, "Test operation failed.").boxed_local();
        future.expect_pending();
        std::thread::sleep(Duration::from_millis(10));
        future.expect_pending();
        std::thread::sleep(Duration::from_millis(30));
        let result = future.expect_ready();
        assert_eq!(result, RetryResult::OkAfterRetries(3, NonEmptyVec::new(1, vec![2])));
    }

    #[test]
    fn operation_always_failing() {
        let mut call_index = 0;
        let operation = move || {
            call_index += 1;
            async move { Err(call_index) }
        };
        let retry_times = [10, 20, 30, 40, 50].into_iter().map(Duration::from_millis);
        let mut future =
            retry_operation(operation, retry_times, "One does not simply walk into Mordor.")
                .boxed_local();
        future.expect_pending();
        std::thread::sleep(Duration::from_millis(10));
        future.expect_pending();
        std::thread::sleep(Duration::from_millis(20));
        future.expect_pending();
        std::thread::sleep(Duration::from_millis(30));
        future.expect_pending();
        std::thread::sleep(Duration::from_millis(40));
        future.expect_pending();
        std::thread::sleep(Duration::from_millis(60));
        let result: RetryResult<usize, _> = future.expect_ready();
        assert_eq!(result, RetryResult::Err(NonEmptyVec::new(1, vec![2, 3, 4, 5, 6])));
    }
}
