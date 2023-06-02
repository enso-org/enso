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
pub enum RetryResult<T, E> {
    Ok(T),
    OkAfterRetries(T, NonEmptyVec<E>),
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
