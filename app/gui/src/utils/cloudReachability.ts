/** @file Classification of failures caused by the Enso Cloud being unreachable. */
import {
  isNetworkError,
  OfflineError,
  NetworkError as TransportError,
} from 'enso-common/src/utilities/errors'

/**
 * Whether the error means that a Cloud host could not be reached at all — the name did not
 * resolve, or the connection could not be established — as opposed to the host answering with
 * an HTTP status.
 *
 * Such a failure carries no information about the user's authorization, so it must never be
 * mistaken for a `401`: retrying it or signing the user out cannot make it succeed. The only
 * useful response is to stop waiting for the Cloud and continue with local projects.
 *
 * `fetch` reports these as a bare {@link TypeError}, `HttpClient` rewraps them as
 * {@link TransportError}, and AWS Amplify reports them with its own `'Network error'` message.
 */
export function isCloudUnreachableError(error: unknown): boolean {
  return error instanceof OfflineError || error instanceof TransportError || isNetworkError(error)
}
