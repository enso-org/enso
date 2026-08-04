import { isCloudUnreachableError } from '$/utils/cloudReachability'
import { NetworkError as BackendError, NotAuthorizedError } from 'enso-common/src/services/Backend'
import { OfflineError, NetworkError as TransportError } from 'enso-common/src/utilities/errors'
import { describe, expect, it } from 'vitest'

describe('isCloudUnreachableError', () => {
  it('recognizes a failed `fetch`', () => {
    expect(isCloudUnreachableError(new TypeError('Failed to fetch'))).toBe(true)
  })

  it('recognizes the error `HttpClient` reports for a failed connection', () => {
    expect(isCloudUnreachableError(new TransportError('Failed to fetch'))).toBe(true)
  })

  it('recognizes the AWS Amplify network error', () => {
    expect(isCloudUnreachableError(new Error('Network error'))).toBe(true)
  })

  it('recognizes being offline', () => {
    expect(isCloudUnreachableError(new OfflineError())).toBe(true)
  })

  it('rejects an unauthorized response, which must keep its own recovery path', () => {
    expect(isCloudUnreachableError(new NotAuthorizedError('Not authorized', 401))).toBe(false)
  })

  it('rejects a response carrying a server-error status', () => {
    expect(isCloudUnreachableError(new BackendError('Internal server error', 500))).toBe(false)
  })

  it('rejects errors that are not about reaching the host', () => {
    expect(isCloudUnreachableError(new Error('Fetch config returned 404'))).toBe(false)
    expect(isCloudUnreachableError(new TypeError('x is not a function'))).toBe(false)
    expect(isCloudUnreachableError(undefined)).toBe(false)
  })
})
