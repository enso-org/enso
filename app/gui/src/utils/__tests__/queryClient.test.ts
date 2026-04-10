import { NotAuthorizedError } from 'enso-common/src/services/Backend'
import { describe, expect, it } from 'vitest'
import { createQueryClient } from '../queryClient'

function getRetryPolicy() {
  return createQueryClient().then((queryClient) => {
    const retry = queryClient.getDefaultOptions().queries?.retry
    expect(typeof retry).toBe('function')
    queryClient.clear()
    return retry as (failureCount: number, error: unknown) => boolean
  })
}

describe('createQueryClient retry policy', () => {
  it('does not retry NotAuthorizedError', async () => {
    const retry = await getRetryPolicy()
    expect(retry(0, new NotAuthorizedError('Not authorized', 401))).toBe(false)
  })

  it('does not retry status 401 errors', async () => {
    const retry = await getRetryPolicy()
    expect(retry(0, { status: 401 })).toBe(false)
  })

  it('still retries non-auth transient failures', async () => {
    const retry = await getRetryPolicy()
    expect(retry(0, { status: 500 })).toBe(true)
    expect(retry(2, { status: 500 })).toBe(true)
    expect(retry(3, { status: 500 })).toBe(false)
  })
})
