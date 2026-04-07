import { describe, expect, it } from 'vitest'
import { shouldWaitForResolvedSession } from '../sessionResolution'

describe('shouldWaitForResolvedSession', () => {
  it('waits when entering protected routes', () => {
    expect(shouldWaitForResolvedSession('anyLoggedIn', 'guest', null)).toBe(true)
    expect(shouldWaitForResolvedSession('deleted', 'guest', null)).toBe(true)
  })

  it('waits for guest routes while the session is still unresolved', () => {
    expect(shouldWaitForResolvedSession('guest', 'anyLoggedIn', undefined)).toBe(true)
  })

  it('skips guest-route waits once logout is already resolved', () => {
    expect(shouldWaitForResolvedSession('guest', 'anyLoggedIn', null)).toBe(false)
  })

  it('skips redundant waits when access level does not change', () => {
    expect(shouldWaitForResolvedSession('guest', 'guest', undefined)).toBe(false)
  })
})
