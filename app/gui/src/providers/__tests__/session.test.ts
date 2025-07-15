import { HttpsUrl } from '#/services/Backend'
import HttpClient from '#/utilities/HttpClient'
import { SessionProvider, UserSession } from '$/authentication/auth0'
import { withSetup } from '@/util/testing'
import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { nextTick } from 'vue'
import { createSessionStore } from '../session'

class MockAuthService implements SessionProvider {
  ready = Promise.resolve()
  saveAccessToken = vi.fn()
  refreshUserSession = vi.fn(() => Promise.resolve(null))
  userSession = vi.fn(() =>
    Promise.resolve<UserSession>({
      email: 'test@test.com',
      organizationId: null,
      accessToken: 'accessToken',
      refreshToken: 'refreshToken',
      refreshUrl: HttpsUrl('https://enso.dev'),
      expireAt: Rfc3339DateTime(new Date(Date.now() + 5_000).toJSON()),
      clientId: 'clientId',
    }),
  )
  signUp = vi.fn(() => Promise.resolve())
  signIn = vi.fn(() => Promise.resolve())
  signOut = vi.fn(() => Promise.resolve())
}

describe('SessionProvider', () => {
  const authService = new MockAuthService()

  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('Should retrieve the user session', () =>
    withSetup(async () => {
      const session = createSessionStore(authService, new HttpClient())
      // One tick for useQuery firing refetch
      await nextTick()
      expect(authService.userSession).toBeCalled()
      await expect.poll(() => session.session?.email).toBe('test@test.com')
    }))

  it('Should set the access token on the HTTP client', () =>
    withSetup(async () => {
      const httpClient = new HttpClient()
      httpClient.setSessionToken = vi.fn()
      createSessionStore(authService, httpClient)
      await expect.poll(() => httpClient.setSessionToken).toBeCalledWith('accessToken')
    }))
})
