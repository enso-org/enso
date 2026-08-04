import type {
  AmplifyError,
  ConfirmSignUpError,
  ForgotPasswordSubmitError,
  ISessionProvider,
  MfaType,
  SignUpError,
  UserSession,
} from '$/authentication/cognito'
import { withSetup } from '@/util/testing'
import * as vueQuery from '@tanstack/vue-query'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { NetworkError } from 'enso-common/src/utilities/errors'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { Result } from 'ts-results'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { nextTick } from 'vue'
import { createSessionStore, USER_SESSION_QUERY_KEY } from '../session'

function createUserSession(): UserSession {
  return {
    email: 'test@test.com',
    accessToken: 'accessToken',
    refreshToken: 'refreshToken',
    refreshUrl: 'https://enso.dev',
    expireAt: Rfc3339DateTime(new Date(Date.now() + 5_000).toJSON()),
    clientId: 'clientId',
  }
}

class MockAuthService implements ISessionProvider {
  saveAccessToken = vi.fn()
  refreshUserSession = vi.fn(() => Promise.resolve(null))
  userSession = vi.fn((): Promise<UserSession | null> => Promise.resolve(createUserSession()))
  email = vi.fn().mockReturnValue('example@email.com')
  changePassword = vi.fn()
  forgotPassword = vi.fn()
  organizationId = vi.fn().mockReturnValue(`organization-${uniqueString()}`)
  confirmSignIn = vi.fn()
  confirmSignUp = vi.fn(() => Promise.resolve(Result.wrap<undefined, ConfirmSignUpError>(() => {})))
  forgotPasswordSubmit = vi.fn(() =>
    Promise.resolve(Result.wrap<undefined, ForgotPasswordSubmitError>(() => {})),
  )
  setupTOTP = vi.fn(() =>
    Promise.resolve(
      Result.wrap<{ secret: string; url: string }, AmplifyError>(() => ({
        secret: 'secret',
        url: 'url',
      })),
    ),
  )
  getMFAPreference = vi.fn(() => Promise.resolve(Result.wrap<MfaType, AmplifyError>(() => 'NOMFA')))
  signInWithGitHub = vi.fn(() => Promise.resolve())
  signInWithGoogle = vi.fn(() => Promise.resolve())
  signInWithMicrosoft = vi.fn(() => Promise.resolve())
  signInWithApple = vi.fn(() => Promise.resolve())
  signOut = vi.fn(() => Promise.resolve())
  signUp = vi.fn(() => Promise.resolve(Result.wrap<undefined, SignUpError>(() => {})))
  updateMFAPreference = vi.fn()
  signInWithPassword = vi.fn()
  verifyTotpSetup = vi.fn()
  verifyTotpToken = vi.fn()
}

describe('SessionProvider', () => {
  const registerAuthEventListener = vi.fn()

  const authService = new MockAuthService()

  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('Should retrieve the user session', () =>
    withSetup(async () => {
      const session = createSessionStore(authService, registerAuthEventListener, new HttpClient())
      // One tick for useQuery firing refetch
      await nextTick()
      expect(authService.userSession).toBeCalled()
      await expect.poll(() => session.session?.email).toBe('test@test.com')
    }))

  it('Should set the access token on the HTTP client', () =>
    withSetup(async () => {
      const httpClient = new HttpClient()
      httpClient.setSessionToken = vi.fn()
      createSessionStore(authService, registerAuthEventListener, httpClient)
      await expect.poll(() => httpClient.setSessionToken).toBeCalledWith('accessToken')
    }))

  it('Should call registerAuthEventListener when the session is updated', () =>
    withSetup(async () => {
      createSessionStore(authService, registerAuthEventListener, new HttpClient())
      await nextTick()
      expect(registerAuthEventListener).toBeCalled()
    }))

  describe('when Cognito cannot be reached', () => {
    it('resolves the session query rather than failing it', () =>
      withSetup(async () => {
        authService.userSession.mockRejectedValue(new NetworkError('Failed to fetch'))

        const session = createSessionStore(authService, registerAuthEventListener, new HttpClient())

        // A failed session query would leave startup waiting on a query that never succeeds.
        await expect.poll(() => session.isCloudUnreachable).toBe(true)
        expect(session.session).toBeNull()
      }))

    it('does not confuse being signed out with being unable to ask', () =>
      withSetup(async () => {
        authService.userSession.mockResolvedValue(null)

        const session = createSessionStore(authService, registerAuthEventListener, new HttpClient())

        await expect.poll(() => session.session).toBeNull()
        expect(session.isCloudUnreachable).toBe(false)
      }))

    it('clears the flag once Cognito answers again', () =>
      withSetup(async () => {
        const queryClient = vueQuery.useQueryClient()
        authService.userSession.mockRejectedValue(new NetworkError('Failed to fetch'))
        const session = createSessionStore(authService, registerAuthEventListener, new HttpClient())
        await expect.poll(() => session.isCloudUnreachable).toBe(true)

        authService.userSession.mockResolvedValue(createUserSession())
        await queryClient.refetchQueries({ queryKey: USER_SESSION_QUERY_KEY })

        expect(session.isCloudUnreachable).toBe(false)
      }))
  })
})
