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
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { Result } from 'ts-results'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { nextTick } from 'vue'
import { createSessionStore } from '../session'

class MockAuthService implements ISessionProvider {
  saveAccessToken = vi.fn()
  refreshUserSession = vi.fn(() => Promise.resolve(null))
  userSession = vi.fn(() =>
    Promise.resolve<UserSession>({
      email: 'test@test.com',
      accessToken: 'accessToken',
      refreshToken: 'refreshToken',
      refreshUrl: 'https://enso.dev',
      expireAt: Rfc3339DateTime(new Date(Date.now() + 5_000).toJSON()),
      clientId: 'clientId',
    }),
  )
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
})
