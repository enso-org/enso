import HttpClient from '#/utilities/HttpClient'
import type {
  AmplifyError,
  ConfirmSignUpError,
  ForgotPasswordSubmitError,
  ISessionProvider,
  MfaType,
  SignUpError,
  UserSession,
} from '$/authentication/cognito'
import { withEffectsScope } from '@/util/testing'
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
  getMFAPreference = vi.fn(() =>
    Promise.resolve(Result.wrap<MfaType, AmplifyError>(() => 'NOMFA' as const)),
  )
  signInWithGitHub = vi.fn(() => Promise.resolve())
  signInWithGoogle = vi.fn(() => Promise.resolve())
  signOut = vi.fn(() => Promise.resolve())
  signUp = vi.fn(() => Promise.resolve(Result.wrap<undefined, SignUpError>(() => {})))
  updateMFAPreference = vi.fn()
  signInWithPassword = vi.fn()
  verifyTotpSetup = vi.fn()
  verifyTotpToken = vi.fn()
}

describe('SessionProvider', () => {
  const mainPageUrl = new URL('https://enso.dev')
  const registerAuthEventListener = vi.fn()

  const authService = new MockAuthService()

  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('Should retrieve the user session', () =>
    withEffectsScope(async () => {
      const session = createSessionStore(new HttpClient())
      await nextTick()
      expect(authService.userSession).toBeCalled()
      expect(session.session?.email).toBe('test@test.com')
    }))

  it('Should set the access token on the HTTP client', () =>
    withEffectsScope(async () => {
      const httpClient = new HttpClient()
      httpClient.setSessionToken = vi.fn()
      createSessionStore(httpClient)
      await nextTick()
      expect(httpClient.setSessionToken).toBeCalledWith('accessToken')
    }))

  it('Should call registerAuthEventListener when the session is updated', () =>
    withEffectsScope(async () => {
      createSessionStore(new HttpClient())
      await nextTick()
      expect(registerAuthEventListener).toBeCalled()
    }))
})
