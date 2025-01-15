import type {
  AmplifyError,
  ConfirmSignUpError,
  ForgotPasswordSubmitError,
  ISessionProvider,
  MfaType,
  SignUpError,
  UserSession,
} from '#/authentication/cognito'
import { render, screen, waitFor } from '#/test'
import { Rfc3339DateTime } from '#/utilities/dateTime'
import HttpClient from '#/utilities/HttpClient'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { Suspense } from 'react'
import { Result } from 'ts-results'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { HttpClientProvider } from '../HttpClientProvider'
import SessionProvider from '../SessionProvider'

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

  it('Should retrieve the user session', async () => {
    const { getByText } = render(
      <Suspense fallback={<div>Loading...</div>}>
        <SessionProvider
          authService={authService}
          mainPageUrl={mainPageUrl}
          registerAuthEventListener={registerAuthEventListener}
        >
          <div>Hello</div>
        </SessionProvider>
      </Suspense>,
    )

    expect(authService.userSession).toBeCalled()
    expect(getByText(/Loading/)).toBeInTheDocument()

    await waitFor(() => {
      expect(getByText(/Hello/)).toBeInTheDocument()
    })
  })

  it('Should set the access token on the HTTP client', async () => {
    const httpClient = new HttpClient()

    httpClient.setSessionToken = vi.fn()

    render(
      <Suspense fallback={<div>Loading...</div>}>
        <HttpClientProvider httpClient={httpClient}>
          <SessionProvider
            authService={authService}
            mainPageUrl={mainPageUrl}
            registerAuthEventListener={registerAuthEventListener}
          >
            <div>Hello</div>
          </SessionProvider>
        </HttpClientProvider>
      </Suspense>,
    )

    await waitFor(() => {
      expect(httpClient.setSessionToken).toBeCalledWith('accessToken')
    })
  })

  it('Should refresh the expired user session', async () => {
    authService.userSession.mockReturnValueOnce(
      Promise.resolve({
        ...(await authService.userSession()),
        // 24 hours from now
        expireAt: Rfc3339DateTime(new Date(Date.now() - 1).toJSON()),
      }),
    )

    render(
      <Suspense fallback={<div>Loading...</div>}>
        <SessionProvider
          authService={authService}
          mainPageUrl={mainPageUrl}
          registerAuthEventListener={registerAuthEventListener}
        >
          <div>Hello</div>
        </SessionProvider>
      </Suspense>,
    )

    expect(authService.refreshUserSession).not.toBeCalled()
    expect(authService.userSession).toBeCalledTimes(2)

    await waitFor(() => {
      expect(authService.refreshUserSession).toBeCalledTimes(1)
      expect(screen.getByText(/Hello/)).toBeInTheDocument()

      expect(authService.userSession).toBeCalledTimes(3)
    })
  })

  it('Should refresh not stale user session', { timeout: 5_000 }, async () => {
    authService.userSession.mockReturnValueOnce(
      Promise.resolve({
        ...(await authService.userSession()),
        expireAt: Rfc3339DateTime(new Date(Date.now() + 1_500).toJSON()),
      }),
    )

    let session: UserSession | null = null

    render(
      <Suspense fallback={<div>Loading...</div>}>
        <SessionProvider
          authService={authService}
          mainPageUrl={mainPageUrl}
          registerAuthEventListener={registerAuthEventListener}
        >
          {({ session: sessionFromContext }) => {
            session = sessionFromContext
            return null
          }}
        </SessionProvider>
      </Suspense>,
    )

    vi.useFakeTimers().runAllTimers().useRealTimers()

    await waitFor(
      () => {
        expect(authService.refreshUserSession).toBeCalledTimes(1)
        expect(session).not.toBeNull()
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        expect(new Date(session!.expireAt).getTime()).toBeGreaterThan(Date.now())
      },
      { timeout: 2_000 },
    )
  })

  it('Should call registerAuthEventListener when the session is updated', async () => {
    render(
      <Suspense fallback={<div>Loading...</div>}>
        <SessionProvider
          authService={authService}
          mainPageUrl={mainPageUrl}
          registerAuthEventListener={registerAuthEventListener}
        >
          <div>Hello</div>
        </SessionProvider>
      </Suspense>,
    )

    await waitFor(() => {
      expect(registerAuthEventListener).toBeCalled()
    })
  })
})
