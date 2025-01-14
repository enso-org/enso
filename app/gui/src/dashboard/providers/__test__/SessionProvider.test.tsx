import type { UserSession } from '#/authentication/cognito'
import { render, screen, waitFor } from '#/test'
import { Rfc3339DateTime } from '#/utilities/dateTime'
import HttpClient from '#/utilities/HttpClient'
import { Suspense } from 'react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { HttpClientProvider } from '../HttpClientProvider'
import SessionProvider from '../SessionProvider'

describe('SessionProvider', () => {
  const mainPageUrl = new URL('https://enso.dev')
  const userSession = vi.fn(() =>
    Promise.resolve<UserSession>({
      email: 'test@test.com',
      accessToken: 'accessToken',
      refreshToken: 'refreshToken',
      refreshUrl: 'https://enso.dev',
      expireAt: Rfc3339DateTime(new Date(Date.now() + 5_000).toJSON()),
      clientId: 'clientId',
    }),
  )
  const refreshUserSession = vi.fn(() => Promise.resolve(null))
  const registerAuthEventListener = vi.fn()
  const saveAccessToken = vi.fn()

  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('Should retrieve the user session', async () => {
    const { getByText } = render(
      <Suspense fallback={<div>Loading...</div>}>
        <SessionProvider
          mainPageUrl={mainPageUrl}
          userSession={userSession}
          refreshUserSession={refreshUserSession}
          registerAuthEventListener={registerAuthEventListener}
          saveAccessToken={saveAccessToken}
        >
          <div>Hello</div>
        </SessionProvider>
      </Suspense>,
    )

    expect(userSession).toBeCalled()
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
            mainPageUrl={mainPageUrl}
            userSession={userSession}
            refreshUserSession={refreshUserSession}
            registerAuthEventListener={registerAuthEventListener}
            saveAccessToken={saveAccessToken}
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
    userSession.mockReturnValueOnce(
      Promise.resolve({
        ...(await userSession()),
        // 24 hours from now
        expireAt: Rfc3339DateTime(new Date(Date.now() - 1).toJSON()),
      }),
    )

    render(
      <Suspense fallback={<div>Loading...</div>}>
        <SessionProvider
          mainPageUrl={mainPageUrl}
          userSession={userSession}
          refreshUserSession={refreshUserSession}
          registerAuthEventListener={registerAuthEventListener}
          saveAccessToken={saveAccessToken}
        >
          <div>Hello</div>
        </SessionProvider>
      </Suspense>,
    )

    expect(refreshUserSession).not.toBeCalled()
    expect(userSession).toBeCalledTimes(2)

    await waitFor(() => {
      expect(refreshUserSession).toBeCalledTimes(1)
      expect(screen.getByText(/Hello/)).toBeInTheDocument()

      expect(userSession).toBeCalledTimes(3)
    })
  })

  it('Should refresh not stale user session', { timeout: 5_000 }, async () => {
    userSession.mockReturnValueOnce(
      Promise.resolve({
        ...(await userSession()),
        expireAt: Rfc3339DateTime(new Date(Date.now() + 1_500).toJSON()),
      }),
    )

    let session: UserSession | null = null

    render(
      <Suspense fallback={<div>Loading...</div>}>
        <SessionProvider
          mainPageUrl={mainPageUrl}
          userSession={userSession}
          refreshUserSession={refreshUserSession}
          registerAuthEventListener={registerAuthEventListener}
          saveAccessToken={saveAccessToken}
        >
          {({ session: sessionFromContext }) => {
            session = sessionFromContext
            return null
          }}
        </SessionProvider>
      </Suspense>,
    )

    await waitFor(
      () => {
        expect(refreshUserSession).toBeCalledTimes(1)
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
          mainPageUrl={mainPageUrl}
          userSession={userSession}
          refreshUserSession={refreshUserSession}
          registerAuthEventListener={registerAuthEventListener}
          saveAccessToken={saveAccessToken}
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
