import type { UserSession as CognitoUserSession } from '$/authentication/cognito'
import { createAuthStore } from '$/providers/auth'
import { createQueryClient } from '$/utils/queryClient'
import { VueQueryPlugin } from '@tanstack/vue-query'
import {
  isDirectoryId,
  isOrganizationId,
  isUserId,
  NotAuthorizedError,
  Plan,
} from 'enso-common/src/services/Backend'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import type { LocalBackend } from 'enso-common/src/services/LocalBackend'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { defaultGetText } from 'enso-common/src/text'
import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { NetworkError } from 'enso-common/src/utilities/errors'
import { describe, expect, it, vi } from 'vitest'
import { computed, createApp, reactive } from 'vue'
import { isUsersMeQueryKey, makeSyntheticUser } from '../auth'

function fakeCognitoSession(overrides: Partial<CognitoUserSession> = {}): CognitoUserSession {
  return {
    email: 'user@example.com',
    accessToken: 'access',
    refreshToken: 'refresh',
    refreshUrl: 'https://example.com',
    expireAt: Rfc3339DateTime(new Date(Date.now() + 60_000).toJSON()),
    clientId: 'cognito-client-id',
    ...overrides,
  }
}

describe('isUsersMeQueryKey', () => {
  it('matches reactive usersMe query keys', () => {
    expect(isUsersMeQueryKey(['remote', 'usersMe', computed(() => 'client-id')])).toBe(true)
  })

  it('rejects unrelated query keys', () => {
    expect(isUsersMeQueryKey(['remote', 'otherQuery', computed(() => 'client-id')])).toBe(false)
  })
})

describe('makeSyntheticUser', () => {
  it('returns a placeholder user without any features enabled', () => {
    const user = makeSyntheticUser(fakeCognitoSession())
    expect(user.isEnabled).toBe(false)
    expect(user.isOrganizationAdmin).toBe(false)
    expect(user.isEnsoTeamMember).toBe(false)
    expect(user.plan).toBe(Plan.free)
    expect(user.userGroups).toBeNull()
    expect(user.groups).toEqual([])
  })

  it('derives identifiers in the expected newtype shape', () => {
    const user = makeSyntheticUser(fakeCognitoSession({ email: 'someone@enso.org' }))
    expect(isUserId(user.userId)).toBe(true)
    expect(user.userId).toContain('someone@enso.org')
    expect(isOrganizationId(user.organizationId)).toBe(true)
    expect(isDirectoryId(user.rootDirectoryId)).toBe(true)
  })

  it('propagates the Cognito email into name and email fields', () => {
    const user = makeSyntheticUser(fakeCognitoSession({ email: 'someone@enso.org' }))
    expect(user.email).toBe('someone@enso.org')
    expect(user.name).toBe('someone@enso.org')
  })

  it('keys identifiers on email so two users on the same Cognito app are distinct', () => {
    const a = makeSyntheticUser(fakeCognitoSession({ email: 'a@enso.org' }))
    const b = makeSyntheticUser(fakeCognitoSession({ email: 'b@enso.org' }))
    expect(a.userId).not.toBe(b.userId)
  })

  it('handles a missing email without producing an empty identifier', () => {
    const user = makeSyntheticUser(fakeCognitoSession({ email: '' }))
    expect(user.userId).toBe('user-cloud-unavailable-unknown')
  })
})

interface AuthStoreOptions {
  /** The Cognito session, or `null` when the user is not (known to be) signed in. */
  readonly cognitoSession?: CognitoUserSession | null
  /** Whether a local backend is configured, i.e. this is not a cloud-only deployment. */
  readonly hasLocalBackend?: boolean
  /** Whether the configuration endpoint could not be reached. */
  readonly isConfigUnreachable?: boolean
  /** Whether Cognito could not be reached. */
  readonly isCognitoUnreachable?: boolean
  readonly usersMe?: () => Promise<never>
}

async function setupAuthStore({
  cognitoSession = null,
  hasLocalBackend = true,
  isConfigUnreachable = false,
  isCognitoUnreachable = false,
  usersMe = () => Promise.reject(new NetworkError('Failed to fetch')),
}: AuthStoreOptions = {}) {
  const remoteBackend = new RemoteBackend({
    apiUrl: 'https://cloud.invalid',
    getText: defaultGetText,
    client: new HttpClient(),
    downloader: () => {},
    downloadCloudProject: () => Promise.reject(new Error('unused')),
    getProjectArchive: () => Promise.reject(new Error('unused')),
  })
  vi.spyOn(remoteBackend, 'usersMe').mockImplementation(usersMe)

  const sessionData = reactive({
    session: cognitoSession,
    isLoggingOut: false,
    isReconnectingSession: false,
    isCloudUnreachable: isCognitoUnreachable,
    organizationId: () => Promise.resolve(null),
    signOut: () => Promise.resolve(),
    waitForSession: () => Promise.resolve(),
  })
  const config = reactive({ isCloudUnreachable: isConfigUnreachable })
  // `LocalBackend`'s behavior is irrelevant here; only its presence is, as it decides whether
  // there is anywhere to degrade to.
  const localBackend = hasLocalBackend ? (Object.create(null) as LocalBackend) : null

  const queryClient = await createQueryClient()
  let store: ReturnType<typeof createAuthStore> | undefined
  const app = createApp({
    setup() {
      store = createAuthStore(
        undefined,
        sessionData,
        { localBackend, remoteBackend },
        { getText: defaultGetText },
        config,
      )
      return () => {}
    },
  })
  app.use(VueQueryPlugin, { queryClient })
  app.mount(document.createElement('div'))
  return { store: store!, sessionData, config }
}

describe('auth store while the Cloud is unreachable', () => {
  it('degrades to a local-only session even though nobody could be signed in', async () => {
    const { store } = await setupAuthStore({ cognitoSession: null, isConfigUnreachable: true })

    expect(store.isCloudDataUnavailable).toBe(true)
    // A session is required, or the app would bounce to a login screen that cannot work either.
    expect(store.session).not.toBeNull()
    expect(store.session?.isCloudDataUnavailable).toBe(true)
    expect(store.session?.user.isEnabled).toBe(false)
    expect(store.session?.isNotSignedIn).toBe(true)
  })

  it('degrades when only Cognito is unreachable', async () => {
    const { store } = await setupAuthStore({ cognitoSession: null, isCognitoUnreachable: true })

    expect(store.isCloudDataUnavailable).toBe(true)
    expect(store.session?.isCloudDataUnavailable).toBe(true)
  })

  it('grants the stand-in session no credentials', async () => {
    const { store } = await setupAuthStore({ cognitoSession: null, isConfigUnreachable: true })

    expect(store.session?.accessToken).toBe('')
    expect(store.session?.refreshToken).toBe('')
  })

  it('keeps the signed-in identity when one is already known', async () => {
    const { store } = await setupAuthStore({
      cognitoSession: fakeCognitoSession({ email: 'someone@enso.org' }),
      isCognitoUnreachable: true,
    })

    expect(store.isCloudDataUnavailable).toBe(true)
    expect(store.session?.user.email).toBe('someone@enso.org')
    expect(store.session?.isNotSignedIn).toBe(false)
  })

  it('redirects to login on cloud-only deployments instead of degrading', async () => {
    const { store } = await setupAuthStore({
      cognitoSession: null,
      hasLocalBackend: false,
      isConfigUnreachable: true,
    })

    expect(store.isCloudDataUnavailable).toBe(false)
    expect(store.session).toBeNull()
  })

  it('does not degrade while signing out', async () => {
    const { store, sessionData } = await setupAuthStore({ isConfigUnreachable: true })
    sessionData.isLoggingOut = true

    expect(store.isCloudDataUnavailable).toBe(false)
  })

  it('recovers once the Cloud answers again', async () => {
    const { store, config } = await setupAuthStore({ isConfigUnreachable: true })
    expect(store.isCloudDataUnavailable).toBe(true)

    config.isCloudUnreachable = false

    expect(store.isCloudDataUnavailable).toBe(false)
    expect(store.session).toBeNull()
  })
})

describe('auth store while the Cloud is reachable but failing', () => {
  it('degrades when `users/me` fails for a signed-in user', async () => {
    const { store } = await setupAuthStore({ cognitoSession: fakeCognitoSession() })

    await vi.waitFor(() => expect(store.isCloudDataUnavailable).toBe(true))
    expect(store.session?.isCloudDataUnavailable).toBe(true)
    expect(store.session?.isNotSignedIn).toBe(false)
  })

  it('leaves an unauthorized user to the sign-out flow', async () => {
    const { store } = await setupAuthStore({
      cognitoSession: fakeCognitoSession(),
      usersMe: () => Promise.reject(new NotAuthorizedError('Not authorized', 401)),
    })

    await vi.waitFor(() => expect(store.session).toBeNull())
    expect(store.isCloudDataUnavailable).toBe(false)
  })

  it('does not invent a session for a signed-out user', async () => {
    const { store } = await setupAuthStore({ cognitoSession: null })

    expect(store.isCloudDataUnavailable).toBe(false)
    expect(store.session).toBeNull()
  })
})
