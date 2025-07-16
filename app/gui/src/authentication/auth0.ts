import { HttpsUrl, type OrganizationId } from '#/services/Backend'
import { Auth0Client, User as Auth0User, CacheKey } from '@auth0/auth0-spa-js'
import { CacheManager } from '@auth0/auth0-spa-js/dist/typings/cache'
import { isOnElectron } from 'enso-common/src/detect'
import { toRfc3339, type Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'

const AUTH0_SCOPE = 'profile email offline_access'
const AUTH0_AUDIENCE = 'default'

/** User's session, provides information for identifying and authenticating the user. */
export interface UserSession {
  /**
   * User's email address, used to uniquely identify the user.
   * Provided by the identity provider used by the user to log in.
   * One of Google, Microsoft, Apple, GitHub or email.
   */
  readonly email: string
  /** User's access token, used to authenticate the user (e.g., when making API calls). */
  readonly accessToken: string
  /** User's refresh token, used to refresh the access token when it expires. */
  readonly refreshToken: string
  /** URL to refresh the access token. */
  readonly refreshUrl: HttpsUrl
  /** Time when the access token will expire, date and time in ISO 8601 format (UTC timezone). */
  readonly expireAt: Rfc3339DateTime
  /** App integration client ID. */
  readonly clientId: string
  /** The parent organization of the user. */
  readonly organizationId: OrganizationId | null
}

export interface User extends Auth0User {
  readonly email: string
  readonly organization_id?: OrganizationId
}

/**
 * Interface that represents Auth Provider API
 * Currently, it's tightly coupled with Cognito, but in the future, it should be decoupled from
 * Cognito and be able to be used with other Auth Providers.
 *
 * Currently used in unit tests to mock the Auth Provider API
 */
export interface SessionProvider {
  /** Resolves when ready. */
  readonly ready: Promise<void>
  /** Will refresh the session if it has expired. */
  readonly userSession: () => Promise<UserSession | null>
  readonly refreshUserSession: () => Promise<UserSession | null>
  readonly signUp: () => Promise<void>
  readonly signIn: () => Promise<void>
  readonly signOut: () => Promise<void>
}

/** Create a session provider backed by Auth0. */
export function createAuth0SessionProvider(): SessionProvider {
  const clientId = $config.AUTH0_CLIENT_ID ?? ''
  const client = new Auth0Client({
    domain: $config.AUTH0_DOMAIN ?? '',
    clientId,
    authorizationParams: {
      // eslint-disable-next-line camelcase
      redirect_uri: isOnElectron() ? 'enso://login' : location.href,
      scope: AUTH0_SCOPE,
      audience: AUTH0_AUDIENCE,
    },
  })
  const ready = client.checkSession()
  const computeUserSession = async (): Promise<UserSession | null> => {
    // @ts-expect-error This code accesses a private property with the correct type.
    const cacheManager = client.cacheManager as CacheManager
    const [user, claims, tokens, cache] = await Promise.all([
      client.getUser<User>(),
      client.getIdTokenClaims(),
      client.getTokenSilently({ detailedResponse: true }),
      cacheManager.get(new CacheKey({ scope: AUTH0_SCOPE, audience: AUTH0_AUDIENCE, clientId })),
    ])
    return !user || !claims ?
        null
      : {
          email: user.email,
          accessToken: tokens.access_token,
          refreshToken: cache?.refresh_token ?? '',
          refreshUrl: HttpsUrl(claims.iss ?? ''),
          expireAt: toRfc3339(new Date(claims.exp ?? 0)),
          clientId,
          organizationId: user.organization_id ?? null,
        }
  }

  return {
    ready,
    userSession: () => client.checkSession().then(computeUserSession),
    refreshUserSession: () => client.checkSession({ cacheMode: 'off' }).then(computeUserSession),
    signUp: () =>
      // eslint-disable-next-line camelcase
      client.loginWithPopup({ authorizationParams: { screen_hint: 'signup' } }),
    signIn: () => client.loginWithPopup(),
    signOut: () => client.logout({ logoutParams: { returnTo: 'enso://logout' } }),
  }
}
