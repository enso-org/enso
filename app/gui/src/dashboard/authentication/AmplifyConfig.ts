/** @file Configuration for the AWS Amplify library. */
import { AccessToken } from 'enso-common/src/accessToken'

/**
 * Configuration for the AWS Amplify library.
 *
 * This details user pools, federated identity providers, etc. that are used to authenticate users.
 * The values in this object are not secret, and can be swapped out for testing values to avoid
 * creating authenticated users in the production environment.
 */
export interface AmplifyConfig {
  readonly region: string
  readonly endpoint: string | undefined
  readonly userPoolId: string
  readonly userPoolWebClientId: string
  readonly urlOpener: ((url: string, redirectUrl: string) => void) | null
  readonly saveAccessToken: ((accessToken: AccessToken | null) => void) | null
  readonly domain: string
  readonly scope: string[]
  readonly redirectSignIn: string
  readonly redirectSignOut: string
  readonly responseType: string
}

/** Configuration options for a {@link OauthAmplifyConfig}. */
interface OauthAmplifyConfigOptions {
  readonly urlOpener?: (url: string, redirectUrl: string) => void
}

/** OAuth configuration for a {@link NestedAmplifyConfig}. */
interface OauthAmplifyConfig {
  readonly options: OauthAmplifyConfigOptions
  readonly domain: string
  readonly scope: string[]
  readonly redirectSignIn: string
  readonly redirectSignOut: string
  readonly responseType: string
}

/** Same as {@link AmplifyConfig}, but in a format recognized by the AWS Amplify library. */
export interface NestedAmplifyConfig {
  readonly region: string
  readonly endpoint: string | undefined
  readonly userPoolId: string
  readonly userPoolWebClientId: string
  readonly oauth: OauthAmplifyConfig
}

/**
 * Convert the flattened `AmplifyConfig` struct to a form recognizable to the AWS Amplify library.
 *
 * We use a flattened form of the config for easier object manipulation, but the AWS Amplify library
 * expects a nested form.
 */
export function toNestedAmplifyConfig(config: AmplifyConfig): NestedAmplifyConfig {
  return {
    region: config.region,
    // endpoint: config.endpoint,
    // TODO: Use the endpoint when it is working.
    endpoint: undefined,
    userPoolId: config.userPoolId,
    userPoolWebClientId: config.userPoolWebClientId,
    oauth: {
      options: config.urlOpener ? { urlOpener: config.urlOpener } : {},
      domain: config.domain,
      scope: config.scope,
      redirectSignIn: config.redirectSignIn,
      redirectSignOut: config.redirectSignOut,
      responseType: config.responseType,
    },
  }
}
