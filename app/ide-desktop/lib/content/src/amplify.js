// FIXME [NP]: https://github.com/enso-org/enso/pull/4041/files#r1091509001
//   1. Use typescript
//   2. Use eslint-compatible docs for typescript in all files
// DO THIS FOR ALL

// =====================
// === amplifyConfig ===
// =====================

// FIXME [NP]: replace these with the values for the production pool
//   Note to reviewers: this FIXME will be fixed before the PR goes in, but the production pool is
//   still being configured to send the correct emails, so we need to use the test pool for now.
const IDENTITY_POOL_ID = process.env.NEXT_IDENTITY_POOL_ID
// (required) - Amazon Cognito Region.
const REGION = process.env.REGION || 'us-east-1'
// (optional) - Amazon Cognito User Pool ID.
const USER_POOL_ID = process.env.NEXT_PUBLIC_AUTH_USER_POOL_ID || 'us-east-1_VcFZzGyhv'
// (optional) - Amazon Cognito Web Client ID (26-char alphanumeric string, App client secret needs
// to be disabled).
// FIXME [NP]: https://github.com/enso-org/enso/pull/4041/files#r1092210494
//   - 7vic1uoogbq4aq2rve897j0ep0 - magic
//   DO THIS FOR ALL
const USER_POOL_WEB_CLIENT_ID = process.env.NEXT_PUBLIC_AUTH_USER_POOL_WEB_CLIENT_ID || '7vic1uoogbq4aq2rve897j0ep0'
// (required) - Domain that hosts the OAuth endpoints for login, logout, and token refresh.
const DOMAIN = process.env.NEXT_PUBLIC_AUTH_DOMAIN || 'test-enso-pool.auth.us-east-1.amazoncognito.com/'
// (optional) - Redirect to this URL back from Cognito after the user signs in.
const REDIRECT_SIGN_IN = process.env.NEXT_PUBLIC_AUTH_REDIRECT_SIGN_IN || 'enso://localhost'
// (optional) - Redirect to this URL back from Cognito after the user signs out.
const REDIRECT_SIGN_OUT = process.env.NEXT_PUBLIC_AUTH_REDIRECT_SIGN_OUT || 'enso://localhost'
// (optional) - Response type of the OAuth flow. Replaces the default of 'token' since we want to be
// provided a refresh token.
const RESPONSE_TYPE = "code";

const GITHUB_ENDPOINT_NAME = 'GitHub'
const GITHUB_ENDPOINT_URL = 'https://py4rm53d36.execute-api.us-east-1.amazonaws.com/signin'

/// Configuration object to configure Amplify library, which provides our authentication
/// functionality via external OAuth providers (e.g. Google) through Amazon Cognito.
export const amplifyConfig = {
    region: REGION,
    identityPoolId: IDENTITY_POOL_ID,
    userPoolId: USER_POOL_ID,
    userPoolWebClientId: USER_POOL_WEB_CLIENT_ID,
    oauth: {
        options: {},
        domain: DOMAIN,
        scope: ['email', 'openid'],
        redirectSignIn: REDIRECT_SIGN_IN,
        redirectSignOut: REDIRECT_SIGN_OUT,
        responseType: RESPONSE_TYPE,
    }
};
