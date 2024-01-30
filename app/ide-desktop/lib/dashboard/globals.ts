/** @file An object containing globals to inject. */

/** Return an object containing globals to inject. */
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
export function globals(devMode: boolean, serverPort = 8080) {
  return {
    /* eslint-disable @typescript-eslint/naming-convention */
    // Whether the application is being run locally. This determines whether some variables are
    // exposed to the global scope.
    'process.env.NODE_ENV': JSON.stringify(devMode ? 'development' : 'production'),
    CLOUD_REDIRECT: JSON.stringify(
      process.env.ENSO_CLOUD_REDIRECT ??
        (devMode ? `http://localhost:${serverPort}` : 'https://cloud.enso.org')
    ),
    CLOUD_ENVIRONMENT: JSON.stringify(process.env.ENSO_CLOUD_ENVIRONMENT ?? 'production'),
    API_URL: JSON.stringify(
      process.env.ENSO_API_URL ?? 'https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com'
    ),
    CHAT_URL: JSON.stringify(
      process.env.ENSO_API_URL ??
        (devMode ? 'https://localhost:8082' : 'https://chat.cloud.enso.org')
    ),
    AMPLIFY_USER_POOL_ID: JSON.stringify(
      process.env.ENSO_AMPLIFY_USER_POOL_ID ?? 'eu-west-1_9Kycu2SbD'
    ),
    AMPLIFY_USER_POOL_WEB_CLIENT_ID: JSON.stringify(
      process.env.ENSO_AMPLIFY_USER_POOL_WEB_CLIENT_ID ?? '4j9bfs8e7415erf82l129v0qhe'
    ),
    AMPLIFY_DOMAIN: JSON.stringify(
      process.env.ENSO_AMPLIFY_DOMAIN ?? 'production-enso-domain.auth.eu-west-1.amazoncognito.com'
    ),
    AMPLIFY_REGION: JSON.stringify(process.env.ENSO_AMPLIFY_REGION ?? 'eu-west-1'),
    /* eslint-enable @typescript-eslint/naming-convention */
  }
}
