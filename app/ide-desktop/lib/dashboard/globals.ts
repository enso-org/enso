/** @file An object containing globals to inject. */

/** The value as JSON if it is not nullish, else `'undefined'`. */
function stringify(string: unknown) {
  return string == null ? 'undefined' : JSON.stringify(string)
}

/** Return an object containing globals to inject. */
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
export function globals(devMode: boolean, serverPort = 8080) {
  return {
    /* eslint-disable @typescript-eslint/naming-convention */
    // Whether the application is being run locally. This determines whether some variables are
    // exposed to the global scope.
    'process.env.NODE_ENV': stringify(devMode ? 'development' : 'production'),
    'process.env.CLOUD_REDIRECT': stringify(
      // The actual environment variable does not necessarily exist.
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
      process.env.CLOUD_REDIRECT ?? `http://localhost:${serverPort}`
    ),
    // The actual environment variable does not necessarily exist.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    'process.env.CLOUD_ENVIRONMENT': stringify(process.env.CLOUD_ENVIRONMENT ?? 'production'),
    'process.env.API_URL': stringify(process.env.API_URL),
    'process.env.CHAT_URL': stringify(process.env.CHAT_URL),
    'process.env.AMPLIFY_USER_POOL_ID': stringify(process.env.AMPLIFY_USER_POOL_ID),
    'process.env.AMPLIFY_USER_POOL_WEB_CLIENT_ID': stringify(
      process.env.AMPLIFY_USER_POOL_WEB_CLIENT_ID
    ),
    'process.env.AMPLIFY_DOMAIN': stringify(process.env.AMPLIFY_DOMAIN),
    'process.env.AMPLIFY_REGION': stringify(process.env.AMPLIFY_REGION ?? 'eu-west-1'),
    /* eslint-enable @typescript-eslint/naming-convention */
  }
}
