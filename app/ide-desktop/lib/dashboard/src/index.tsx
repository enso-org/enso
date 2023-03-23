/** @file Index file declaring main DOM structure for the app. */

import * as authentication from 'enso-authentication'

import * as platform from './authentication/src/platform'

if (IS_DEV_MODE) {
    new EventSource('/esbuild').addEventListener('change', () => {
        location.reload()
    })
    void navigator.serviceWorker.register('/serviceWorker.js')
}

authentication.run({
    logger: console,
    // This file is only included when building for the cloud,
    // so it is safe to set `platform` to `cloud`.
    platform: platform.Platform.cloud,
    // The `onAuthenticated` parameter is required but we don't need it, so we pass an empty function.
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    onAuthenticated() {},
})
