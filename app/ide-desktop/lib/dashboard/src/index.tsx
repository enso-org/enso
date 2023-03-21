/** @file Index file declaring main DOM structure for the app. */

import * as authentication from 'enso-authentication'

import * as platform from './authentication/src/platform'

const logger = console
/** This package is a standalone React app (i.e., IDE deployed to the Cloud), so we're not
 * running on the desktop. */
const PLATFORM = platform.Platform.cloud
// The `onAuthenticated` parameter is required but we don't need it, so we pass an empty function.
// eslint-disable-next-line @typescript-eslint/no-empty-function
function onAuthenticated() {}

authentication.run(logger, PLATFORM, onAuthenticated)
