/** @file The base URL of the app. */
import * as appUtils from '#/appUtils'

/** The base URL of the app. */
export const APP_BASE_URL = location.pathname.replace(appUtils.ALL_PATHS_REGEX, '')
