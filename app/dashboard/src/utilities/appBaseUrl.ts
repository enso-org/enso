/** @file The base URL of the app. */
import { ALL_PATHS_REGEX } from '#/appUtils'

/** The base URL of the app. */
export const APP_BASE_URL = location.pathname.replace(ALL_PATHS_REGEX, '')
