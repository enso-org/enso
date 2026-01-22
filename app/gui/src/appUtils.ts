/** @file Constants related to the application root component. */

export const ORGANIZATION_NAME_MIN_LENGTH = 3
export const ORGANIZATION_NAME_MAX_LENGTH = 64
export const USER_GROUP_NAME_MAX_LENGTH = 64

export const OPEN_IDE_DEEPLINK = `enso://`

/** Path to the root of the app (i.e., the Cloud dashboard). */
export const DASHBOARD_PATH = '/'
/** Path to the login page. */
export const LOGIN_PATH = '/login'
/** Path to the registration page. */
export const REGISTRATION_PATH = '/registration'
/** Path to the payments success page. */
export const PAYMENTS_SUCCESS_PATH = '/payments/success'
/** Path to the confirm registration page. */
export const CONFIRM_REGISTRATION_PATH = '/confirmation'
/**
 * Path to the page in which a user can restore their account after it has been
 * marked for deletion.
 */
export const RESTORE_USER_PATH = '/restore-user'
/** Path to the forgot password page. */
export const FORGOT_PASSWORD_PATH = '/forgot-password'
/** Path to the reset password page. */
export const RESET_PASSWORD_PATH = '/password-reset'
/** Path to page in which the currently active payment plan can be managed. */
export const SUBSCRIBE_PATH = '/subscribe'
/** A {@link RegExp} matching all paths. */
export const ALL_PATHS_REGEX = new RegExp(
  `(?:${DASHBOARD_PATH}|${LOGIN_PATH}|${REGISTRATION_PATH}|${CONFIRM_REGISTRATION_PATH}|` +
    `${FORGOT_PASSWORD_PATH}|${RESET_PASSWORD_PATH}|${RESTORE_USER_PATH}|${SUBSCRIBE_PATH})$`,
)

// === Constants related to URLs ===

export const SEARCH_PARAMS_PREFIX = 'cloud-ide_'
/** Return the email address for contacting support. */
export const SUPPORT_EMAIL = 'cloud@enso.org'
/** Return the `mailto:` URL for contacting support. */
export const SUPPORT_EMAIL_URL = `mailto:${SUPPORT_EMAIL}`

/** Build a Subscription URL for a given plan. */
export function getUpgradeURL(plan: string): string {
  return SUBSCRIBE_PATH + '?plan=' + plan
}

/** Return url address of Enso Analytics contact page. */
export function getContactPage(): string {
  return `${$config.ENSO_HOST}/contact`
}

/** Return the mailto URL for contacting sales. */
export function getSalesEmail(): string {
  return 'mailto:contact@enso.org'
}
