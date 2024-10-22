/** @file Constants related to the application root component. */

// =================
// === Constants ===
// =================

// === Paths ===

/** Path to the root of the app (i.e., the Cloud dashboard). */
export const DASHBOARD_PATH = '/'
/** Path to the login page. */
export const LOGIN_PATH = '/login'
/** Path to the registration page. */
export const REGISTRATION_PATH = '/registration'
/** Path to the confirm registration page. */
export const CONFIRM_REGISTRATION_PATH = '/confirmation'

export const SETUP_PATH = '/setup'
/**
 * Path to the page in which a user can restore their account after it has been
 * marked for deletion.
 */
export const RESTORE_USER_PATH = '/restore-user'
/** Path to the forgot password page. */
export const FORGOT_PASSWORD_PATH = '/forgot-password'
/** Path to the reset password page. */
export const RESET_PASSWORD_PATH = '/password-reset'
/** Path to the set username page. */
/** Path to the offline mode entrypoint. */
/** Path to page in which the currently active payment plan can be managed. */
export const SUBSCRIBE_PATH = '/subscribe'
export const SUBSCRIBE_SUCCESS_PATH = '/subscribe/success'
/** A {@link RegExp} matching all paths. */
export const ALL_PATHS_REGEX = new RegExp(
  `(?:${DASHBOARD_PATH}|${LOGIN_PATH}|${REGISTRATION_PATH}|${CONFIRM_REGISTRATION_PATH}|` +
    `${FORGOT_PASSWORD_PATH}|${RESET_PASSWORD_PATH}|${RESTORE_USER_PATH}|` +
    `${SUBSCRIBE_PATH}|${SUBSCRIBE_SUCCESS_PATH}|${SETUP_PATH})$`,
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

/** Return the mailto URL for contacting sales. */
export function getSalesEmail(): string {
  return 'mailto:contact@enso.org'
}

/** Build a Subscription URL for contacting sales. */
export function getContactSalesURL(): string {
  return 'mailto:contact@enso.org?subject=Upgrading%20to%20Organization%20Plan'
}
