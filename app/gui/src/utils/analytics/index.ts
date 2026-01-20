import * as gtag from '$/utils/analytics/gtag'
import * as detect from 'enso-common/src/utilities/detect'

export const createUser = {
  /** Log successful user creation. */
  after: () => gtag.event('cloud_user_created'),
}

export const cloudSignOut = {
  /** Log a successful sign-out. */
  after: () => gtag.event('cloud_sign_out'),
}

/**
 * Account creation.
 *
 * The expected sequence of events is:
 *     cloud_sign_up -> cloud_sign_up_confirm -> cloud_sign_up_confirm_success
 */
export const cloudSignUp = {
  /** Log the initiation of a sign-up attempt. */
  before: () => gtag.event('cloud_sign_up'),
  /** Email-address confirmation. */
  confirm: {
    /** Log an attempt to confirm sign-up. */
    before: () => gtag.event('cloud_sign_up_confirm'),
    /** Log a successfully confirmed sign-up. */
    after: () => gtag.event('cloud_sign_up_confirm_success'),
  },
}

export type AuthProvider = 'Email' | 'Apple' | 'Google' | 'GitHub' | 'Microsoft'

/**
 * Cloud sign-in.
 *
 * Depending on the user's authentication method, the expected events may be one of:
 *     cloud_sign_in -> cloud_sign_in_success
 *     cloud_sign_in
 *       -> cloud_sign_in_confirm_expected
 *       -> cloud_sign_in_confirm
 *       -> cloud_sign_in_success
 */
export const signIn = {
  /** Log initiation of a sign-in attempt. */
  before: (provider: AuthProvider) => gtag.event('cloud_sign_in', { provider }),
  confirm: {
    /** Log when asking the user for a code to confirm sign-in. */
    expected: (challenge: string) => gtag.event('cloud_sign_in_confirm_expected', { challenge }),
    /** Log when attempting sign-in confirmation. */
    before: () => gtag.event('cloud_sign_in_confirm'),
  },
  /** Log a completed sign-in. */
  after: () => gtag.event('cloud_sign_in_success'),
}

interface PlanInfo {
  price: string
  quantity: number
  interval: number
}

export const checkout = {
  /** Log a checkout window being opened. */
  before: (selectedPlan: PlanInfo) => gtag.event('checkout', selectedPlan),
  /** Log a completed plan change. */
  after: () => gtag.event('checkout_success'),
}

/** Log when the app is opened and closed. */
export function appOpenCloseCallback(): () => void {
  gtag.gtag('set', { platform: detect.platform(), architecture: detect.architecture() })
  return gtag.openCloseCallback('open_app', 'close_app')
}

/** Log when a graph editor instance is opened and closed. */
export function editorOpenCloseCallback(): () => void {
  return gtag.openCloseCallback('open_workflow', 'close_workflow')
}
