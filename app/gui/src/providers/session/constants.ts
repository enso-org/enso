/** @file Constants for the session provider. */
import type {
  AuthRecoveryBackoffOptions,
  RepeatedUnauthorizedRecoveryBackoffOptions,
} from './backoff'

export const LOGOUT_EVENT = 'enso-logout'

export const AUTH_RECOVERY_BACKOFF_DEFAULTS: AuthRecoveryBackoffOptions = {
  maxAttempts: 4,
  initialDelayMs: 300,
  multiplier: 2,
  maxDelayMs: 5000,
  jitter: 0.2,
}

export const REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS: RepeatedUnauthorizedRecoveryBackoffOptions =
  {
    maxAttempts: 3,
    initialDelayMs: 300,
    multiplier: 2,
    maxDelayMs: 5000,
    jitter: 0.2,
    resetWindowMs: 30_000,
  }

export const RECONNECTING_SESSION_DELAY_MS = 1000
