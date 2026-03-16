/** @file Backoff utilities for session recovery. */

export interface AuthRecoveryBackoffOptions {
  readonly maxAttempts: number
  readonly initialDelayMs: number
  readonly multiplier: number
  readonly maxDelayMs: number
  readonly jitter: number
}

export interface RepeatedUnauthorizedRecoveryBackoffOptions extends AuthRecoveryBackoffOptions {
  readonly resetWindowMs: number
}

function withJitter(delayMs: number, jitter: number) {
  if (jitter <= 0) {
    return delayMs
  }
  const jitterRange = delayMs * jitter
  const randomOffset = (Math.random() * 2 - 1) * jitterRange
  return Math.max(0, Math.round(delayMs + randomOffset))
}

/** Calculate the current and next delay for exponential backoff. */
export function nextBackoffDelay(delayMs: number, options: AuthRecoveryBackoffOptions) {
  const boundedDelayMs = Math.min(delayMs, options.maxDelayMs)
  return {
    delayMs: withJitter(boundedDelayMs, options.jitter),
    nextDelayMs: Math.min(options.maxDelayMs, Math.round(delayMs * options.multiplier)),
  }
}
