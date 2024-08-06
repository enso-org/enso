/**
 * @file
 *
 * This file contains common schemas for authentication.
 */
import type { GetText } from '#/providers/TextProvider'
import { PASSWORD_REGEX } from '#/utilities/validation'
import { z } from 'zod'

/**
 * A schema for validating passwords.
 */
export function passwordSchema(getText: GetText) {
  return (
    z
      .string()
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      .min(6, { message: getText('passwordValidationError') })
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      .max(256, { message: getText('passwordValidationError') })
  )
}

/**
 * A schema for validating passwords that match the required pattern.
 */
export function passwordWithPatternSchema(getText: GetText) {
  return passwordSchema(getText).refine(
    (password) => PASSWORD_REGEX.test(password),
    getText('passwordValidationError'),
  )
}
