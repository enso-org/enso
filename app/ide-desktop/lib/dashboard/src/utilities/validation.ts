/** @file Validation patterns for text inputs. */

// ==================
// === Validation ===
// ==================

/** Regex pattern for valid AWS Cognito passwords.
 * A fully correct regex is here: https://stackoverflow.com/a/58767981/3323231.
 * Official documentation is here: https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-policies.html.
 * However, non-ASCII passwords are allowed, contrary to the official documentation. Further
 * investigation may be needed.
 *
 * Each of the four lookaheads in the regex below check for, respectively:
 * - a digit
 * - a Basic Latin uppercase character
 * - a Basic Latin lowercase character, and
 * - an ASCII symbol.
 */
export const PASSWORD_PATTERN =
  '(?=.*[0-9])(?=.*[A-Z])(?=.*[a-z])(?=.*[ ^$*.\\[\\]\\{\\}\\(\\)?"!@#%&\\/,><\':;\\|_~`=+\\-]).{6,256}'
/** Human readable explanation of password requirements. */
export const PASSWORD_ERROR =
  'Your password must include numbers, letters (both lowercase and uppercase) and symbols, ' +
  'and must be between 6 and 256 characters long.'

export const CONFIRM_PASSWORD_ERROR = 'Passwords must match.'

// The Project Manager has restrictions on names of projects.
/** Regex pattern for valid names for local projects.
 *
 * Validation rules:
 * - allow any non-empty string
 */
export const LOCAL_PROJECT_NAME_PATTERN = '.*\\S.*'
/** Human readable explanation of project name restrictions for local projects. */
export const LOCAL_PROJECT_NAME_TITLE = 'Project name cannot be empty.'
