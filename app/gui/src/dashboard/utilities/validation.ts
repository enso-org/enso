/** @file Validation patterns for text inputs. */

// ==================
// === Validation ===
// ==================

/**
 * Regex pattern for valid AWS Cognito passwords.
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
export const PASSWORD_REGEX = new RegExp('^' + PASSWORD_PATTERN + '$')

// The Project Manager has restrictions on names of projects.
/**
 * Regex pattern for valid names for local projects.
 *
 * Validation rules:
 * - allow any non-empty string
 */
export const LOCAL_PROJECT_NAME_PATTERN = '.*\\S.*'

/**
 * Match only valid names for titles. The following substrings are disallowed:
 * - `/` - folder separator (non-Windows)
 * - `\` - folder separator (Windows)
 * - `..` - parent directory
 */
export const DIRECTORY_NAME_REGEX = /^(?:[^/\\.]|[.](?=[^.]|$))+$/
