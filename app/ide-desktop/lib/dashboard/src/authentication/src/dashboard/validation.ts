/** @file Validation patterns for text inputs. */

/** Regex pattern for valid AWS Cognito passwords. */
export const PASSWORD_PATTERN =
    '(?=.*[0-9])(?=.*[A-Z])(?=.*[a-z])(?=.*[ ^$*.\\[\\]\\{\\}\\(\\)?"!@#%&\\/,><\':;\\|_~`=+\\-])' +
    '[0-9A-Za-z ^$*.\\[\\]\\{\\}\\(\\)?"!@#%&\\/,><\':;\\|_~`=+\\-]{6,256}'
/** Human readable explanation of password requirements. */
export const PASSWORD_TITLE =
    'Your password must include numbers, letters (both lowercase and uppercase) and symbols, ' +
    'and must be between 6 and 256 characters long.'

/** Regex pattern used by the backend for validating the previous password,
 * when changing password. */
export const PREVIOUS_PASSWORD_PATTERN = '^[\\S]+.*[\\S]+$'
/** Human readable explanation of password requirements. */
export const PREVIOUS_PASSWORD_TITLE =
    'Your password must neither start nor end with whitespace, and must contain ' +
    'at least two characters.'

// The Project Manager has restrictions on names of projects.
/** Regex pattern for valid names for local projects.
 * @see https://github.com/enso-org/enso/blob/72ec775d8cf46b1862884fe7905477354943f0a5/lib/scala/pkg/src/main/scala/org/enso/pkg/validation/NameValidation.scala#L37
 */
export const LOCAL_PROJECT_NAME_PATTERN = '[A-Z]+[a-z]*(?:_\\d+|_[A-Z]+[a-z]*)*'
/** Human readable explanation of project name restrictions for local projects. */
export const LOCAL_PROJECT_NAME_TITLE =
    'Project names must be in Upper_Snake_Case. (Numbers (_0, _1) are also allowed.)'
