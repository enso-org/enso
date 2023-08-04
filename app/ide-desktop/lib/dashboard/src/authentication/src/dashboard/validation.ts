/** @file Validation patterns for text inputs. */

/** Regex pattern for valid AWS Cognito passwords. */
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
