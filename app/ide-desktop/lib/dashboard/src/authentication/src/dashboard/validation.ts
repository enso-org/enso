/** @file Validation patterns for text inputs. */

/** Regex pattern for valid AWS Cognito passwords. */
export const PASSWORD_PATTERN =
    '(?=.*[0-9])(?=.*[A-Z])(?=.*[a-z])(?=.*[ ^$*.\\[\\]\\{\\}\\(\\)?"!@#%&\\/,><\':;\\|_~`=+\\-])' +
    '[0-9A-Za-z ^$*.\\[\\]\\{\\}\\(\\)?"!@#%&\\/,><\':;\\|_~`=+\\-]{6,256}'
/** Human readable explanation of password requirements. */
export const PASSWORD_TITLE =
    'Your password must include numbers, letters (both lowercase and uppercase) and symbols, ' +
    'and must be between 6 and 256 characters long.'
