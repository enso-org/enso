/**
 * Check that value is a number other than NaN.
 */
function isValidNumber(value) {
    return typeof value === 'number' && Number.isNaN(value) === false
}
