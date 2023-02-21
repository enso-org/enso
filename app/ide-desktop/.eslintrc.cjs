/** @file configuration eslint file */
/* eslint-disable @typescript-eslint/naming-convention */

// TODO: Some of the config options are commented out because eslint is not configured properly.
// This will be fixed in the future.
const rules = {
    '@typescript-eslint/no-explicit-any': 'off',
    '@typescript-eslint/no-var-requires': 'off',
    '@typescript-eslint/no-this-alias': 'off',
    '@typescript-eslint/ban-ts-comment': 'off',
    '@typescript-eslint/no-empty-function': 'off',
    '@typescript-eslint/no-non-null-assertion': 'off',
    '@typescript-eslint/require-await': 'off',
    '@typescript-eslint/naming-convention': [
        'error',
        {
            selector: 'objectLiteralProperty',
            format: ['camelCase', 'snake_case', 'UPPER_CASE'],
            filter: {
                // filter is kebab-case
                regex: '[a-z]+(-[a-z]*)*',
                match: false,
            },
        },
    ],
    // '@typescript-eslint/no-unnecessary-condition': 'error',
    // '@typescript-eslint/restrict-template-expressions': [
    //     'error',
    //     {
    //         allowAny: true,
    //         allowBoolean: true,
    //     },
    // ],
    'jsdoc/require-param-type': 'off',
    'jsdoc/check-access': 'warn',
    'jsdoc/check-alignment': 'warn',
    'jsdoc/check-indentation': 'warn',
    'jsdoc/check-line-alignment': 'warn',
    'jsdoc/check-param-names': 'warn',
    'jsdoc/check-property-names': 'warn',
    'jsdoc/check-syntax': 'warn',
    'jsdoc/check-tag-names': 'warn',
    'jsdoc/check-types': 'warn',
    'jsdoc/check-values': 'warn',
    'jsdoc/empty-tags': 'warn',
    'jsdoc/implements-on-classes': 'warn',
    'jsdoc/no-bad-blocks': 'warn',
    'jsdoc/no-defaults': 'warn',
    'jsdoc/no-multi-asterisks': 'warn',
    'jsdoc/no-types': 'warn',
    'jsdoc/no-undefined-types': 'warn',
    'jsdoc/require-asterisk-prefix': 'warn',
    'jsdoc/require-description': 'warn',
    // 'jsdoc/require-description-complete-sentence': 'warn,
    'jsdoc/require-file-overview': 'warn',
    'jsdoc/require-hyphen-before-param-description': 'warn',
    'jsdoc/require-jsdoc': 'warn',
    'jsdoc/require-param-description': 'warn',
    'jsdoc/require-param-name': 'warn',
    'jsdoc/require-property': 'warn',
    'jsdoc/require-property-description': 'warn',
    'jsdoc/require-property-name': 'warn',
    'jsdoc/require-property-type': 'warn',
    'jsdoc/require-returns-check': 'warn',
    'jsdoc/require-returns-description': 'warn',
    'jsdoc/require-throws': 'warn',
    'jsdoc/require-yields': 'warn',
    'jsdoc/require-yields-check': 'warn',
    'jsdoc/tag-lines': 'warn',
    'jsdoc/valid-types': 'warn',
}
module.exports = {
    env: {
        browser: true,
        node: true,
        es6: true,
    },
    extends: ['eslint:recommended', 'plugin:@typescript-eslint/recommended'],
    parser: '@typescript-eslint/parser',
    parserOptions: {
        tsconfigRootDir: __dirname,
        project: true,
    },
    plugins: ['jsdoc', '@typescript-eslint'],
    root: true,
    rules,
    overrides: [
        {
            files: ['*.{ts,tsx}'],
            extends: [
                'plugin:@typescript-eslint/recommended-requiring-type-checking',
                'plugin:@typescript-eslint/strict',
            ],
            rules,
        },
    ],
}
