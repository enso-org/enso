module.exports = {
    extends: [
        'eslint:recommended',
        'plugin:@typescript-eslint/recommended',
        // "plugin:@typescript-eslint/recommended-requiring-type-checking"
    ],
    parser: '@typescript-eslint/parser',
    plugins: ['@typescript-eslint'],
    root: true,
    rules: {
        '@typescript-eslint/no-explicit-any': 'off',
        '@typescript-eslint/no-var-requires': 'off',
        '@typescript-eslint/no-this-alias': 'off',
        '@typescript-eslint/ban-ts-comment': 'off',
        "@typescript-eslint/naming-convention": "error",
    },
    // overrides: [
    //     {
    //         files: ['*.ts', '*.tsx'],
    //         parserOptions: {
    //             project: ['./tsconfig.json']
    //         },
    //     }
    // ]
};
