/** @file ESLint configuration file. */

module.exports = {
    env: {
        browser: true,
        node: true,
        es6: true,
    },
    extends: [
        'eslint:recommended',
        'plugin:@typescript-eslint/recommended',
        'plugin:@typescript-eslint/recommended-requiring-type-checking',
        'plugin:@typescript-eslint/strict',
    ],
    parser: '@typescript-eslint/parser',
    parserOptions: {
        tsconfigRootDir: __dirname,
        project: true,
    },
    plugins: ['jsdoc', '@typescript-eslint'],
    root: true,
    rules: {
        /* eslint-disable @typescript-eslint/naming-convention */
        'eqeqeq': 'error',
        'sort-imports': [
            'error',
            { allowSeparatedGroups: true },
        ],
        'no-restricted-syntax': [
            'error',
            {
                selector: ':matches(ImportDeclaration:has(ImportSpecifier), ExportDeclaration, ExportSpecifier)',
                message: 'No {} imports and exports',
            },
            {
                selector: 'ImportDeclaration[source.value=/^[^.]/] > ImportDefaultSpecifier',
                message: 'No default imports from modules',
            },
            // FIXME[sb]
            // {
            //     selector: 'ImportNamespaceSpecifier > Identifier:not([name=/^[A-Z_]+$/])',
            //     message: 'Use `CONSTANT_CASE` for imports from data files',
            // },
            {
                selector: 'ImportNamespaceSpecifier > Identifier:not([name=/^[a-z]+(?:[A-Z][a-z]*)*$/])',
                message: 'Use `camelCase` for imports',
            },
            {
                selector: 'ExportAllDeclaration',
                message: 'No re-exports',
            },
            {
                selector: 'TSTypeLiteral',
                message: 'No object types - use interfaces instead',
            },
            {
                selector: '[declare=true]',
                message: 'No ambient declarations',
            },
            {
                selector: 'TSTypeAliasDeclaration > TSTypeReference > Identifier',
                message: 'No renamed types',
            },
            {
                selector: 'TSTypeAliasDeclaration > :matches(TSLiteralType)',
                message: 'No aliases to literal types',
            },
            {
                selector: 'TSTypeAliasDeclaration > :matches(TSBooleanKeyword, TSBigintKeyword, TSNullKeyword, TSNumberKeyword, TSObjectKeyword, TSStringKeyword, TSSymbolKeyword, TSUndefinedKeyword, TSUnknownKeyword, TSVoidKeyword)',
                message: 'No aliases to primitives - consider using brands instead: `string & { _brand: "BrandName"; }`',
            },
            {
                selector: ':matches(Program, ExportNamedDeclaration) > VariableDeclaration[kind=const] > * > ObjectExpression:has(Property > Literal[raw=/^["\']/].value):not(:has(Property > .value:not(Literal[raw=/^["\']/])))',
                message: 'Use `as const` for object literals only containing string literals',
            },
            {
                selector: 'ExportNamedDeclaration > VariableDeclaration[kind=let]',
                message: 'Use `export const` instead of `export let`',
            },
            {
                selector: 'Program > VariableDeclaration[kind=let] > * > ObjectExpression:has(Property > Literal[raw=/^["\']/].value):not(:has(Property > .value:not(Literal[raw=/^["\']/])))',
                message: 'Use `const` instead of `let` for object literals only containing string literals',
            },
            {
                selector: 'ImportDeclaration[source.value=/^(?:assert|async_hooks|buffer|child_process|cluster|console|constants|crypto|dgram|diagnostics_channel|dns|domain|events|fs|fs\\u002Fpromises|http|http2|https|inspector|module|net|os|path|perf_hooks|process|punycode|querystring|readline|repl|stream|string_decoder|timers|tls|trace_events|tty|url|util|v8|vm|wasi|worker_threads|zlib)$/]',
                message: 'Use `node:` prefix to import builtin node modules',
            },
            {
                selector: 'ImportDeclaration[source.value=/^(?!node:)/] ~ ImportDeclaration[source.value=/^node:/]',
                message: 'Import node modules before npm modules and relative imports, separated by a blank line',
            },
            {
                selector: 'ImportDeclaration[source.value=/^\\./] ~ ImportDeclaration[source.value=/^[^.]/]',
                message: 'Import npm modules before relative imports, separated by a blank line',
            },
            {
                // FIXME[sb]: disallow `constructor(public entries: (readonly [cmdOption: string, option: config.Option<T>])[] = []) {}`
                selector: 'FunctionDeclaration > .params TupleType',
                message: 'No tuples in type annotations',
            },
        ],
        // TODO[sb]: import ordering
        // TODO[sb]: decide between export function vs export const arrow function
        // TODO[sb]: consistent key style - when to quote properties
        // prefer interface over type
        '@typescript-eslint/consistent-type-definitions': 'error',
        '@typescript-eslint/consistent-type-imports': ['error', { prefer: 'no-type-imports' }],
        '@typescript-eslint/member-ordering': 'error',
        // Method syntax is not type-safe.
        // See: https://typescript-eslint.io/rules/method-signature-style
        '@typescript-eslint/method-signature-style': 'error',
        '@typescript-eslint/naming-convention': [
            'error',
            {
                selector: ['function'],
                // PascalCase for React components, camelCase for all other situations
                format: ['camelCase', 'PascalCase'],
            },
            {
                selector: ['variable'],
                modifiers: ['const', 'global'],
                format: ['UPPER_CASE'],
            },
            {
                selector: ['variable'],
                modifiers: ['const', 'exported'],
                format: ['UPPER_CASE'],
            },
            {
                selector: ['variable', 'parameter', 'property', 'method'],
                modifiers: ['unused'],
                format: ['camelCase'],
                leadingUnderscore: 'require',
            },
            {
                selector: ['variable', 'parameter', 'property', 'method'],
                format: ['camelCase'],
            },
        ],
        '@typescript-eslint/no-confusing-void-expression': 'error',
        '@typescript-eslint/no-extraneous-class': 'error',
        // React 17 and later supports async functions as event handlers, so we need to disable this
        // rule to avoid false positives.
        //
        // See: https://github.com/typescript-eslint/typescript-eslint/pull/4623
        '@typescript-eslint/no-misused-promises': [
            'error',
            { checksVoidReturn: { attributes: false } },
        ],
        '@typescript-eslint/no-redundant-type-constituents': 'error',
        '@typescript-eslint/no-unnecessary-condition': 'error',
        '@typescript-eslint/no-useless-empty-export': 'error',
        '@typescript-eslint/parameter-properties': ['error', { prefer: 'parameter-property' }],
        '@typescript-eslint/prefer-enum-initializers': 'error',
        '@typescript-eslint/prefer-readonly': 'error',
        '@typescript-eslint/require-array-sort-compare': [
            'error',
            { ignoreStringArrays: true },
        ],
        '@typescript-eslint/restrict-template-expressions': 'error',
        '@typescript-eslint/sort-type-constituents': 'error',
        '@typescript-eslint/switch-exhaustiveness-check': 'error',
        'default-param-last': 'off',
        '@typescript-eslint/default-param-last': 'error',
        'no-invalid-this': 'off',
        '@typescript-eslint/no-invalid-this': ['error', { capIsConstructor: false }],
        'jsdoc/no-magic-numbers': 'off',
        '@typescript-eslint/no-magic-numbers': [
            'error',
            {
                ignore: [0, 1, 2],
                ignoreArrayIndexes: true,
                ignoreEnums: true,
                detectObjects: true,
                enforceConst: true,
            },
        ],
        'no-redeclare': 'off',
        // Important to warn on accidental duplicated `interface`s e.g. when writing API wrappers.
        '@typescript-eslint/no-redeclare': 'error',
        'no-shadow': 'off',
        '@typescript-eslint/no-shadow': 'warn',
        'no-unused-expressions': 'off',
        '@typescript-eslint/no-unused-expressions': 'error',
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
        // 'jsdoc/no-multi-asterisks': 'warn',
        'jsdoc/no-types': 'warn',
        'jsdoc/no-undefined-types': 'warn',
        'jsdoc/require-asterisk-prefix': 'warn',
        'jsdoc/require-description': 'warn',
        // 'jsdoc/require-description-complete-sentence': 'warn,
        'jsdoc/require-file-overview': 'warn',
        'jsdoc/require-hyphen-before-param-description': 'warn',
        'jsdoc/require-jsdoc': [
            'off', // FIXME[sb]: 'error',
            {
                'require': {
                    // We should enforce this for let and const too,
                    // but there doesn't seem to be options for that.
                    'FunctionDeclaration': true,
                    'MethodDefinition': true,
                    'ClassDeclaration': true,
                    'ArrowFunctionExpression': true,
                    'FunctionExpression': true,
                },
            },
        ],
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
        /* eslint-enable @typescript-eslint/naming-convention */
    },
    overrides: [
        {
            files: ['*.js', '*.jsx', '*.cjs', '*.mjs'],
            rules: {
                /* eslint-disable @typescript-eslint/naming-convention */
                '@typescript-eslint/no-var-requires': 'off',
                // Parameter types must be specified using JSDoc in JS files.
                'jsdoc/no-types': 'off',
                /* eslint-enable @typescript-eslint/naming-convention */
            }
        },
        {
            files: ['*.d.ts'],
            rules: {
                /* eslint-disable @typescript-eslint/naming-convention */
                // 
                'no-restricted-syntax': 'off',
                /* eslint-enable @typescript-eslint/naming-convention */
            }
        },
    ]
}
