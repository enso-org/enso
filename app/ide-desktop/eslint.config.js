/** @file ESLint configuration file. */
/** NOTE: The "Experimental: Use Flat Config" option must be enabled.
 * Flat config is still not quite mature, so is disabled by default. */
import * as path from 'node:path'
import * as url from 'node:url'

// We prefer `import * as name`, however these modules do not support it.
// This is specialcased in other files, but these modules shouldn't be used in other files anyway.
/* eslint-disable no-restricted-syntax */
import eslintJs from '@eslint/js'
import globals from 'globals'
import jsdoc from 'eslint-plugin-jsdoc'
import tsEslint from '@typescript-eslint/eslint-plugin'
import tsEslintParser from '@typescript-eslint/parser'
/* eslint-enable no-restricted-syntax */

const DIR_NAME = path.dirname(url.fileURLToPath(import.meta.url))
const NAME = 'enso'
/** An explicit whitelist of CommonJS modules, which do not support namespace imports.
 * Many of these have incorrect types, so no type error may not mean they support ESM,
 * and conversely type errors may not mean they don't support ESM -
 * but we add those to the whitelist anyway otherwise we get type errors.
 * In particular, `string-length` supports ESM but its type definitions don't.
 * `yargs` and `react-hot-toast` are modules we explicitly want the default imports of.
 * `node:process` is here because `process.on` does not exist on the namespace import. */
const DEFAULT_IMPORT_ONLY_MODULES =
    'node:process|chalk|string-length|yargs|yargs\\u002Fyargs|sharp|to-ico|connect|morgan|serve-static|create-servers|electron-is-dev|fast-glob|esbuild-plugin-.+|opener|tailwindcss.*'
const ALLOWED_DEFAULT_IMPORT_MODULES = `${DEFAULT_IMPORT_ONLY_MODULES}|postcss|react-hot-toast`
const OUR_MODULES = 'enso-content-config|enso-common'
const RELATIVE_MODULES =
    'bin\\u002Fproject-manager|bin\\u002Fserver|config\\u002Fparser|authentication|config|debug|file-associations|index|ipc|naming|paths|preload|security|url-associations'
const STRING_LITERAL = ':matches(Literal[raw=/^["\']/], TemplateLiteral)'
const JSX = ':matches(JSXElement, JSXFragment)'
const NOT_PASCAL_CASE = '/^(?!_?([A-Z][a-z0-9]*)+$)/'
const NOT_CAMEL_CASE = '/^(?!_?[a-z][a-z0-9*]*([A-Z0-9][a-z0-9]*)*$)/'
const WHITELISTED_CONSTANTS = 'logger|.+Context'
const NOT_CONSTANT_CASE = `/^(?!${WHITELISTED_CONSTANTS}$|_?[A-Z][A-Z0-9]*(_[A-Z0-9]+)*$)/`
const WITH_ROUTER = 'CallExpression[callee.name=withRouter]'

// Extracted to a variable because it needs to be used twice:
// - once as-is for `.d.ts`
// - once explicitly disallowing `declare`s in regular `.ts`.
/** @type {{ selector: string; message: string; }[]} */
const RESTRICTED_SYNTAXES = [
    {
        selector:
            ':matches(ImportDeclaration:has(ImportSpecifier), ExportDeclaration, ExportSpecifier)',
        message: 'No {} imports and exports',
    },
    {
        selector: `ImportDeclaration[source.value=/^(?!(${ALLOWED_DEFAULT_IMPORT_MODULES})$)[^.]/] > ImportDefaultSpecifier`,
        message:
            'No default imports from modules. Add to `DEFAULT_IMPORT_ONLY_MODULES` in `eslint.config.js` if the module only has a default export.',
    },
    {
        selector: `ImportDeclaration[source.value=/^(?:${DEFAULT_IMPORT_ONLY_MODULES})$/] > ImportNamespaceSpecifier`,
        message: 'No namespace imports from modules that only have a default import',
    },
    {
        selector: `ImportDeclaration[source.value=/\\.(?:json|yaml|yml)$/] > ImportDefaultSpecifier[local.name=${NOT_CONSTANT_CASE}]`,
        message: 'Use `CONSTANT_CASE` for imports from data files',
    },
    {
        selector: `ImportDeclaration[source.value=/\\.json$/]:not(:has(ImportAttribute[key.name=type][value.value=json]))`,
        message: "JSON imports must be { type: 'json' }",
    },
    {
        selector: `ImportDeclaration[source.value=/\\.(?:yaml|yml)$/]:not(:has(ImportAttribute[key.name=type][value.value=yaml]))`,
        message: "YAML imports must be { type: 'yaml' }",
    },
    {
        selector: `ImportDeclaration[source.value=/\\.(?:json|yaml|yml)$/] > ImportNamespaceSpecifier`,
        message: 'Use default imports for imports from data files',
    },
    {
        selector: `ImportNamespaceSpecifier > Identifier[name=${NOT_CAMEL_CASE}]`,
        message: 'Use `camelCase` for imports',
    },
    {
        selector: `:matches(ImportDefaultSpecifier[local.name=/^${NAME}/i], ImportNamespaceSpecifier > Identifier[name=/^${NAME}/i])`,
        message: `Don't prefix modules with \`${NAME}\``,
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
        selector: 'ForOfStatement > .left[kind=let]',
        message: 'Use `for (const x of xs)`, not `for (let x of xs)`',
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
        selector:
            ':not(:matches(FunctionDeclaration, FunctionExpression, ArrowFunctionExpression, SwitchStatement, SwitchCase, IfStatement:has(.consequent > :matches(ReturnStatement, ThrowStatement)):has(.alternate :matches(ReturnStatement, ThrowStatement)), Program > TryStatement, Program > TryStatement > .handler, TryStatement:has(.block > :matches(ReturnStatement, ThrowStatement)):has(:matches([handler=null], .handler :matches(ReturnStatement, ThrowStatement))), TryStatement:has(.block > :matches(ReturnStatement, ThrowStatement)):has(:matches([handler=null], .handler :matches(ReturnStatement, ThrowStatement))) > .handler)) > * > :matches(ReturnStatement, ThrowStatement)',
        message: 'No early returns',
    },
    {
        selector:
            'TSTypeAliasDeclaration > :matches(TSBooleanKeyword, TSBigintKeyword, TSNullKeyword, TSNumberKeyword, TSObjectKeyword, TSStringKeyword, TSSymbolKeyword, TSUndefinedKeyword, TSUnknownKeyword, TSVoidKeyword)',
        message:
            'No aliases to primitives - consider using brands instead: `string & { _brand: "BrandName"; }`',
    },
    {
        // Matches functions and arrow functions, but not methods.
        selector: `:matches(FunctionDeclaration[id.name=${NOT_PASCAL_CASE}]:has(${JSX}), VariableDeclarator[id.name=${NOT_PASCAL_CASE}]:has(:matches(ArrowFunctionExpression.init ${JSX}, ${WITH_ROUTER})))`,
        message: 'Use `PascalCase` for React components',
    },
    {
        // Matches other functions, non-consts, and consts not at the top level.
        selector: `:matches(FunctionDeclaration[id.name=${NOT_CAMEL_CASE}]:not(:has(${JSX})), VariableDeclarator[id.name=${NOT_CAMEL_CASE}]:has(ArrowFunctionExpression.init:not(:has(${JSX}))), :matches(VariableDeclaration[kind^=const], Program :not(ExportNamedDeclaration, TSModuleBlock) > VariableDeclaration[kind=const], ExportNamedDeclaration > * VariableDeclaration[kind=const]) > VariableDeclarator[id.name=${NOT_CAMEL_CASE}]:not(:has(ArrowFunctionExpression)))`,
        message: 'Use `camelCase` for everything but React components',
    },
    {
        // Matches non-functions.
        selector: `:matches(Program, ExportNamedDeclaration, TSModuleBlock) > VariableDeclaration[kind=const] > VariableDeclarator[id.name=${NOT_CONSTANT_CASE}]:not(:has(:matches(ArrowFunctionExpression, ${WITH_ROUTER})))`,
        message: 'Use `CONSTANT_CASE` for top-level constants that are not functions',
    },
    {
        selector: `:matches(Program, ExportNamedDeclaration, TSModuleBlock) > VariableDeclaration > VariableDeclarator > ArrowFunctionExpression`,
        message: 'Use `function foo() {}` instead of `const foo = () => {}`',
    },
    {
        selector: `ClassBody > PropertyDefinition > ArrowFunctionExpression`,
        message: 'Use `foo() {}` instead of `foo = () => {}`',
    },
    {
        selector: `:matches(Program, ExportNamedDeclaration) > VariableDeclaration[kind=const] > * > ObjectExpression:has(Property > ${STRING_LITERAL}.value):not(:has(Property > .value:not(${STRING_LITERAL})))`,
        message: 'Use `as const` for top-level object literals only containing string literals',
    },
    {
        // Matches `as T` in either:
        // - anything other than a variable declaration
        // - a variable declaration that is not at the top level
        // - a top-level variable declaration that shouldn't be `as const`
        // - a top-level variable declaration that should be `as const`, but is `as SomeActualType` instead
        selector: `:matches(:not(VariableDeclarator) > TSAsExpression, :not(:matches(Program, ExportNamedDeclaration)) > VariableDeclaration > * > TSAsExpression, :matches(Program, ExportNamedDeclaration) > VariableDeclaration > * > TSAsExpression > .expression:not(ObjectExpression:has(Property > ${STRING_LITERAL}.value):not(:has(Property > .value:not(${STRING_LITERAL})))), :matches(Program, ExportNamedDeclaration) > VariableDeclaration > * > TsAsExpression:not(:has(TSTypeReference > Identifier[name=const])) > ObjectExpression.expression:has(Property > ${STRING_LITERAL}.value):not(:has(Property > .value:not(${STRING_LITERAL}))))`,
        message: 'Avoid `as T`. Consider using a type annotation instead.',
    },
    {
        selector:
            ':matches(TSUndefinedKeyword, Identifier[name=undefined], UnaryExpression[operator=void]:not(:has(CallExpression.argument)), BinaryExpression[operator=/^===?$/]:has(UnaryExpression.left[operator=typeof]):has(Literal.right[value=undefined]))',
        message: 'Use `null` instead of `undefined`, `void 0`, or `typeof x === "undefined"`',
    },
    {
        selector: 'ExportNamedDeclaration > VariableDeclaration[kind=let]',
        message: 'Use `export const` instead of `export let`',
    },
    {
        selector: `Program > VariableDeclaration[kind=let] > * > ObjectExpression:has(Property > ${STRING_LITERAL}.value):not(:has(Property > .value:not(${STRING_LITERAL})))`,
        message:
            'Use `const` instead of `let` for top-level object literals only containing string literals',
    },
    {
        selector:
            'ImportDeclaration[source.value=/^(?:assert|async_hooks|buffer|child_process|cluster|console|constants|crypto|dgram|diagnostics_channel|dns|domain|events|fs|fs\\u002Fpromises|http|http2|https|inspector|module|net|os|path|perf_hooks|process|punycode|querystring|readline|repl|stream|string_decoder|timers|tls|trace_events|tty|url|util|v8|vm|wasi|worker_threads|zlib)$/]',
        message: 'Use `node:` prefix to import builtin node modules',
    },
    {
        selector: 'TSEnumDeclaration:not(:has(TSEnumMember))',
        message: 'Enums must not be empty',
    },
    {
        selector:
            'ImportDeclaration[source.value=/^(?!node:)/] ~ ImportDeclaration[source.value=/^node:/]',
        message:
            'Import node modules before npm modules, our modules, and relative imports, separated by a blank line',
    },
    {
        selector: `ImportDeclaration[source.value=/^(?:${OUR_MODULES}|${RELATIVE_MODULES})$/] ~ ImportDeclaration[source.value=/^(?!(|${OUR_MODULES}|${RELATIVE_MODULES})$|\\.)/]`,
        message:
            'Import npm modules before our modules and relative imports, separated by a blank line',
    },
    {
        selector: `ImportDeclaration[source.value=/^(?:${RELATIVE_MODULES})$/] ~ ImportDeclaration[source.value=/^(?:${OUR_MODULES})$/]`,
        message: 'Import our modules before relative imports, separated by a blank line',
    },
    {
        selector: `ImportDeclaration[source.value=/^\\./] ~ ImportDeclaration[source.value=/^[^.]/]`,
        message: 'Import relative imports last',
    },
    {
        selector: `ImportDeclaration[source.value=/^\\..+\\.(?:json|yml|yaml)$/] ~ ImportDeclaration[source.value=/^\\..+\\.(?!json|yml|yaml)[^.]+$/]`,
        message: 'Import data files after other relative imports',
    },
    {
        selector:
            'TSAsExpression:has(TSUnknownKeyword, TSNeverKeyword, TSAnyKeyword) > TSAsExpression',
        message: 'Use type assertions to specific types instead of `unknown`, `any` or `never`',
    },
]

/* eslint-disable @typescript-eslint/naming-convention */
export default [
    eslintJs.configs.recommended,
    {
        plugins: {
            jsdoc: jsdoc,
            '@typescript-eslint': tsEslint,
        },
        languageOptions: {
            parser: tsEslintParser,
            parserOptions: {
                tsconfigRootDir: DIR_NAME,
                project: true,
            },
            globals: {
                ...globals.browser,
                ...globals.node,
                ...globals.es2015,
            },
        },
        rules: {
            ...tsEslint.configs['eslint-recommended']?.rules,
            ...tsEslint.configs.recommended?.rules,
            ...tsEslint.configs['recommended-requiring-type-checking']?.rules,
            ...tsEslint.configs.strict?.rules,
            eqeqeq: ['error', 'always', { null: 'never' }],
            'sort-imports': ['error', { allowSeparatedGroups: true }],
            'no-restricted-syntax': ['error', ...RESTRICTED_SYNTAXES],
            'prefer-arrow-callback': 'error',
            // Prefer `interface` over `type`.
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
                    format: ['UPPER_CASE', 'camelCase', 'PascalCase'],
                },
                {
                    selector: ['variable'],
                    modifiers: ['const', 'exported'],
                    format: ['UPPER_CASE', 'camelCase', 'PascalCase'],
                },
                {
                    selector: ['variable'],
                    format: ['camelCase', 'PascalCase'],
                },
                {
                    selector: ['parameter', 'property', 'method'],
                    format: ['camelCase'],
                },
                {
                    selector: ['parameter'],
                    modifiers: ['unused'],
                    format: ['camelCase'],
                    leadingUnderscore: 'require',
                },
            ],
            '@typescript-eslint/no-confusing-void-expression': 'error',
            '@typescript-eslint/no-empty-interface': 'off',
            '@typescript-eslint/no-extraneous-class': 'error',
            '@typescript-eslint/no-invalid-void-type': ['error', { allowAsThisParameter: true }],
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
            '@typescript-eslint/no-redeclare': ['error', { ignoreDeclarationMerge: false }],
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
            'jsdoc/no-multi-asterisks': 'warn',
            'jsdoc/no-types': 'warn',
            'jsdoc/no-undefined-types': 'warn',
            'jsdoc/require-asterisk-prefix': 'warn',
            'jsdoc/require-description': 'warn',
            // This rule does not handle `# Heading`s and "etc.", "e.g.", "vs." etc.
            // 'jsdoc/require-description-complete-sentence': 'warn',
            'jsdoc/require-file-overview': 'warn',
            'jsdoc/require-hyphen-before-param-description': 'warn',
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
        },
    },
    {
        files: ['**/*.js', '**/*.jsx', '**/*.cjs', '**/*.mjs'],
        rules: {
            '@typescript-eslint/no-var-requires': 'off',
            // Parameter types must be specified using JSDoc in JS files.
            'jsdoc/no-types': 'off',
        },
    },
    {
        files: ['**/*.ts', '**/*.mts', '**/*.cts', '**/*.tsx', '**/*.mtsx', '**/*.ctsx'],
        ignores: ['**/*.d.ts'],
        rules: {
            'no-restricted-syntax': [
                'error',
                ...RESTRICTED_SYNTAXES,
                {
                    selector: '[declare=true]',
                    message: 'No ambient declarations',
                },
            ],
            // This rule does not work with TypeScript, and TypeScript already does this.
            'no-undef': 'off',
        },
    },
    {
        files: ['**/*.d.ts'],
        rules: {
            'no-undef': 'off',
        },
    },
]
