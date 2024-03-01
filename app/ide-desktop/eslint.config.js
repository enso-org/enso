/** @file ESLint configuration file. */
/** NOTE: The "Experimental: Use Flat Config" option must be enabled.
 * Flat config is still not quite mature, so is disabled by default. */
import * as path from 'node:path'
import * as url from 'node:url'

// The preferred syntax is `import * as name`, however these modules do not support it.
// This is specialcased in other files, but these modules shouldn't be used in other files anyway.
/* eslint-disable no-restricted-syntax */
import eslintJs from '@eslint/js'
import globals from 'globals'
import jsdoc from 'eslint-plugin-jsdoc'
import react from 'eslint-plugin-react'
import reactHooks from 'eslint-plugin-react-hooks'
import tsEslint from '@typescript-eslint/eslint-plugin'
import tsEslintParser from '@typescript-eslint/parser'
/* eslint-enable no-restricted-syntax */

// =================
// === Constants ===
// =================

const DIR_NAME = path.dirname(url.fileURLToPath(import.meta.url))
const NAME = 'enso'
/** An explicit whitelist of CommonJS modules, which do not support namespace imports.
 * Many of these have incorrect types, so no type error may not mean they support ESM,
 * and conversely type errors may not mean they don't support ESM -
 * but we add those to the whitelist anyway otherwise we get type errors.
 * In particular, `string-length` supports ESM but its type definitions don't.
 * `yargs` is a modules we explicitly want the default imports of.
 * `node:process` is here because `process.on` does not exist on the namespace import. */
const DEFAULT_IMPORT_ONLY_MODULES =
    '@vitejs\\u002Fplugin-react|node:process|chalk|string-length|yargs|yargs\\u002Fyargs|sharp|to-ico|connect|morgan|serve-static|create-servers|electron-is-dev|fast-glob|esbuild-plugin-.+|opener|tailwindcss.*|enso-assets.*|@modyfi\\u002Fvite-plugin-yaml|is-network-error|validator.+'
const OUR_MODULES = 'enso-.*'
const RELATIVE_MODULES =
    'bin\\u002Fproject-manager|bin\\u002Fserver|config\\u002Fparser|authentication|config|debug|detect|file-associations|index|ipc|log|naming|paths|preload|project-management|security|url-associations|#\\u002F.*'
const ALLOWED_DEFAULT_IMPORT_MODULES = `${DEFAULT_IMPORT_ONLY_MODULES}|postcss|ajv\\u002Fdist\\u002F2020|${RELATIVE_MODULES}`
const STRING_LITERAL = ':matches(Literal[raw=/^["\']/], TemplateLiteral)'
const JSX = ':matches(JSXElement, JSXFragment)'
const NOT_PASCAL_CASE = '/^(?!do[A-Z])(?!_?([A-Z][a-z0-9]*)+$)/'
const NOT_CAMEL_CASE = '/^(?!_?[a-z][a-z0-9*]*([A-Z0-9][a-z0-9]*)*$)(?!React$)/'
const WHITELISTED_CONSTANTS = 'logger|.+Context|interpolationFunction.+'
const NOT_CONSTANT_CASE = `/^(?!${WHITELISTED_CONSTANTS}$|_?[A-Z][A-Z0-9]*(_[A-Z0-9]+)*$)/`

// =======================================
// === Restricted syntactic constructs ===
// =======================================

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
        selector: 'TSTypeLiteral',
        message: 'No object types - use interfaces instead',
    },
    {
        selector: 'ForOfStatement > .left[kind=let]',
        message: 'Use `for (const x of xs)`, not `for (let x of xs)`',
    },
    {
        selector:
            'TSTypeAliasDeclaration > TSTypeReference:not(:has(.typeParameters)) > Identifier',
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
        selector: `:matches(FunctionDeclaration[id.name=${NOT_PASCAL_CASE}]:has(${JSX}), VariableDeclarator[id.name=${NOT_PASCAL_CASE}]:has(:matches(ArrowFunctionExpression.init ${JSX})))`,
        message: 'Use `PascalCase` for React components',
    },
    {
        // Matches other functions, non-consts, and consts not at the top level.
        selector: `:matches(FunctionDeclaration[id.name=${NOT_CAMEL_CASE}]:not(:has(${JSX})), VariableDeclarator[id.name=${NOT_CAMEL_CASE}]:has(ArrowFunctionExpression.init:not(:has(${JSX}))), :matches(VariableDeclaration[kind^=const], Program :not(ExportNamedDeclaration, TSModuleBlock) > VariableDeclaration[kind=const], ExportNamedDeclaration > * VariableDeclaration[kind=const]) > VariableDeclarator[id.name=${NOT_CAMEL_CASE}]:not(:has(ArrowFunctionExpression)))`,
        message: 'Use `camelCase` for everything but React components',
    },
    {
        // Matches non-functions.
        selector: `:matches(Program, ExportNamedDeclaration, TSModuleBlock) > VariableDeclaration[kind=const] > VariableDeclarator[id.name=${NOT_CONSTANT_CASE}]:not(:matches(:has(ArrowFunctionExpression), :has(CallExpression[callee.object.name=newtype][callee.property.name=newtypeConstructor])))`,
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
        // This lint intentionally excludes classes and readonly arrays.
        selector: 'TSInterfaceBody:has(TSPropertySignature[readonly=false])',
        message: 'Add `readonly` modifier to all interface properties',
    },
    {
        selector: `TSAsExpression:not(:has(TSTypeReference > Identifier[name=const]))`,
        message: 'Avoid `as T`. Consider using a type annotation instead.',
    },
    {
        selector: `:matches(\
            TSUndefinedKeyword,\
            Identifier[name=undefined],\
            UnaryExpression[operator=void]:not(:has(CallExpression.argument)), BinaryExpression[operator=/^===?$/]:has(UnaryExpression.left[operator=typeof]):has(Literal.right[value=undefined])\
        )`,
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
    {
        selector: ':matches(MethodDeclaration, FunctionDeclaration) FunctionDeclaration',
        message: 'Use arrow functions for nested functions',
    },
    {
        selector:
            ':not(ExportNamedDeclaration) > TSInterfaceDeclaration[id.name=/^(?!Internal).+Props$/]',
        message: 'All React component `Props` types must be exported',
    },
    {
        selector: 'FunctionDeclaration:has(:matches(ObjectPattern.params, ArrayPattern.params))',
        message: 'Destructure function parameters in the body, instead of in the parameter list',
    },
    {
        selector: 'IfStatement > ExpressionStatement',
        message: 'Wrap `if` branches in `{}`',
    },
    {
        selector: ':matches(ForStatement[test=null], ForStatement[test.value=true])',
        message: 'Use `while (true)` instead of `for (;;)`',
    },
    {
        selector: 'CallExpression[callee.name=toastAndLog][arguments.0.value=/\\.$/]',
        message: '`toastAndLog` already includes a trailing `.`',
    },
    {
        selector:
            'JSXElement[closingElement!=null]:not(:has(.children:matches(JSXText[raw=/\\S/], :not(JSXText))))',
        message: 'Use self-closing tags (`<tag />`) for tags without children',
    },
    {
        // TODO [sb]: `z-3` should be eliminated, but is currently still required.
        // TODO: this does not work for template strings and nested strings
        selector: `:matches(\
            TemplateElement[value.raw=/\\b(?:size|w|h|p[xylrbt]?|m[xylrbt]?)-(?:\\d|px|\\[)/],\
            Literal[value=/\\b(?:size|w|h|p[xylrbt]?|m[xylrbt]?)-(?:\\d|px|\\[)/]\
        )`,
        message: 'Fixed values for Tailwind `size-`, `w-`, `h-`, `p-`, `m-` are not allowed',
    },
    {
        selector: `:matches(\
            TemplateElement[value.raw=/\\b(?:opacity|gap|rounded(?:-[lrbtxy])?|leading|duration|grid-cols-fill)-(?:xs|sm|md|lg|xl|\\d|\\[)/],\
            Literal[value=/\\b(?:opacity|gap|rounded(?:-[lrbtxy])?|leading|duration|grid-cols-fill)-(?:xs|sm|md|lg|xl|\\d|\\[)/]\
        )`,
        message:
            'Fixed values for Tailwind `opacity-`, `rounded-`, `leading-`, `duration-` and `grid-cols-fill` are not allowed',
    },
]

// ============================
// === ESLint configuration ===
// ============================

/* eslint-disable @typescript-eslint/naming-convention */
export default [
    eslintJs.configs.recommended,
    {
        // Playwright build cache.
        ignores: ['**/.cache/**'],
    },
    {
        settings: {
            react: {
                version: '18.2',
            },
        },
        plugins: {
            jsdoc: jsdoc,
            '@typescript-eslint': tsEslint,
            react: react,
            'react-hooks': reactHooks,
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
            ...react.configs.recommended.rules,
            eqeqeq: ['error', 'always', { null: 'never' }],
            'jsdoc/require-jsdoc': [
                'error',
                {
                    require: {
                        FunctionDeclaration: true,
                        MethodDefinition: true,
                        ClassDeclaration: true,
                        ArrowFunctionExpression: false,
                        FunctionExpression: true,
                    },
                    // Top-level constants should require JSDoc as well,
                    // however it does not seem like there is a way to do this.
                    contexts: [
                        'TSInterfaceDeclaration',
                        'TSEnumDeclaration',
                        'TSTypeAliasDeclaration',
                        'TSMethodSignature',
                    ],
                },
            ],
            'no-constant-condition': ['error', { checkLoops: false }],
            'no-restricted-properties': [
                'error',
                {
                    object: 'router',
                    property: 'useNavigate',
                    message: 'Use `hooks.useNavigate` instead.',
                },
            ],
            'no-restricted-syntax': ['error', ...RESTRICTED_SYNTAXES],
            'prefer-arrow-callback': 'error',
            'prefer-const': 'error',
            // Not relevant because TypeScript checks types.
            'react/prop-types': 'off',
            'react-hooks/rules-of-hooks': 'error',
            'react-hooks/exhaustive-deps': 'error',
            // Prefer `interface` over `type`.
            '@typescript-eslint/consistent-type-definitions': 'error',
            '@typescript-eslint/consistent-type-imports': 'error',
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
            '@typescript-eslint/no-unnecessary-condition': [
                'error',
                { allowConstantLoopConditions: true },
            ],
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
            '@typescript-eslint/strict-boolean-expressions': 'error',
            '@typescript-eslint/switch-exhaustiveness-check': 'error',
            'default-param-last': 'off',
            '@typescript-eslint/default-param-last': 'error',
            'no-invalid-this': 'off',
            '@typescript-eslint/no-invalid-this': ['error', { capIsConstructor: false }],
            'jsdoc/no-magic-numbers': 'off',
            '@typescript-eslint/no-magic-numbers': [
                'error',
                {
                    ignore: [-1, 0, 1, 2, 3, 4, 5],
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
            '@typescript-eslint/no-shadow': 'error',
            'no-unused-expressions': 'off',
            '@typescript-eslint/no-unused-expressions': 'error',
            'jsdoc/require-param-type': 'off',
            'jsdoc/check-access': 'error',
            'jsdoc/check-alignment': 'error',
            'jsdoc/check-indentation': 'error',
            'jsdoc/check-line-alignment': 'error',
            'jsdoc/check-param-names': 'error',
            'jsdoc/check-property-names': 'error',
            'jsdoc/check-syntax': 'error',
            'jsdoc/check-tag-names': 'error',
            'jsdoc/check-types': 'error',
            'jsdoc/check-values': 'error',
            'jsdoc/empty-tags': 'error',
            'jsdoc/implements-on-classes': 'error',
            'jsdoc/no-bad-blocks': 'error',
            'jsdoc/no-defaults': 'error',
            'jsdoc/no-multi-asterisks': 'error',
            'jsdoc/no-types': 'error',
            'jsdoc/no-undefined-types': 'error',
            'jsdoc/require-asterisk-prefix': 'error',
            'jsdoc/require-description': 'error',
            // This rule does not handle `# Heading`s and "etc.", "e.g.", "vs." etc.
            // 'jsdoc/require-description-complete-sentence': 'error',
            'jsdoc/require-file-overview': 'error',
            'jsdoc/require-hyphen-before-param-description': 'error',
            'jsdoc/require-param-description': 'error',
            'jsdoc/require-param-name': 'error',
            'jsdoc/require-property': 'error',
            'jsdoc/require-property-description': 'error',
            'jsdoc/require-property-name': 'error',
            'jsdoc/require-property-type': 'error',
            'jsdoc/require-returns-check': 'error',
            'jsdoc/require-returns-description': 'error',
            'jsdoc/require-throws': 'error',
            'jsdoc/require-yields': 'error',
            'jsdoc/require-yields-check': 'error',
            'jsdoc/tag-lines': 'error',
            'jsdoc/valid-types': 'error',
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
                    selector: ':not(TSModuleDeclaration)[declare=true]',
                    message: 'No ambient declarations',
                },
                {
                    selector: 'ExportDefaultDeclaration:has(Identifier.declaration)',
                    message:
                        'Use `export default` on the declaration, instead of as a separate statement',
                },
            ],
            // This rule does not work with TypeScript, and TypeScript already does this.
            'no-undef': 'off',
        },
    },
    {
        files: [
            'lib/dashboard/src/**/*.ts',
            'lib/dashboard/src/**/*.mts',
            'lib/dashboard/src/**/*.cts',
            'lib/dashboard/src/**/*.tsx',
            'lib/dashboard/src/**/*.mtsx',
            'lib/dashboard/src/**/*.ctsx',
            'lib/dashboard/mock/**/*.ts',
            'lib/dashboard/mock/**/*.mts',
            'lib/dashboard/mock/**/*.cts',
            'lib/dashboard/mock/**/*.tsx',
            'lib/dashboard/mock/**/*.mtsx',
            'lib/dashboard/mock/**/*.ctsx',
        ],
        rules: {
            'no-restricted-properties': [
                'error',
                {
                    object: 'console',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    property: 'useDebugState',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    property: 'useDebugEffect',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    property: 'useDebugMemo',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    property: 'useDebugCallback',
                    message: 'Avoid leaving debugging statements when committing code',
                },
            ],
        },
    },
    {
        files: [
            'lib/dashboard/e2e/**/*.ts',
            'lib/dashboard/e2e/**/*.mts',
            'lib/dashboard/e2e/**/*.cts',
            'lib/dashboard/e2e/**/*.tsx',
            'lib/dashboard/e2e/**/*.mtsx',
            'lib/dashboard/e2e/**/*.ctsx',
        ],
        rules: {
            'no-restricted-properties': [
                'error',
                {
                    object: 'console',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    object: 'hooks',
                    property: 'useDebugState',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    object: 'hooks',
                    property: 'useDebugEffect',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    object: 'hooks',
                    property: 'useDebugMemo',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    object: 'hooks',
                    property: 'useDebugCallback',
                    message: 'Avoid leaving debugging statements when committing code',
                },
                {
                    object: 'page',
                    property: 'type',
                    message: 'Prefer `locator.type` instead',
                },
                {
                    object: 'page',
                    property: 'click',
                    message: 'Prefer `locator.click` instead',
                },
                {
                    object: 'page',
                    property: 'fill',
                    message: 'Prefer `locator.fill` instead',
                },
                {
                    object: 'page',
                    property: 'locator',
                    message: 'Prefer `page.getBy*` instead',
                },
            ],
        },
    },
    {
        files: ['**/*.d.ts'],
        rules: {
            'no-undef': 'off',
        },
    },
    {
        files: ['**/tailwind.config.ts'],
        rules: {
            'no-restricted-syntax': 'off',
            '@typescript-eslint/naming-convention': 'off',
        },
    },
]
