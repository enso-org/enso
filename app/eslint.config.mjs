/** @file ESLint configuration file. */
/** NOTE: The "Experimental: Use Flat Config" option must be enabled.
 * Flat config is still not quite mature, so is disabled by default. */
import * as path from 'node:path'
import * as url from 'node:url'

// The preferred syntax is `import * as name`, however these modules do not support it.
// This is specialcased in other files, but these modules shouldn't be used in other files anyway.
/* eslint-disable no-restricted-syntax */
import eslintJs from '@eslint/js'
import tsEslint from '@typescript-eslint/eslint-plugin'
import tsEslintParser from '@typescript-eslint/parser'
import jsdoc from 'eslint-plugin-jsdoc'
import react from 'eslint-plugin-react'
import reactHooks from 'eslint-plugin-react-hooks'
import globals from 'globals'
/* eslint-enable no-restricted-syntax */

// =================
// === Constants ===
// =================

const DEBUG_STATEMENTS_MESSAGE = 'Avoid leaving debugging statements when committing code'
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
  '@vitejs\\u002Fplugin-react|node:process|chalk|string-length|yargs|yargs\\u002Fyargs|sharp|to-ico|connect|morgan|serve-static|tiny-invariant|clsx|create-servers|electron-is-dev|fast-glob|esbuild-plugin-.+|opener|tailwindcss.*|@modyfi\\u002Fvite-plugin-yaml|build-info|is-network-error|validator.+|.*[.]json$'
const RELATIVE_MODULES =
  'projectManager|server|configParser|authentication|config|debug|detect|fileAssociations|index|ipc|log|naming|paths|preload|projectManagement|security|urlAssociations|contentConfig|desktopEnvironment|#\\u002F.*'
const ALLOWED_DEFAULT_IMPORT_MODULES = `${DEFAULT_IMPORT_ONLY_MODULES}|postcss|ajv\\u002Fdist\\u002F2020|${RELATIVE_MODULES}`
const STRING_LITERAL = ':matches(Literal[raw=/^["\']/], TemplateLiteral)'
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
    selector: 'ForOfStatement > .left[kind=let]',
    message: 'Use `for (const x of xs)`, not `for (let x of xs)`',
  },
  {
    selector: 'TSTypeAliasDeclaration > TSTypeReference:not(:has(.typeParameters)) > Identifier',
    message: 'No renamed types',
  },
  {
    selector: 'TSTypeAliasDeclaration > :matches(TSLiteralType)',
    message: 'No aliases to literal types',
  },
  {
    selector:
      'TSTypeAliasDeclaration > :matches(TSBooleanKeyword, TSBigintKeyword, TSNullKeyword, TSNumberKeyword, TSObjectKeyword, TSStringKeyword, TSSymbolKeyword, TSUndefinedKeyword, TSUnknownKeyword, TSVoidKeyword)',
    message:
      'No aliases to primitives - consider using brands instead: `string & { _brand: "BrandName"; }`',
  },
  {
    // Matches non-functions.
    selector: `:matches(Program, ExportNamedDeclaration, TSModuleBlock) > VariableDeclaration[kind=const] > VariableDeclarator[id.name=${NOT_CONSTANT_CASE}]:not(:matches([init.callee.object.name=React][init.callee.property.name=forwardRef], :has(ArrowFunctionExpression), :has(CallExpression[callee.object.name=newtype][callee.property.name=newtypeConstructor]), :has(CallExpression[callee.name=newtypeConstructor])))`,
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
    selector: 'TSAsExpression:has(TSUnknownKeyword, TSNeverKeyword, TSAnyKeyword) > TSAsExpression',
    message: 'Use type assertions to specific types instead of `unknown`, `any` or `never`',
  },
  {
    selector: ':matches(MethodDeclaration, FunctionDeclaration) FunctionDeclaration',
    message: 'Use arrow functions for nested functions',
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
    selector: `:matches(\
            JSXAttribute[name.name=/^(?:alt|error|label|placeholder|text|title|actionButtonLabel|actionText|aria-label)$/][value.raw=/^'|^"|^\`/], \
            JSXText[value=/\\S/], \
            JSXAttribute[name.name=/^(?:alt|error|label|placeholder|text|title|actionButtonLabel|actionText|aria-label)$/] ConditionalExpression:matches(\
                [consequent.raw=/^'|^"|^\`/], \
                [alternate.raw=/^'|^"|^\`/]\
            )\
        )`,
    message: 'Use a `getText()` from `useText` instead of a literal string',
  },
  {
    selector: `JSXAttribute[name.name=/^(?:className)$/] TemplateLiteral`,
    message:
      'Use `tv` from `#/utilities/tailwindVariants` or `twMerge` from `tailwind-merge` instead of template strings for classes',
  },
  {
    selector: 'JSXOpeningElement[name.name=button] > JSXIdentifier',
    message: 'Use `Button` or `UnstyledButton` instead of `button`',
  },
  {
    selector: 'JSXOpeningElement[name.name=/^h[123456]$/] > JSXIdentifier',
    message: 'Use `aria.Heading` instead of `h1`-`h6`',
  },
]

// ============================
// === ESLint configuration ===
// ============================

/* eslint-disable @typescript-eslint/naming-convention */
export default [
  eslintJs.configs.recommended,
  {
    // Playwright build cache and Vite build directory.
    ignores: [
      '**/.cache/**',
      '**/playwright-report',
      '**/dist',
      '**/build.mjs',
      '**/*.timestamp-*.mjs',
    ],
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
      ...react.configs['jsx-runtime'].rules,
      eqeqeq: ['error', 'always', { null: 'never' }],
      // Any extra semicolons that exist, are required by Prettier.
      'no-extra-semi': 'off',
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
      'no-restricted-syntax': ['error', ...RESTRICTED_SYNTAXES],
      'prefer-const': 'error',
      'react/forbid-elements': [
        'error',
        { forbid: [{ element: 'Debug', message: DEBUG_STATEMENTS_MESSAGE }] },
      ],
      // Not relevant because TypeScript checks types.
      'react/prop-types': 'off',
      'react/self-closing-comp': 'error',
      'react-hooks/rules-of-hooks': 'error',
      'react-hooks/exhaustive-deps': [
        'error',
        { additionalHooks: 'useOnScroll|useStickyTableHeaderOnScroll' },
      ],
      'react/jsx-pascal-case': ['error', { allowNamespace: true }],
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
          selector: ['parameter', 'method'],
          format: ['camelCase'],
        },
        {
          selector: ['parameter'],
          modifiers: ['unused'],
          format: ['camelCase'],
          leadingUnderscore: 'require',
        },
        {
          selector: ['property'],
          format: ['camelCase'],
          filter: {
            regex: '^(?:data-testid)$',
            match: false,
          },
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
      '@typescript-eslint/require-array-sort-compare': ['error', { ignoreStringArrays: true }],
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
    ignores: ['**/build.mjs'],
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
      ],
      // This rule does not work with TypeScript, and TypeScript already does this.
      'no-undef': 'off',
    },
  },
  {
    files: [
      'dashboard/src/**/*.ts',
      'dashboard/src/**/*.mts',
      'dashboard/src/**/*.cts',
      'dashboard/src/**/*.tsx',
      'dashboard/src/**/*.mtsx',
      'dashboard/src/**/*.ctsx',
      'dashboard/mock/**/*.ts',
      'dashboard/mock/**/*.mts',
      'dashboard/mock/**/*.cts',
      'dashboard/mock/**/*.tsx',
      'dashboard/mock/**/*.mtsx',
      'dashboard/mock/**/*.ctsx',
    ],
    rules: {
      'no-restricted-properties': [
        'error',
        { object: 'console', message: DEBUG_STATEMENTS_MESSAGE },
        { property: 'useDebugState', message: DEBUG_STATEMENTS_MESSAGE },
        { property: 'useDebugEffect', message: DEBUG_STATEMENTS_MESSAGE },
        { property: 'useDebugMemo', message: DEBUG_STATEMENTS_MESSAGE },
        { property: 'useDebugCallback', message: DEBUG_STATEMENTS_MESSAGE },
      ],
    },
  },
  {
    files: [
      'dashboard/e2e/**/*.ts',
      'dashboard/e2e/**/*.mts',
      'dashboard/e2e/**/*.cts',
      'dashboard/e2e/**/*.tsx',
      'dashboard/e2e/**/*.mtsx',
      'dashboard/e2e/**/*.ctsx',
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
