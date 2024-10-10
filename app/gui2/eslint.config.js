import { FlatCompat } from '@eslint/eslintrc'
import eslintJs from '@eslint/js'
import jsdoc from 'eslint-plugin-jsdoc'
import * as path from 'node:path'
import * as url from 'node:url'

const compat = new FlatCompat()

const DIR_NAME = path.dirname(url.fileURLToPath(import.meta.url))

const conf = [
  {
    ignores: [
      'dist',
      'templates',
      '.histoire',
      'playwright-report',
      'test-results',
      '**/*.timestamp-*.mjs',
    ],
  },
  ...compat.extends('plugin:vue/vue3-recommended'),
  eslintJs.configs.recommended,
  ...compat.extends('@vue/eslint-config-typescript', '@vue/eslint-config-prettier'),
  {
    // files: ['{**,src}/*.{vue,js,jsx,cjs,mjs,ts,tsx,cts,mts}'],
    plugins: { jsdoc },
    languageOptions: {
      parserOptions: {
        tsconfigRootDir: DIR_NAME,
        ecmaVersion: 'latest',
        project: ['./tsconfig.app.json', './tsconfig.node.json', './tsconfig.app.vitest.json'],
      },
    },
    rules: {
      camelcase: [1, { ignoreImports: true }],
      'no-inner-declarations': 0,
      'vue/attribute-hyphenation': [2, 'never'],
      'vue/v-on-event-hyphenation': [2, 'never'],
      '@typescript-eslint/no-unused-vars': [
        1,
        {
          varsIgnorePattern: '^_',
          argsIgnorePattern: '^_',
        },
      ],
      'no-unused-labels': 0,
    },
  },
  {
    files: ['stories/*.vue'],
    rules: {
      'vue/multi-word-component-names': 0,
    },
  },
  // JsDoc lints for typescript - the recommended set with some modifications.
  {
    ignores: ['**/*.js'],
    ...jsdoc.configs['flat/recommended-typescript'],
    rules: {
      ...jsdoc.configs['flat/recommended-typescript'].rules,
      'jsdoc/check-param-names': [
        'warn',
        { checkDestructured: false, disableMissingParamChecks: true },
      ],
      'jsdoc/require-jsdoc': [
        'warn',
        {
          publicOnly: true,
          require: {
            FunctionDeclaration: true,
            MethodDefinition: true,
            ClassDeclaration: true,
            ArrowFunctionExpression: false,
            FunctionExpression: true,
          },
        },
      ],
      'jsdoc/require-param': 'off',
      'jsdoc/require-returns': 'off',
      'jsdoc/require-yields': 'off',
    },
  },
  // We must make sure our E2E tests await all steps, otherwise they're flaky.
  {
    files: ['e2e/**/*.spec.ts'],
    languageOptions: {
      parser: await import('@typescript-eslint/parser'),
    },
    rules: {
      '@typescript-eslint/no-floating-promises': 2,
    },
  },
]

export default conf
