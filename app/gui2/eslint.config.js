import { FlatCompat } from '@eslint/eslintrc'
import eslintJs from '@eslint/js'
import * as path from 'node:path'
import * as url from 'node:url'

const compat = new FlatCompat()

const DIR_NAME = path.dirname(url.fileURLToPath(import.meta.url))

const conf = [
  {
    ignores: ['rust-ffi/pkg', 'dist', 'src/generated'],
  },
  ...compat.extends('plugin:vue/vue3-recommended'),
  eslintJs.configs.recommended,
  ...compat.extends('@vue/eslint-config-typescript', '@vue/eslint-config-prettier'),
  {
    // files: ['{**,src}/*.{vue,js,jsx,cjs,mjs,ts,tsx,cts,mts}'],
    languageOptions: {
      parserOptions: {
        tsconfigRootDir: DIR_NAME,
        ecmaVersion: 'latest',
        project: [
          './tsconfig.app.json',
          './tsconfig.node.json',
          './tsconfig.server.json',
          './tsconfig.app.vitest.json',
          './tsconfig.server.vitest.json',
        ],
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
    },
  },
]

export default conf
