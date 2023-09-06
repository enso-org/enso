/* eslint-env node */
require('@rushstack/eslint-patch/modern-module-resolution')

module.exports = {
  root: true,
  extends: [
    'plugin:vue/vue3-recommended',
    'eslint:recommended',
    '@vue/eslint-config-typescript',
    '@vue/eslint-config-prettier/skip-formatting',
  ],
  parserOptions: {
    ecmaVersion: 'latest',
  },
  ignorePatterns: ['rust-ffi/pkg/**'],
  rules: {
    camelcase: [
      1,
      {
        ignoreImports: true,
      },
    ],
    'no-inner-declarations': 0,
    'vue/v-on-event-hyphenation': [2, 'never'],
    '@typescript-eslint/no-unused-vars': [
      1,
      {
        varsIgnorePattern: '^_',
        argsIgnorePattern: '^_',
      },
    ],
  },
}
