/**
 * @file Type definitions for modules that currently lack typings on DefinitelyTyped.
 *
 * This file MUST NOT `export {}` so that the modules are visible to other files.
 */

// ===========================
// === Module declarations ===
// ===========================

declare module 'eslint-plugin-react' {
  /** An ESLint configuration. */
  interface Config {
    readonly plugins: string[]
    readonly rules: Record<string, number>
    readonly parserOptions: object
  }

  /** Configurations defined by this ESLint plugin. */
  interface Configs {
    readonly recommended: Config
    readonly all: Config
    readonly 'jsx-runtime': Config
  }

  /** Deprecated rules contained in this ESLint plugin. */
  interface DeprecatedRules {
    readonly 'jsx-sort-default-props': object
    readonly 'jsx-space-before-closing': object
  }

  /** The default export of this ESLint plugin. */
  export interface Default {
    readonly rules: Record<string, object>
    readonly configs: Configs
    readonly deprecatedRules: DeprecatedRules
  }

  export const deprecatedRules: DeprecatedRules

  const DEFAULT: Default
  export default DEFAULT
}

declare module 'eslint-plugin-react-hooks' {
  /** An ESLint configuration. */
  interface Config {
    readonly plugins: string[]
    readonly rules: Record<string, string>
  }

  /** Configurations defined by this ESLint plugin. */
  interface Configs {
    readonly recommended: Config
  }

  /** Rules defined by this ESLint plugin. */
  interface ReactHooksRules {
    readonly 'rules-of-hooks': object
    readonly 'exhaustive-deps': object
  }

  /** The default export of this ESLint plugin. */
  export interface Default {
    readonly configs: Configs
    readonly rules: ReactHooksRules
  }

  export const configs: Configs
  export const rules: ReactHooksRules

  const DEFAULT: Default
  export default DEFAULT
}
