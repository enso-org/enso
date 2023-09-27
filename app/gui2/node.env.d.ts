module 'tailwindcss/nesting' {
  import { PluginCreator } from 'postcss'
  declare const plugin: PluginCreator<unknown>
  export default plugin
}

// This is an augmentation to the built-in `ImportMeta` interface.
// This file MUST NOT contain any top-level imports.
interface ImportMeta {
  vitest: typeof import('vitest') | undefined
}
