// Environment detection

// Normally `global` is node-specific, but a workaround requires `global` to also exist
// in the browser for Amplify to work.
export const isNode =
  typeof global !== 'undefined' && (global as any)[Symbol.toStringTag] === 'global'

// Java environment is set up to have a `jvm: 'graalvm'` property.
export const isJvm = typeof global !== 'undefined' && (global as any)['jvm'] === 'graalvm'

export const isDevMode = process.env.NODE_ENV === 'development'
