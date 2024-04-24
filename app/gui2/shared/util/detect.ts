// Environment detection

// Normally `global` is node-specific, but a workaround requires `global` to also exist
// in the browser for Amplify to work.
export const isNode =
  typeof global !== 'undefined' && (global as any)[Symbol.toStringTag] === 'global'

export const isDevMode = process.env.NODE_ENV === 'development'
