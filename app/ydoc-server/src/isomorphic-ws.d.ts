// `isomorphic-ws` exports `ws` as a CJS module, we need to fix manually.
declare module 'isomorphic-ws' {
  export * from 'ws'
}
