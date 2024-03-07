/** @file @{link pkg} init export. */

// Following imports are only valid in context of the built package. Here, those are guaranteed to
// not resolve correctly, so we need to disable the type checking.

// @ts-expect-error
import init from './pkg.js'
// @ts-expect-error
export * from './runtime-libs'

export { init }
