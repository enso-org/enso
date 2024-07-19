/** @file `tailwind-merge` with a custom configuration. */
import * as tailwindMerge from 'tailwind-merge'

// =================
// === Constants ===
// =================

export const TAILWIND_MERGE_CONFIG = {
  extend: {
    classGroups: {
      m: [{ m: [() => true] }],
      p: [{ p: [() => true] }],
      w: [{ w: [() => true] }],
      h: [{ h: [() => true] }],
      size: [{ size: [() => true] }],
    },
    conflictingClassGroups: {
      size: ['w', 'h'] as const,
    },
  },
}

// ===============
// === twMerge ===
// ===============

/** `twMerge` with a custom configuration. */
// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const twMerge = tailwindMerge.extendTailwindMerge(TAILWIND_MERGE_CONFIG)
// reexporting twJoin from the original library for convenience.
// eslint-disable-next-line no-restricted-syntax
export const twJoin = tailwindMerge.twJoin
