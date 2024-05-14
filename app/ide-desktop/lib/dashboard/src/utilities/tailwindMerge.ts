/** @file `tailwind-merge` with a custom configuration. */
import * as tailwindMerge from 'tailwind-merge'

// ===============
// === twMerge ===
// ===============

/** `twMerge` with a custom configuration. */
export const twMerge = tailwindMerge.extendTailwindMerge({
  extend: {
    classGroups: {
      m: [{ m: [() => true] }],
      p: [{ p: [() => true] }],
      w: [{ w: [() => true] }],
      h: [{ h: [() => true] }],
    },
  },
})
