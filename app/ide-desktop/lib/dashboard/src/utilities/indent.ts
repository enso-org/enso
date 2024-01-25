/** @file Utilities for indenting elements. */

// =================
// === Constants ===
// =================

/** An explicit list of classes  used for indentation. This is required so that Tailwind includes
 * the classes in the final CSS file. */
const INDENT_CLASSES = [
  '',
  'ml-6',
  'ml-12',
  'ml-18',
  'ml-24',
  'ml-30',
  'ml-36',
  'ml-42',
  'ml-48',
  'ml-54',
  'ml-60',
]
const FALLBACK_INDENT_CLASS = 'w-60'

/** Returns the appropriate Tailwind class for the given amount of nesting.. */
export function indentClass(depth: number): string {
  return INDENT_CLASSES[depth] ?? FALLBACK_INDENT_CLASS
}
