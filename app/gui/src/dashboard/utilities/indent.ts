/** @file Utilities for indenting elements. */

// =================
// === Constants ===
// =================

/**
 * An explicit list of classes  used for indentation. This is required so that Tailwind includes
 * the classes in the final CSS file.
 */
const INDENT_CLASSES = [
  '',
  'ml-indent-1',
  'ml-indent-2',
  'ml-indent-3',
  'ml-indent-4',
  'ml-indent-5',
  'ml-indent-6',
  'ml-indent-7',
  'ml-indent-8',
  'ml-indent-9',
  'ml-indent-10',
]
const FALLBACK_INDENT_CLASS = 'ml-indent-10'

/** Returns the appropriate Tailwind class for the given amount of nesting.. */
export function indentClass(depth: number): string {
  return INDENT_CLASSES[depth] ?? FALLBACK_INDENT_CLASS
}
