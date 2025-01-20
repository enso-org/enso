import { type SyntaxNode, type TreeCursor } from '@lezer/common'

/**
 * Compares the value of `cursor.name` to the provided value. This can be used instead of reading the field directly to
 * avoid incorrect type narrowing and related spurious type errors caused by the side-effect-based operation of the
 * cursor API. See: https://github.com/microsoft/TypeScript/issues/9998
 */
export function isNodeType(cursor: TreeCursor, type: string): boolean {
  return cursor.name === type
}

/** Yields the provided node, and its parents recursively. */
export function* syntaxNodeAncestors(syn: SyntaxNode | null) {
  let currentSyn: SyntaxNode | null = syn
  while (currentSyn != null) {
    yield currentSyn
    currentSyn = currentSyn.parent
  }
}
