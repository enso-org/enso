import type { SyntaxNode, TreeCursor } from '@lezer/common'

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

/** Represents the structure of a @{link Tree} in a JSON-compatible format. */
export type DebugTree = (string | DebugTree)[]

/** @returns A debug representation of the provided {@link Tree} */
export function debugTree(tree: { cursor: () => TreeCursor }, doc: string): DebugTree {
  const cursor = tree.cursor()
  let current: (string | DebugTree)[] = []
  const stack: (string | DebugTree)[][] = []
  cursor.iterate(
    (node) => {
      const child: (string | DebugTree)[] = [node.name]
      current.push(child)
      stack.push(current)
      current = child
    },
    (node) => {
      if (current.length === 1) current.push(doc.slice(node.from, node.to))
      current = stack.pop()!
    },
  )
  return current[0]! as DebugTree
}
