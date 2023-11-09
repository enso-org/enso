/**
 * # AST
 *
 * The goal of the new AST is to make expression edits simple.
 *
 * ## Nodes
 *
 * A node has an ExprId and other metadata, type-specific information, and a sequence of children. Children can be node
 * references or strings. String children are the tokens of the frontend AST. For example, an identifier node has a
 * single string child; a parser "macro" contains strings for its reserved words and and expression references for its
 * general expressions.
 *
 * Only nodes have ExprId assignments and thus are able to receive type info from the backend. Some things that are
 * only tokens in `enso-parser` have nodes in the expression tree, e.g. operator symbols.
 *
 * ### Referential transparency
 *
 * Nodes are not dependent on any auxiliary data structure. This allows them to be easily created and rearranged.
 *
 * ## The AST is abstract
 *
 * `Tree` is concrete; this is important for preserving information such as exact whitespace characters, but it is not
 * conducive to editing. The GUI's AST is abstract. Avoiding redundant information allows application logic to perform
 * edits without needing to maintain unnecessary invariants. When ASTs are printed to text, any tokens needed to
 * represent the structure are materialized (e.g. parentheses are inserted as needed.) During this printing process,
 * the `Tree`s corresponding to preexisting AST nodes are consulted, in order to ensure that the concrete syntax of
 * unmodified parts of the output matches the input. This is the only use of `Tree` in the frontend outside of the
 * process of translating from `Tree` to frontend AST.
 *
 * ## AST graph
 *
 * Nodes are stored in a map indexed by internal IDs.
 *
 * The internal IDs are distinct from ExprIds because their consistency is driven by different concerns: E.g. when the
 * frontend is modifying a value atomically, it does so by replacing a child expression [more on this in the
 * synchronization documentation, to be written]; however the backend can reevaluate more efficiently if this operation
 * doesn't change the ExprId, so the new node should have the same ExprID as the node it replaces.
 *
 * ## Executing edits
 *
 * ### Dirty-node overlay
 *
 * There are two maps holding nodes: A primary map contains committed nodes. A dirty-node overlay map contains new or
 * modified nodes. Application logic performs edits by inserting (or modifying) entries in the dirty-node map. When
 * resolving a node reference, the overlay map is always checked first.
 *
 * Dirty nodes are ephemeral; they can only be committed in the same frame that they are created. This avoids a
 * tree-edit becoming outdated by remote changes; all conflict resolution is the responsibility of the Yjs CDRT.
 *
 * ### Print-parse roundtripping
 *
 * It is important that the AST is always consistent with the AST that would result from parsing the corresponding
 * source code. To ensure this, after executing the changes of an edit but before committing them, the modified
 * subtree(s) of the AST are printed and reparsed.
 *
 * AST-printing produces two outputs: Source code, and an ID map attaching identities to spans. This map is read when
 * translating the parsed `Tree`s to AST nodes, and used to reattach IDs. In addition to spans it stores node-type
 * information, which is used to disambiguate nodes that contain the same code (i.e. when a node contains one other
 * node as a child and has no other tokens or whitespace). Update operations on the map are not needed; the map is
 * written once while printing, and read once while translating.
 *
 * ### Updating the committed-node map
 *
 * The primary map is only updated in response to Yjs events. When an edit is completed, all the nodes in the overlay
 * map are used to produce modifications to the Yjs data structures, and the overlay is cleared. By using the Yjs
 * notifications even for local changes, we avoid needing separate tree-update mechanisms for local and remote changes.
 *
 * [In the initial implementation, until the new AST is used for synchronization, local edits can be committed by
 * simply moving dirty nodes into the committed node map. Remote edits can recreate the committed-node map by parsing
 * the new code.]
 */

import { Token,Tree } from '@/generated/ast'
import { parseEnso } from '@/util/ast'
import type { LazyObject } from '@/util/parserSupport'

declare const brandAbstractNodeId: unique symbol
export type AbstractNodeId = number & { [brandAbstractNodeId]: never }

let nextNodeId = 0
function newNodeId(): AbstractNodeId {
  const id = nextNodeId
  nextNodeId++
  return id as AbstractNodeId
}

type NodeChild = { whitespace: string | undefined; node: AbstractNodeId | string }

type AbstractNode = {
  children: NodeChild[]
}

type NodeMap = Map<AbstractNodeId, AbstractNode>

function abstract(
  tree: Tree,
  info: InfoMap | undefined,
  nodesOut: NodeMap,
  source: string,
): [AbstractNodeId, string | undefined] {
  const children: NodeChild[] = []
  const visitor = (child: LazyObject) => {
    if (Tree.isInstance(child)) {
      const [childId, whitespace] = abstract(child, info, nodesOut, source)
      children.push({ whitespace, node: childId })
    } else if (Token.isInstance(child)) {
      const whitespaceStart = child.whitespaceStartInCodeBuffer
      const whitespaceEnd = whitespaceStart + child.whitespaceStartInCodeBuffer
      const whitespace = source.substring(whitespaceStart, whitespaceEnd)
      const codeStart = child.startInCodeBuffer
      const codeEnd = codeStart + child.lengthInCodeBuffer
      const node = source.substring(codeStart, codeEnd)
      children.push({ whitespace, node })
    } else {
      child.visitChildren(visitor)
    }
  }
  tree.visitChildren(visitor)
  const whitespaceStart = tree.whitespaceStartInCodeParsed
  const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed
  const whitespace = source.substring(whitespaceStart, whitespaceEnd)
  const codeStart = whitespaceEnd
  const codeEnd = codeStart + tree.childrenLengthInCodeParsed
  const span = [codeStart, codeEnd]
  const id = info?.get(span) ?? newNodeId()
  const node = { children }
  // XXX: We could skip adding a node to the overlay if it is unchanged.
  // - Use a smart map type that COWs its values and elides unnecessary changes.
  // (Alternatively, we could update the overlay unconditionally and check for no-op changes when syncing.)
  nodesOut.set(id, node)
  return [id, whitespace]
}

// TODO: Include tree/token type disambiguation
type Span = [number, number]

type InfoMap = Map<Span, AbstractNodeId>

function print(root: AbstractNodeId, nodes: NodeMap, info: InfoMap, offset?: number): string {
  let node = nodes.get(root)
  if (node == null) throw new Error('missing node?')
  let result = ''
  for (const child of node.children) {
    if (child.whitespace != null) {
      // TODO: Ensure indentation is appropriate for block.
      result += child.whitespace
    } else if (result.length != 0) {
      result += ' '
    }
    if (typeof child.node == 'string') {
      result += child.node
    } else {
      result += print(child.node, nodes, info, result.length)
    }
  }
  info.set([offset ?? 0, result.length], root)
  return result
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test('parse/print round trip', () => {
    const source = 'foo bar+baz'

    // Get an AST.
    const tree = parseEnso(source)
    const nodes = new Map<AbstractNodeId, AbstractNode>()
    const [root, _ws] = abstract(tree, undefined, nodes, source)

    // Print AST back to source.
    const info1 = new Map<Span, AbstractNodeId>()
    const printed = print(root, nodes, info1)
    expect(printed).toEqual(source)

    //throw new Error(JSON.stringify(Object.fromEntries(nodes)))

    // Re-parse.
    const tree1 = parseEnso(source)
    const [root1, _ws1] = abstract(tree1, info1, nodes, source)
    // Check that Identities match original AST.
    const info2 = new Map<Span, AbstractNodeId>()
    print(root, nodes, info2)
    expect(info2).toEqual(info1)
  })
}
