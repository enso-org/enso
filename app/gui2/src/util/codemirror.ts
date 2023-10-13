/**
 * @file This module is a collection of codemirror related imports that are intended to be loaded
 * asynchronously using a single dynamic import, allowing for code splitting.
 */

export { defaultKeymap } from '@codemirror/commands'
export { EditorState } from '@codemirror/state'
export { EditorView } from '@codemirror/view'
export { classHighlighter } from '@lezer/highlight'
export { minimalSetup } from 'codemirror'
export { yCollab } from 'y-codemirror.next'
import { childrenAstNodes, parseEnso, type Ast } from '@/util/ast'
import { NodeType, Parser, Tree, type Input, type PartialParse } from '@lezer/common'

const nodeTypeCache = new Map<Ast.Tree.Type, NodeType>()

function nodeType(tree: Ast.Tree) {
  const cached = nodeTypeCache.get(tree.type)
  if (cached != null) return cached
  const nodeType = NodeType.define({ id: tree.type, name: tree.constructor.name })
  nodeTypeCache.set(tree.type, nodeType)
  return nodeType
}

function astToCodeMirrorTree(tree: Ast.Tree): Tree {
  const begin = tree.spanLeftOffsetCodeReprBegin
  return new Tree(
    nodeType(tree),
    Array.from(childrenAstNodes(tree), astToCodeMirrorTree),
    Array.from(childrenAstNodes(tree), (child) => child.spanLeftOffsetCodeReprBegin - begin),
    tree.spanLeftOffsetCodeReprLen,
  )
}

export class EnsoParser extends Parser {
  createParse(input: Input): PartialParse {
    const tree = astToCodeMirrorTree(parseEnso(input.read(0, input.length)))
    return {
      parsedPos: input.length,
      stopAt() {},
      stoppedAt: null,
      advance() {
        return tree
      },
    }
  }
}
