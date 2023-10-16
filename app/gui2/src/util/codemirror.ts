/**
 * @file This module is a collection of codemirror related imports that are intended to be loaded
 * asynchronously using a single dynamic import, allowing for code splitting.
 */

export { defaultKeymap } from '@codemirror/commands'
export {
  bracketMatching,
  defaultHighlightStyle,
  foldGutter,
  foldNodeProp,
  syntaxHighlighting,
} from '@codemirror/language'
export { highlightSelectionMatches } from '@codemirror/search'
export { EditorState } from '@codemirror/state'
export { EditorView, tooltips, type TooltipView } from '@codemirror/view'
export { type Highlighter } from '@lezer/highlight'
export { minimalSetup } from 'codemirror'
export { yCollab } from 'y-codemirror.next'
import { Ast, childrenAstNodes, parseEnso } from '@/util/ast'
import {
  Language,
  LanguageSupport,
  defineLanguageFacet,
  foldNodeProp,
  languageDataProp,
  syntaxTree,
} from '@codemirror/language'
import { hoverTooltip as originalHoverTooltip, type TooltipView } from '@codemirror/view'
import {
  NodeProp,
  NodeSet,
  NodeType,
  Parser,
  Tree,
  type Input,
  type PartialParse,
} from '@lezer/common'
import { styleTags, tags } from '@lezer/highlight'
import type { EditorView } from 'codemirror'

const nodeTypes: NodeType[] = []
for (const potentialAstNodeType of Object.values(Ast.Tree)) {
  if (
    'prototype' in potentialAstNodeType &&
    potentialAstNodeType.prototype instanceof Ast.Tree.AbstractBase &&
    potentialAstNodeType !== Ast.Tree.AbstractBase
  ) {
    const view = new DataView(new Uint8Array().buffer)
    const tree = new (potentialAstNodeType as new (
      view: DataView,
    ) => Ast.Tree.AbstractBase & { type: Ast.Tree.Type })(view)
    nodeTypes.push(NodeType.define({ id: tree.type, name: tree.constructor.name }))
  }
}

const nodeSet = new NodeSet(nodeTypes).extend(
  styleTags({
    Ident: tags.variableName,
    Private: tags.variableName,
    Number: tags.number,
    Wildcard: tags.variableName,
    TextLiteral: tags.string,
    OprApp: tags.operator,
    UnaryOprApp: tags.operator,
    Function: tags.function(tags.variableName),
    ForeignFunction: tags.function(tags.variableName),
    Import: tags.function(tags.moduleKeyword),
    Export: tags.function(tags.moduleKeyword),
    Lambda: tags.function(tags.variableName),
    Documented: tags.docComment,
    ConstructorDefinition: tags.function(tags.variableName),
  }),
  foldNodeProp.add({
    Function: (node) => node,
    ArgumentBlockApplication: (node) => node,
    OperatorBlockApplication: (node) => node,
  }),
)

export const astProp = new NodeProp<Ast.Tree>({ perNode: true })

function astToCodeMirrorTree(
  nodeSet: NodeSet,
  tree: Ast.Tree,
  props?: readonly [number | NodeProp<any>, any][] | undefined,
): Tree {
  const begin = tree.whitespaceStartInCodeParsed + tree.whitespaceLengthInCodeParsed
  return new Tree(
    nodeSet.types[tree.type]!,
    Array.from(childrenAstNodes(tree), (tree) => astToCodeMirrorTree(nodeSet, tree)),
    Array.from(
      childrenAstNodes(tree),
      (child) => child.whitespaceStartInCodeParsed + child.whitespaceLengthInCodeParsed - begin,
    ),
    tree.childrenLengthInCodeParsed,
    [...(props ?? []), [astProp, tree]],
  )
}

const facet = defineLanguageFacet()

class EnsoParser extends Parser {
  nodeSet
  constructor() {
    super()
    this.nodeSet = nodeSet
  }
  cachedCode: string | undefined
  cachedTree: Tree | undefined
  createParse(input: Input): PartialParse {
    const self = this
    return {
      parsedPos: input.length,
      stopAt() {},
      stoppedAt: null,
      advance() {
        const code = input.read(0, input.length)
        if (code !== self.cachedCode || self.cachedTree == null) {
          self.cachedCode = code
          const ast = parseEnso(code)
          self.cachedTree = astToCodeMirrorTree(self.nodeSet, ast, [[languageDataProp, facet]])
        }
        return self.cachedTree
      },
    }
  }
}

class EnsoLanguage extends Language {
  constructor() {
    super(facet, new EnsoParser())
  }
}

const ensoLanguage = new EnsoLanguage()

export function enso() {
  return new LanguageSupport(ensoLanguage)
}

export function hoverTooltip(
  create: (node: Ast.Tree) => TooltipView | ((view: EditorView) => TooltipView) | null | undefined,
) {
  return originalHoverTooltip((view, pos, side) => {
    const node = syntaxTree(view.state).resolveInner(pos, side)
    const ast = node.tree?.prop(astProp)
    if (ast == null) return null
    const domOrCreate = create(ast)
    if (domOrCreate == null) return null
    return {
      pos: node.from,
      end: node.to,
      above: true,
      create: typeof domOrCreate !== 'function' ? () => domOrCreate : domOrCreate,
    }
  })
}
