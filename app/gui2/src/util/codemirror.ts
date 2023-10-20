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
import { Ast, AstExtended } from '@/util/ast'
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
import { EditorView } from 'codemirror'

type AstNode = AstExtended<Ast.Tree | Ast.Token, false>

const nodeTypes: NodeType[] = [
  ...Ast.Tree.typeNames.map((name, id) => NodeType.define({ id, name })),
  ...Ast.Token.typeNames.map((name, id) =>
    NodeType.define({ id: id + Ast.Tree.typeNames.length, name: 'Token' + name }),
  ),
]

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

export const astProp = new NodeProp<AstNode>({ perNode: true })

function astToCodeMirrorTree(
  nodeSet: NodeSet,
  ast: AstNode,
  props?: readonly [number | NodeProp<any>, any][] | undefined,
): Tree {
  const [start, end] = ast.span()
  const children = ast.children()

  const hasSingleTokenChild =
    ast.whitespaceLength() === 0 && children.length === 1 && children[0]!.isToken()
  const childrenToConvert = hasSingleTokenChild ? [] : children

  const t = nodeSet.types[ast.inner.type + (ast.isToken() ? Ast.Tree.typeNames.length : 0)]!
  console.log(ast.treeTypeName(), t.name, hasSingleTokenChild)
  const tree = new Tree(
    nodeSet.types[ast.inner.type + (ast.isToken() ? Ast.Tree.typeNames.length : 0)]!,
    childrenToConvert.map((child) => astToCodeMirrorTree(nodeSet, child)),
    childrenToConvert.map((child) => child.span()[0] - start),
    end - start,
    [...(props ?? []), [astProp, ast]],
  )
  console.log(tree)
  return tree
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
          const ast = AstExtended.parse(code)
          console.log(ast.debug())
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
  create: (node: AstNode) => TooltipView | ((view: EditorView) => TooltipView) | null | undefined,
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
      arrow: true,
      create: typeof domOrCreate !== 'function' ? () => domOrCreate : domOrCreate,
    }
  })
}
