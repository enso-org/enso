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
export { lintGutter, linter, type Diagnostic } from '@codemirror/lint'
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
import { type Diagnostic } from '@codemirror/lint'
import { hoverTooltip as originalHoverTooltip, type TooltipView } from '@codemirror/view'
import {
  NodeProp,
  NodeSet,
  NodeType,
  Parser,
  Tree,
  type Input,
  type PartialParse,
  type SyntaxNode,
} from '@lezer/common'
import { styleTags, tags } from '@lezer/highlight'
import { EditorView } from 'codemirror'
import type { Diagnostic as LSDiagnostic } from 'shared/languageServerTypes'

export function lsDiagnosticsToCMDiagnostics(
  source: string,
  diagnostics: LSDiagnostic[],
): Diagnostic[] {
  if (!diagnostics.length) return []
  const results: Diagnostic[] = []
  let pos = 0
  const lineStartIndices = []
  for (const line of source.split('\n')) {
    lineStartIndices.push(pos)
    pos += line.length + 1
  }
  for (const diagnostic of diagnostics) {
    if (!diagnostic.location) continue
    results.push({
      from:
        (lineStartIndices[diagnostic.location.start.line] ?? 0) +
        diagnostic.location.start.character,
      to: (lineStartIndices[diagnostic.location.end.line] ?? 0) + diagnostic.location.end.character,
      message: diagnostic.message,
      severity:
        diagnostic.kind === 'Error' ? 'error' : diagnostic.kind === 'Warning' ? 'warning' : 'info',
    })
  }
  return results
}

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
    'Private!': tags.variableName,
    Number: tags.number,
    'Wildcard!': tags.variableName,
    'TextLiteral!': tags.string,
    OprApp: tags.operator,
    TokenOperator: tags.operator,
    'Assignment/TokenOperator': tags.definitionOperator,
    UnaryOprApp: tags.operator,
    'Function/Ident': tags.function(tags.variableName),
    ForeignFunction: tags.function(tags.variableName),
    'Import/TokenIdent': tags.function(tags.moduleKeyword),
    Export: tags.function(tags.moduleKeyword),
    Lambda: tags.function(tags.variableName),
    Documented: tags.docComment,
    ConstructorDefinition: tags.function(tags.variableName),
  }),
  foldNodeProp.add({
    Function: (node) => node.lastChild,
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

  const hasSingleTokenChild = children.length === 1 && children[0]!.isToken()
  const childrenToConvert = hasSingleTokenChild ? [] : children

  const tree = new Tree(
    nodeSet.types[ast.inner.type + (ast.isToken() ? Ast.Tree.typeNames.length : 0)]!,
    childrenToConvert.map((child) => astToCodeMirrorTree(nodeSet, child)),
    childrenToConvert.map((child) => child.span()[0] - start),
    end - start,
    [...(props ?? []), [astProp, ast]],
  )
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
  create: (
    ast: AstNode,
    syntax: SyntaxNode,
  ) => TooltipView | ((view: EditorView) => TooltipView) | null | undefined,
) {
  return originalHoverTooltip((view, pos, side) => {
    const syntaxNode = syntaxTree(view.state).resolveInner(pos, side)
    const astNode = syntaxNode.tree?.prop(astProp)
    if (astNode == null) return null
    const domOrCreate = create(astNode, syntaxNode)
    if (domOrCreate == null) return null

    return {
      pos: syntaxNode.from,
      end: syntaxNode.to,
      above: true,
      arrow: true,
      create: typeof domOrCreate !== 'function' ? () => domOrCreate : domOrCreate,
    }
  })
}
