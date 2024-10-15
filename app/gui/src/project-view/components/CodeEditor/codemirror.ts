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
export { forceLinting, lintGutter, linter, type Diagnostic } from '@codemirror/lint'
export { highlightSelectionMatches } from '@codemirror/search'
export { Annotation, EditorState, StateEffect, StateField, type ChangeSet } from '@codemirror/state'
export { EditorView, tooltips, type TooltipView } from '@codemirror/view'
export { type Highlighter } from '@lezer/highlight'
export { minimalSetup } from 'codemirror'
export { yCollab } from 'y-codemirror.next'
import { RawAstExtended } from '@/util/ast/extended'
import { RawAst } from '@/util/ast/raw'
import {
  Language,
  LanguageSupport,
  defineLanguageFacet,
  foldNodeProp,
  languageDataProp,
  syntaxTree,
} from '@codemirror/language'
import { type Diagnostic } from '@codemirror/lint'
import type { ChangeSpec } from '@codemirror/state'
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
import type { Diagnostic as LSDiagnostic } from 'ydoc-shared/languageServerTypes'
import { tryGetSoleValue } from 'ydoc-shared/util/data/iterable'
import type { SourceRangeEdit } from 'ydoc-shared/util/data/text'

/** TODO: Add docs */
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
    const from =
      (lineStartIndices[diagnostic.location.start.line] ?? 0) + diagnostic.location.start.character
    const to =
      (lineStartIndices[diagnostic.location.end.line] ?? 0) + diagnostic.location.end.character
    if (to > source.length || from > source.length) {
      // Suppress temporary errors if the source is not the version of the document the LS is reporting diagnostics for.
      continue
    }
    const severity =
      diagnostic.kind === 'Error' ? 'error'
      : diagnostic.kind === 'Warning' ? 'warning'
      : 'info'
    results.push({ from, to, message: diagnostic.message, severity })
  }
  return results
}

type AstNode = RawAstExtended<RawAst.Tree | RawAst.Token, false>

const nodeTypes: NodeType[] = [
  ...RawAst.Tree.typeNames.map((name, id) => NodeType.define({ id, name })),
  ...RawAst.Token.typeNames.map((name, id) =>
    NodeType.define({ id: id + RawAst.Tree.typeNames.length, name: 'Token' + name }),
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

  const childrenToConvert = tryGetSoleValue(children)?.isToken() ? [] : children

  const tree = new Tree(
    nodeSet.types[ast.inner.type + (ast.isToken() ? RawAst.Tree.typeNames.length : 0)]!,
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
    return {
      parsedPos: input.length,
      stopAt: () => {},
      stoppedAt: null,
      advance: () => {
        const code = input.read(0, input.length)
        if (code !== this.cachedCode || this.cachedTree == null) {
          this.cachedCode = code
          const ast = RawAstExtended.parse(code)
          this.cachedTree = astToCodeMirrorTree(this.nodeSet, ast, [[languageDataProp, facet]])
        }
        return this.cachedTree
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

/** TODO: Add docs */
export function enso() {
  return new LanguageSupport(ensoLanguage)
}

/** TODO: Add docs */
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

/** TODO: Add docs */
export function textEditToChangeSpec({ range: [from, to], insert }: SourceRangeEdit): ChangeSpec {
  return { from, to, insert }
}
