/**
 * @file This module is a collection of codemirror related imports that are intended to be loaded
 * asynchronously using a single dynamic import, allowing for code splitting.
 */

export { defaultKeymap } from '@codemirror/commands'
export {
  bracketMatching,
  defaultHighlightStyle,
  foldNodeProp,
  syntaxHighlighting,
} from '@codemirror/language'
export { EditorState } from '@codemirror/state'
export { EditorView } from '@codemirror/view'
export { type Highlighter } from '@lezer/highlight'
export { minimalSetup } from 'codemirror'
export { yCollab } from 'y-codemirror.next'
import { Ast, childrenAstNodes, parseEnso } from '@/util/ast'
import {
  Language,
  LanguageSupport,
  defineLanguageFacet,
  foldInside,
  foldNodeProp,
  languageDataProp,
} from '@codemirror/language'
import {
  NodeSet,
  NodeType,
  Parser,
  Tree,
  type Input,
  type NodeProp,
  type PartialParse,
} from '@lezer/common'
import { styleTags, tags } from '@lezer/highlight'

// TODO: hover tooltips

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
    BodyBlock: foldInside,
    ArgumentBlockApplication: foldInside,
    OperatorBlockApplication: foldInside,
  }),
)

function astToCodeMirrorTree(
  nodeSet: NodeSet,
  tree: Ast.Tree,
  props?: readonly [number | NodeProp<any>, any][] | undefined,
): Tree {
  const begin = tree.whitespaceStartInCodeParsed
  return new Tree(
    nodeSet.types[tree.type]!,
    Array.from(childrenAstNodes(tree), (tree) => astToCodeMirrorTree(nodeSet, tree)),
    Array.from(childrenAstNodes(tree), (child) => child.whitespaceStartInCodeParsed - begin),
    tree.whitespaceLengthInCodeParsed,
    props,
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
        if (code === self.cachedCode && self.cachedTree != null) {
          return self.cachedTree
        }
        self.cachedCode = code
        const ast = parseEnso(code)
        console.log(ast)
        return (self.cachedTree = astToCodeMirrorTree(self.nodeSet, ast, [
          [languageDataProp, facet],
        ]))
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
