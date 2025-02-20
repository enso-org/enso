import { Ast } from '@/util/ast'
import {
  defineLanguageFacet,
  foldNodeProp,
  indentUnit,
  Language,
  languageDataProp,
  LanguageSupport,
} from '@codemirror/language'
import { type Extension } from '@codemirror/state'
import {
  type Input,
  NodeProp,
  NodeSet,
  NodeType,
  Parser,
  type PartialParse,
  Tree,
} from '@lezer/common'
import { styleTags, tags } from '@lezer/highlight'
import { type Ref } from 'vue'
import { spanMapToSpanGetter, tokenSpanGetter } from 'ydoc-shared/ast/idMap'
import { assertDefined } from 'ydoc-shared/util/assert'
import { rangeLength, type SourceRange } from 'ydoc-shared/util/data/text'

const treeTypeNames = Ast.astTypes
const tokenTypeNames = Ast.tokenTypes.map((name) => `Token${name}`)
const nodeTypes: NodeType[] = [...treeTypeNames, ...tokenTypeNames].map((name, id) =>
  NodeType.define({ id, name }),
)

const nodeSet = new NodeSet(nodeTypes).extend(
  styleTags({
    Ident: tags.variableName,
    Wildcard: tags.variableName,
    TokenIdent: tags.name,
    'PropertyAccess/TokenIdent': tags.propertyName,
    'NumericLiteral!': tags.number,
    'FunctionDef/Ident': tags.definition(tags.function(tags.variableName)),
    'Assignment/Ident': tags.definition(tags.variableName),
    'Import/TokenIdent': tags.moduleKeyword,
    // Strings and comments
    'TextLiteral!': tags.string,
    TokenTextEscape: tags.escape,
    TokenTextStart: tags.docComment,
    TokenTextSection: tags.docComment,
    TokenNewline: tags.lineComment,
    TokenInvalid: tags.invalid,
    // Open/close tokens
    'Group/TokenOpenSymbol': tags.paren,
    'Group/TokenCloseSymbol': tags.paren,
    'Vector/TokenOpenSymbol': tags.squareBracket,
    'Vector/TokenCloseSymbol': tags.squareBracket,
    // Operator tokens
    TokenOperator: tags.operator,
    TokenDotOperator: tags.operator,
    TokenCommaOperator: tags.separator,
    TokenUnaryOperator: tags.operator,
    TokenAnnotationOperator: tags.operator,
    TokenAutoscopeOperator: tags.operator,
    TokenLambdaOperator: tags.function(tags.controlOperator),
    TokenSuspendedDefaultArguments: tags.controlOperator,
    TokenSuspensionOperator: tags.controlOperator,
    TokenAssignmentOperator: tags.definitionOperator,
    TokenTypeAnnotationOperator: tags.typeOperator,
    TokenArrowOperator: tags.typeOperator,
    TokenNegationOperator: tags.number,
    // Keyword tokens
    TokenAllKeyword: tags.moduleKeyword,
    TokenCaseKeyword: tags.controlKeyword,
    TokenOfKeyword: tags.controlKeyword,
    TokenPrivateKeyword: tags.modifier,
    TokenTypeKeyword: tags.definitionKeyword,
    TokenForeignKeyword: tags.modifier,
  }),
  foldNodeProp.add({
    BodyBlock: (node) => (node.from === 0 ? null : node),
  }),
)
const typeByName = new Map(nodeSet.types.map((type) => [type.name, type]))

export type AstNode = Ast.Ast | Ast.Token
export const astProp = new NodeProp<AstNode>({ perNode: true })

function astToCodeMirrorTree(
  ast: Ast.Ast,
  getSpan: (id: Ast.AstId) => SourceRange | undefined,
  getTokenSpan: (token: Ast.Token) => SourceRange | undefined,
  props?: readonly [number | NodeProp<any>, any][] | undefined,
): Tree {
  const nodeType = typeByName.get(ast.typeName)
  assertDefined(nodeType)
  const children = new Array<Tree>()
  const childrenPositions = new Array<number>()
  const { from, to } = getSpan(ast.id)!
  for (const child of ast.children()) {
    if (child instanceof Ast.Ast) {
      children.push(astToCodeMirrorTree(child, getSpan, getTokenSpan))
      childrenPositions.push(getSpan(child.id)!.from - from)
    } else {
      if (child.code().length === 0) continue
      const childSpan = getTokenSpan(child)
      assertDefined(childSpan)
      const tokenTree = tokenToCodeMirrorTree(child, childSpan)
      if (tokenTree) {
        children.push(tokenTree)
        childrenPositions.push(childSpan.from - from)
      }
    }
  }
  return new Tree(nodeType, children, childrenPositions, to - from, [
    ...(props ?? []),
    [astProp, ast],
  ])
}

function tokenToCodeMirrorTree(token: Ast.Token, span: SourceRange): Tree | undefined {
  if (token.typeName === 'Raw') return
  const type = typeByName.get(`Token${token.typeName}`)
  assertDefined(type)
  return new Tree(type, [], [], rangeLength(span), [[astProp, token]])
}

const facet = defineLanguageFacet()

class EnsoParser extends Parser {
  private cachedCode: string | undefined
  private cachedTree: Tree | undefined
  constructor(private readonly moduleRoot: Readonly<Ref<Ast.BodyBlock | undefined>>) {
    super()
  }
  createParse(input: Input): PartialParse {
    return {
      parsedPos: input.length,
      stopAt: () => {},
      stoppedAt: null,
      advance: () => {
        const code = input.read(0, input.length)
        if (code !== this.cachedCode || this.cachedTree == null) {
          this.cachedCode = code
          assertDefined(this.moduleRoot.value)
          const root = Ast.copyIntoNewModule(this.moduleRoot.value)
          const tempModule = root.module
          root.module.setRoot(root)
          root.syncToCode(code)
          const parsedRoot = tempModule.root()
          assertDefined(parsedRoot)
          const { info: spans } = Ast.printWithSpans(parsedRoot)
          const getSpan = spanMapToSpanGetter(spans.nodes)
          const getTokenSpan = tokenSpanGetter(spans.tokens)
          this.cachedTree = astToCodeMirrorTree(parsedRoot, getSpan, getTokenSpan, [
            [languageDataProp, facet],
          ])
        }
        return this.cachedTree
      },
    }
  }
}

/** TODO: Add docs */
export function ensoSyntax(moduleRoot: Readonly<Ref<Ast.BodyBlock | undefined>>): Extension {
  return new LanguageSupport(new Language(facet, new EnsoParser(moduleRoot)), [
    indentUnit.of('    '),
  ])
}
