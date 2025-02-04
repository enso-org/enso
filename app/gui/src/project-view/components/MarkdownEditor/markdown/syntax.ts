import { markdown as markdownExtension } from '@codemirror/lang-markdown'
import {
  defineLanguageFacet,
  foldNodeProp,
  foldService,
  indentNodeProp,
  Language,
  languageDataProp,
  syntaxTree,
} from '@codemirror/language'
import { type Extension } from '@codemirror/state'
import { NodeProp, type NodeType, type Parser, type SyntaxNode } from '@lezer/common'
import { markdownParser } from 'ydoc-shared/ast/ensoMarkdown'

export const ensoMarkdownSyntax: () => Extension = () =>
  markdownExtension({
    base: mkLang(
      markdownParser.configure([
        commonmarkCodemirrorLanguageExtension,
        tableCodemirrorLanguageExtension,
      ]),
    ),
  })

function mkLang(parser: Parser) {
  return new Language(data, parser, [headerIndent], 'markdown')
}

const data = defineLanguageFacet({ commentTokens: { block: { open: '<!--', close: '-->' } } })

const headingProp = new NodeProp<number>()

const commonmarkCodemirrorLanguageExtension = {
  props: [
    foldNodeProp.add((type) => {
      return !type.is('Block') || type.is('Document') || isHeading(type) != null || isList(type) ?
          undefined
        : (tree, state) => ({ from: state.doc.lineAt(tree.from).to, to: tree.to })
    }),
    headingProp.add(isHeading),
    indentNodeProp.add({
      Document: () => null,
    }),
    languageDataProp.add({
      Document: data,
    }),
  ],
}

function isHeading(type: NodeType) {
  const match = /^(?:ATX|Setext)Heading(\d)$/.exec(type.name)
  return match ? +match[1]! : undefined
}

function isList(type: NodeType) {
  return type.name == 'OrderedList' || type.name == 'BulletList'
}

function findSectionEnd(headerNode: SyntaxNode, level: number) {
  let last = headerNode
  for (;;) {
    const next = last.nextSibling
    let heading
    if (!next || ((heading = isHeading(next.type)) != null && heading <= level)) break
    last = next
  }
  return last.to
}

const headerIndent = foldService.of((state, start, end) => {
  for (
    let node: SyntaxNode | null = syntaxTree(state).resolveInner(end, -1);
    node;
    node = node.parent
  ) {
    if (node.from < start) break
    const heading = node.type.prop(headingProp)
    if (heading == null) continue
    const upto = findSectionEnd(node, heading)
    if (upto > end) return { from: end, to: upto }
  }
  return null
})

const tableCodemirrorLanguageExtension = {
  props: [
    foldNodeProp.add({
      Table: (tree, state) => ({ from: state.doc.lineAt(tree.from).to, to: tree.to }),
    }),
  ],
}
