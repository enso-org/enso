import { markdown } from '@codemirror/lang-markdown'
import { foldNodeProp } from '@codemirror/language'
import type { Extension } from '@codemirror/state'
import { ensoMarkdownExtension } from 'ydoc-shared/ast/ensoMarkdown'

const tableCodemirrorLanguageExtension = {
  props: [
    foldNodeProp.add({
      Table: (tree, state) => ({ from: state.doc.lineAt(tree.from).to, to: tree.to }),
    }),
  ],
}

const extension = markdown({
  extensions: [ensoMarkdownExtension, tableCodemirrorLanguageExtension],
})

export const ensoMarkdownSyntax = (): Extension => extension
