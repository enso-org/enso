import { MarkdownEdit } from '@/components/MarkdownEditor/codemirror/formatting/markdownEdit'
import {
  isBlockDelimiter,
  nodeRange,
  topLevelBlock,
  visitBlocks,
  visitLeafBlocks,
} from '@/components/MarkdownEditor/markdown/trees'
import {
  type DelimitedBlockType,
  type SupportedBlockType,
  isSupportedBlockType,
} from '@/components/MarkdownEditor/markdown/types'
import { syntaxTree } from '@codemirror/language'
import {
  type EditorState,
  Line,
  type SelectionRange,
  type TransactionSpec,
} from '@codemirror/state'
import { Range } from 'ydoc-shared/util/data/range'

/**
 * @returns The block type to report to the user for the currently-selected text. `undefined` if
 * the selection contains multiple block types, or is of an "unsupported" type that the parser
 * recognizes.
 */
export function getBlockType(state: EditorState): SupportedBlockType | undefined {
  const range = selectionRange(state.selection.main)
  const tree = syntaxTree(state)
  let type: string | undefined | null = undefined
  visitBlocks(range, tree, (node) => {
    if (type === undefined) type = node.name
    else if (node.name !== type) type = null
  })
  return type && isSupportedBlockType(type) ? type : undefined
}

function selectionRange(selection: SelectionRange): Range {
  return Range.unsafeFromBounds(selection.from, selection.to)
}

const blockDelimiter: Record<DelimitedBlockType & SupportedBlockType, string> = {
  // We can generally treat `Paragraph` as a line-delimited type with a 0-length delimiter.
  Paragraph: '',
  ATXHeading1: '# ',
  ATXHeading2: '## ',
  ATXHeading3: '### ',
  BulletList: '- ',
  OrderedList: '1. ',
  Blockquote: '> ',
}

/** Apply the specified block type to the selection. */
export function setBlockType(
  state: EditorState,
  type: DelimitedBlockType & SupportedBlockType,
): TransactionSpec {
  const selection = selectionRange(state.selection.main)
  const md = new MarkdownEdit(state.doc, syntaxTree(state))
  const delimiter = blockDelimiter[type]
  const cursor = md.tree.cursor()
  if (selection.empty && md.doc.lineAt(selection.from).length === 0)
    md.replace(selection, delimiter)
  visitLeafBlocks(selection, md.tree, (node, parentList) => {
    if (node.name === type || (node.name === 'ListItem' && parentList === type)) return
    const isQuote = node.name === 'Blockquote'
    cursor.moveTo(node.from, 1)
    for (;;) {
      const oldDelimiter =
        isBlockDelimiter(cursor.name) ? nodeRange(cursor) : Range.emptyAt(node.from)
      md.replace(oldDelimiter, delimiter)
      if (isQuote && cursor.nextSibling()) {
        while (cursor.name !== 'QuoteMark') {
          if (!cursor.nextSibling()) break
        }
        if (cursor.name === 'QuoteMark') continue
      }
      break
    }
  })
  const changes = state.changes(md.changes)
  return {
    changes,
    selection: state.selection.main.map(changes),
  }
}

/**
 * Insert a code block after the cursor, or if there is a selection convert the selected lines to a
 * code block.
 */
export function insertCodeBlock(state: EditorState): TransactionSpec {
  const selection = selectionRange(state.selection.main)
  const md = new MarkdownEdit(state.doc, syntaxTree(state))

  let newSelection: Range | undefined = undefined
  if (selection.empty) {
    const newCursorPos = insertCodeBlockAfter(md, selection.from)
    newSelection = Range.emptyAt(newCursorPos)
  } else {
    const lines = md.expandRangeToFullLines(selection)
    md.insert('```\n', lines.from)
    md.insert('\n```', lines.to)
  }

  const changes = state.changes(md.changes)
  return {
    changes,
    selection: newSelection ? rangeToSelection(newSelection) : state.selection.main.map(changes),
  }
}

function rangeToSelection(range: Range): { anchor: number; head: number } {
  return {
    anchor: range.from,
    head: range.to,
  }
}

function insertCodeBlockAfter(md: MarkdownEdit, pos: number): number {
  const currentBlock = topLevelBlock(md.tree, pos)
  const initialNewline = currentBlock ? '\n' : ''
  const insertAt = currentBlock?.to ?? pos
  const beforeCursor = '```\n'
  const afterCursor = '\n```'
  md.insert(initialNewline + beforeCursor + afterCursor, insertAt)
  return insertAt + initialNewline.length + beforeCursor.length
}

/** Remove the code block containing the cursor or within the current selection. */
export function removeCodeBlock(state: EditorState): TransactionSpec {
  const md = new MarkdownEdit(state.doc, syntaxTree(state))
  const currentBlock = topLevelBlock(md.tree, state.selection.main.anchor)
  if (!currentBlock) {
    console.error('Cannot remove code block: Cursor is not inside a block.')
    return {}
  }
  const cursor = md.tree.cursor()
  cursor.moveTo(currentBlock.from, 1)
  if (cursor.name !== 'CodeMark') {
    console.error('Cannot remove code block: Failed to locate opening delimiter')
    return {}
  }
  const firstLine = lineRange(md.doc.lineAt(currentBlock.from))
  md.remove(firstLine.from > 0 ? firstLine.expand(1, 0) : firstLine)
  cursor.moveTo(currentBlock.to, -1)
  if (cursor.name === 'CodeMark') {
    const lastLine = lineRange(md.doc.lineAt(currentBlock.to))
    md.remove(lastLine.expand(1, 0))
  }
  const changes = state.changes(md.changes)
  return {
    changes,
    selection: state.selection.main.map(changes),
  }
}

function lineRange(line: Line): Range {
  return Range.unsafeFromBounds(line.from, line.to)
}
