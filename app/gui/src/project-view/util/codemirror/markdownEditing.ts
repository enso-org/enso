/**
 * Editing markdown with CodeMirror.
 * Allows creating, removing and changing headers, lists and quotes.
 * There are a few assumptions and limitations:
 * - All edits are done after selecting the target range (or placing the cursor on the needed line)
 * - Headers and lists can be created by selecting a range. A header per line is created, and a list item per line.
 * - Changing between different header levels and list types is done by selecting the existing range of headers or lists.
 * - Editing sparse lists is not supported, in general. (e.g. when the list is separated by empty lines or non-list items)
 * - Editing elements in code blocks is supported, but no considerations are made to the actual syntax inside the code block.
 * - Quotes are created at the first selected line, and span multiple lines until the empty line.
 *   User must separate quoted text from the rest of the text by empty lines, manually.
 */

import { ChangeSpec } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { SyntaxNode, Tree } from '@lezer/common'
import { debugTree, markdownParser } from 'ydoc-shared/ast/ensoMarkdown'

/** Supported header levels. */
export type HeaderLevel = 1 | 2 | 3

class MutableChangeSet {
  private addList: ChangeSpec[] = []
  private replaceList: ChangeSpec[] = []
  private removeList: ChangeSpec[] = []
  public constructor(private offset: number) {}
  public merge(other: MutableChangeSet) {
    this.addList.push(...other.addList)
    this.replaceList.push(...other.replaceList)
    this.removeList.push(...other.removeList)
  }
  public add(from: number, to: number, insert: string) {
    this.addList.push({ from: from + this.offset, to: to + this.offset, insert })
  }
  public replace(from: number, to: number, insert: string) {
    this.replaceList.push({ from: from + this.offset, to: to + this.offset, insert })
  }
  public remove(from: number, to: number) {
    this.removeList.push({ from: from + this.offset, to: to + this.offset, insert: '' })
  }
  public dispatch(view: EditorView, suppressRemoveWhenAdding: boolean = false) {
    if (this.addList.length > 0 && suppressRemoveWhenAdding) this.removeList = []
    view.dispatch({ changes: [...this.addList, ...this.replaceList, ...this.removeList] })
  }
}

/** Resolve node at position, descending into the document if needed. */
function resolveNodeAtPos(tree: Tree, pos: number) {
  let node = tree.resolve(pos, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  return node
}

export function toggleHeader(view: EditorView, level: HeaderLevel) {
  const selection = view.state.selection.main
  const startLine = view.state.doc.lineAt(selection.from)
  const endLine = view.state.doc.lineAt(selection.to)
  const src = view.state.doc.toString()
  const tree = markdownParser.parse(src)
  const changeSet = new MutableChangeSet(0)
  for (let lineIndex = startLine.number; lineIndex <= endLine.number; lineIndex++) {
    const line = view.state.doc.line(lineIndex)
    const lineChanges = new MutableChangeSet(0)
    toggleHeaderInner(tree, level, line.from, line.to, src, lineChanges)
    changeSet.merge(lineChanges)
  }
  changeSet.dispatch(view, true)
}

function toggleHeaderInner(
  tree: Tree,
  level: number,
  lineStart: number,
  lineEnd: number,
  src: string,
  changeSet: MutableChangeSet,
) {
  const prefix = `${'#'.repeat(level)} `
  const node = resolveNodeAtPos(tree, lineEnd)
  const cursor = node.cursor()
  if (node.type.name === 'CodeText') {
    const codeText = src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const codeChanges = new MutableChangeSet(node.from)
    toggleHeaderInner(
      codeTree,
      level,
      lineStart - node.from,
      lineEnd - node.from,
      codeText,
      codeChanges,
    )
    changeSet.merge(codeChanges)
    return
  }
  do {
    if (cursor.type.name.startsWith('ATXHeading')) {
      const headerMark = cursor.node.getChild('HeaderMark')
      if (!headerMark) return
      const isLevelMatch = cursor.type.name.endsWith(level.toString())
      if (isLevelMatch) {
        changeSet.remove(headerMark.from, headerMark.to)
      } else {
        changeSet.replace(headerMark.from, headerMark.to, prefix)
      }
      return
    }
  } while (cursor.parent())
  changeSet.add(lineStart, lineStart, prefix)
}

export function toggleQuote(view: EditorView) {
  const changeSet = new MutableChangeSet(0)
  const src = view.state.doc.toString()
  const tree = markdownParser.parse(src)
  const selectionPos = view.state.selection.main.from
  const lineStart = view.state.doc.lineAt(selectionPos).from
  const node = resolveNodeAtPos(tree, selectionPos)
  if (node.type.name === 'CodeText') {
    const codeText = src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const codeChanges = new MutableChangeSet(node.from)
    toggleQuoteInner(codeTree, selectionPos - node.from, lineStart - node.from, codeChanges)
    changeSet.merge(codeChanges)
  } else {
    toggleQuoteInner(tree, selectionPos, lineStart, changeSet)
  }
  changeSet.dispatch(view)
}

function toggleQuoteInner(
  tree: Tree,
  selectionPos: number,
  lineStart: number,
  changeSet: MutableChangeSet,
) {
  const node = resolveNodeAtPos(tree, selectionPos)
  const cursor = node.cursor()
  do {
    if (cursor.type.name === 'EnsoBlockquote') {
      const quoteMark = cursor.node.getChild('QuoteMark')
      if (quoteMark == null) return
      changeSet.remove(quoteMark.from, quoteMark.to)
      return
    }
  } while (cursor.parent())
  changeSet.add(lineStart, lineStart, '> ')
}

export type ListType = 'unordered' | 'ordered'

export function toggleList(view: EditorView, type: ListType) {
  const tree = markdownParser.parse(view.state.doc.toString())
  const startLine = view.state.doc.lineAt(view.state.selection.main.from)
  const endLine = view.state.doc.lineAt(view.state.selection.main.to)
  const changeSet = new MutableChangeSet(0)
  let listIndex = 0
  for (let i = startLine.number; i <= endLine.number; i++) {
    const line = view.state.doc.line(i)
    const src = view.state.doc.toString()
    const lineChanges = new MutableChangeSet(0)
    toggleListInner(tree, listIndex, line.from, line.to, src, type, lineChanges)
    changeSet.merge(lineChanges)
    listIndex++
  }
  changeSet.dispatch(view, true)
}

function toggleListInner(
  tree: Tree,
  listIndex: number,
  lineStart: number,
  lineEnd: number,
  src: string,
  type: ListType,
  changeSet: MutableChangeSet,
) {
  const node = resolveNodeAtPos(tree, lineEnd)
  if (node.type.name === 'CodeText') {
    const codeText = src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const codeChanges = new MutableChangeSet(node.from)
    toggleListInner(
      codeTree,
      listIndex,
      lineStart - node.from,
      lineEnd - node.from,
      codeText,
      type,
      codeChanges,
    )
    changeSet.merge(codeChanges)
    return
  }
  const listInfo = detectList(node)
  if (listInfo != null && listInfo.listType === type) {
    changeSet.remove(listInfo.listMark.from, listInfo.listMark.to)
  } else if (listInfo != null && listInfo.listType !== type) {
    changeSet.replace(listInfo.listMark.from, listInfo.listMark.to, listMark(type, listIndex))
  } else if (listInfo == null) {
    changeSet.add(lineStart, lineStart, listMark(type, listIndex))
  }
}

function listMark(type: ListType, listIndex: number) {
  if (type === 'unordered') return '- '
  return `${listIndex + 1}. `
}

function detectList(
  node: SyntaxNode,
): { listMark: { from: number; to: number }; listType: ListType } | null {
  const cursor = node.cursor()
  let listMark: { from: number; to: number } | null = null
  do {
    if (cursor.type.name === 'ListItem') {
      const mark = cursor.node.getChild('ListMark')
      if (mark) listMark = { from: mark.from, to: mark.to }
    }
    if (cursor.type.name === 'BulletList') {
      return listMark != null ? { listMark, listType: 'unordered' } : null
    }
    if (cursor.type.name === 'OrderedList') {
      return listMark != null ? { listMark, listType: 'ordered' } : null
    }
  } while (cursor.parent())
  return null
}
