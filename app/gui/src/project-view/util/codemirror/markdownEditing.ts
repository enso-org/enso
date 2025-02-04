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
import { Tree } from '@lezer/common'
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

export function toggleHeader(view: EditorView, level: HeaderLevel) {
  const startLine = view.state.doc.lineAt(view.state.selection.main.from)
  const endLine = view.state.doc.lineAt(view.state.selection.main.to)
  const tree = markdownParser.parse(view.state.doc.toString())
  const changeSet = new MutableChangeSet(0)
  for (let lineIndex = startLine.number; lineIndex <= endLine.number; lineIndex++) {
    const line = view.state.doc.line(lineIndex)
    const src = view.state.doc.toString()
    const result = toggleHeaderInner(tree, level, line.from, line.to, src, 0)
    changeSet.merge(result)
  }
  changeSet.dispatch(view, true)
}

function toggleHeaderInner(
  tree: Tree,
  level: number,
  lineStart: number,
  lineEnd: number,
  src: string,
  offset: number,
): MutableChangeSet {
  const changeSet = new MutableChangeSet(offset)
  console.log(debugTree(tree, src))
  console.log('Line: ', src.slice(lineStart, lineEnd))
  const prefix = '#'.repeat(level)
  let node = tree.resolve(lineEnd, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  console.log('Node type: ', node.type.name)
  if (node.type.name.startsWith('ATXHeading')) {
    const headerMark = node.getChild('HeaderMark')
    if (headerMark) {
      if (node.type.name.endsWith(level.toString())) {
        changeSet.remove(headerMark.from, headerMark.to)
      } else {
        changeSet.replace(headerMark.from, headerMark.to, prefix + ' ')
      }
    }
  } else if (node.type.name === 'CodeText') {
    const codeText = src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const result = toggleHeaderInner(
      codeTree,
      level,
      lineStart - node.from,
      lineEnd - node.from,
      codeText,
      node.from,
    )
    changeSet.merge(result)
  } else {
    changeSet.add(lineStart, lineStart, prefix + ' ')
  }
  return changeSet
}

export function toggleQuote(view: EditorView) {
  const changeSet = new MutableChangeSet(0)
  const src = view.state.doc.toString()
  const tree = markdownParser.parse(src)
  const selectionPos = view.state.selection.main.from
  let node = tree.resolve(selectionPos, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  if (node.type.name === 'CodeText') {
    const codeText = src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const lineStart = view.state.doc.lineAt(selectionPos).from
    const result = toggleQuoteInner(
      codeTree,
      selectionPos - node.from,
      lineStart - node.from,
      codeText,
      node.from,
    )
    changeSet.merge(result)
  } else {
    const lineStart = view.state.doc.lineAt(selectionPos).from
    const result = toggleQuoteInner(tree, selectionPos, lineStart, src, 0)
    changeSet.merge(result)
  }
  changeSet.dispatch(view)
}

function toggleQuoteInner(
  tree: Tree,
  selectionPos: number,
  lineStart: number,
  src: string,
  offset: number,
): MutableChangeSet {
  const changeSet = new MutableChangeSet(offset)
  console.log(debugTree(tree, src))
  let node = tree.resolve(selectionPos, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  console.log('Node type: ', node.type.name)
  console.log('Line start: ', lineStart)
  const cursor = node.cursor()
  do {
    if (cursor.type.name === 'EnsoBlockquote') {
      const quoteMark = cursor.node.getChild('QuoteMark')
      if (quoteMark != null) {
        changeSet.remove(quoteMark.from, quoteMark.to)
        return changeSet
      }
    }
  } while (cursor.parent())
  changeSet.add(lineStart, lineStart, '> ')
  return changeSet
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
    const result = toggleListInner(tree, listIndex, line.from, line.to, src, type, 0)
    changeSet.merge(result)
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
  offset: number,
): MutableChangeSet {
  const changeSet = new MutableChangeSet(offset)
  let node = tree.resolve(lineEnd, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  let listMark: false | { from: number; to: number } = false
  let listType: undefined | ListType = undefined
  console.log(debugTree(tree, src))
  console.log('Line: ', src.slice(lineStart, lineEnd))
  console.log('Node type: ', node.type.name)
  if (node.type.name === 'CodeText') {
    const codeText = src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const result = toggleListInner(
      codeTree,
      listIndex,
      lineStart - node.from,
      lineEnd - node.from,
      codeText,
      type,
      node.from,
    )
    changeSet.merge(result)
    return changeSet
  }
  const cursor = node.cursor()
  do {
    if (cursor.type.name === 'ListItem') {
      const mark = cursor.node.getChild('ListMark')
      if (mark) {
        listMark = { from: mark.from, to: mark.to }
      }
    }
    if (cursor.type.name === 'BulletList') {
      listType = 'unordered'
      break
    }
    if (cursor.type.name === 'OrderedList') {
      listType = 'ordered'
      break
    }
  } while (cursor.parent())
  if (listMark && listType === type) {
    changeSet.remove(listMark.from, listType === 'ordered' ? listMark.to + 1 : listMark.to)
  } else if (listMark && listType !== type) {
    changeSet.replace(listMark.from, listMark.to, type === 'unordered' ? '-' : `${listIndex + 1}. `)
  } else if (!listMark) {
    changeSet.add(lineStart, lineStart, type === 'unordered' ? '- ' : `${listIndex + 1}. `)
  }
  return changeSet
}
