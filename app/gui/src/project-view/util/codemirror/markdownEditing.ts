/**
 * Editing markdown with CodeMirror.
 * Allows creating, removing and changing headers, lists and quotes.
 *
 * One trick used throughout this module implementation is related to editing markdown inside code blocks.
 * If we detect such edit, internals of the codeblock are reparsed as a separate markdown document, and the edits
 * are applied to it, adjusting text positions as needed. It allows to reuse the common code while providing
 * basic support for editing codeblocks.
 */

import { ChangeSpec, Line } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { SyntaxNode, Tree } from '@lezer/common'
import { markdownParser } from 'ydoc-shared/ast/ensoMarkdown'

// ===============
// === Helpers ===
// ===============

/**
 * Helper class for managing changes to the markdown document.
 * Recorded changes can be adjusted by some `offset` for editing subparts of the document, like internals of codeblocks.
 */
class MutableChangeSet {
  private addList: ChangeSpec[] = []
  private replaceList: ChangeSpec[] = []
  private removeList: ChangeSpec[] = []

  public constructor(private offset: number) {}

  /** Merge changes from another changeset */
  public merge(other: MutableChangeSet) {
    this.addList.push(...other.addList)
    this.replaceList.push(...other.replaceList)
    this.removeList.push(...other.removeList)
  }

  /** Record a change adding text at the given position. */
  public add(from: number, to: number, insert: string) {
    this.addList.push({ from: from + this.offset, to: to + this.offset, insert })
  }

  /** Record a change replacing text at the given position. */
  public replace(from: number, to: number, insert: string) {
    this.replaceList.push({ from: from + this.offset, to: to + this.offset, insert })
  }

  /** Record a change removing text at the given position. */
  public remove(from: number, to: number) {
    this.removeList.push({ from: from + this.offset, to: to + this.offset, insert: '' })
  }

  /**
   * Dispatch the recorded changes to the editor.
   * If `suppressRemoveWhenAdding` is true, `remove` changes are only applied when there are no `add` changes.
   * It is used when toggling multiple lines at once, to make them consistent on first editing.
   * (consider editing two lines at once using `toggleHeader` when one line is already a header, the other is not)
   */
  public dispatch(view: EditorView, suppressRemoveWhenAdding: boolean = false) {
    if (this.addList.length > 0 && suppressRemoveWhenAdding) this.removeList = []
    view.dispatch({ changes: [...this.addList, ...this.replaceList, ...this.removeList] })
  }
}

/** Helper to reduce the number of arguments in functions. Simply wraps common arguments. */
class Context {
  private offset: number = 0
  public constructor(
    public tree: Tree,
    public src: string,
    public line: Line,
  ) {}
  public withOffset(offset: number): Context {
    const newContext = new Context(this.tree, this.src, this.line)
    newContext.offset = offset
    return newContext
  }
  public lineStart() {
    return this.line.from - this.offset
  }
  public lineEnd() {
    return this.line.to - this.offset
  }
  public lineText() {
    return this.line.text
  }
  public makeChangeSet() {
    return new MutableChangeSet(this.offset)
  }
}

/** Resolve node at position, descending into the document if needed. */
function resolveNodeAtPos(tree: Tree, pos: number) {
  let node = tree.resolve(pos, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  return node
}

/** Check whether via are inside a markdown code block. */
function isCodeText(node: SyntaxNode) {
  return node.type.name === 'CodeText'
}

// ===============
// === Headers ===
// ===============

/** Supported header levels. */
export type HeaderLevel = 1 | 2 | 3

/** Toggle headers of specified level at each of the selected lines. */
export function toggleHeader(view: EditorView, level: HeaderLevel) {
  const selection = view.state.selection.main
  const startLine = view.state.doc.lineAt(selection.from)
  const endLine = view.state.doc.lineAt(selection.to)
  const src = view.state.doc.toString()
  const tree = markdownParser.parse(src)
  const changeSet = new MutableChangeSet(0)
  for (let lineIndex = startLine.number; lineIndex <= endLine.number; lineIndex++) {
    const line = view.state.doc.line(lineIndex)
    const context = new Context(tree, src, line)
    const lineChanges = toggleHeaderInner(context, level)
    changeSet.merge(lineChanges)
  }
  changeSet.dispatch(view, true)
}

function toggleHeaderInner(context: Context, level: number): MutableChangeSet {
  const prefix = `${'#'.repeat(level)} `
  const node = resolveNodeAtPos(context.tree, context.lineEnd())
  const changeSet = context.makeChangeSet()
  if (isCodeText(node)) {
    const codeText = context.src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const codeContext = new Context(codeTree, codeText, context.line).withOffset(node.from)
    const codeChanges = toggleHeaderInner(codeContext, level)
    changeSet.merge(codeChanges)
  } else {
    const headerMark = findAtxHeaderMark(node)
    if (headerMark && headerMark.level === level) {
      changeSet.remove(headerMark.from, headerMark.to)
    } else if (headerMark) {
      changeSet.replace(headerMark.from, headerMark.to, prefix)
    } else {
      changeSet.add(context.lineStart(), context.lineStart(), prefix)
    }
  }
  return changeSet
}

function findAtxHeaderMark(node: SyntaxNode): { level: number; from: number; to: number } | null {
  const cursor = node.cursor()
  do {
    if (cursor.type.name.startsWith('ATXHeading')) {
      const headerMark = cursor.node.getChild('HeaderMark')
      if (!headerMark) return null
      const level = Number(cursor.type.name.slice(-1))
      return { level, from: headerMark.from, to: headerMark.to }
    }
  } while (cursor.parent())
  return null
}

// =============
// === Lists ===
// =============

/** Distinguish between unordered (bullet) and ordered (numbered) lists. */
export type ListType = 'unordered' | 'ordered'

/** Toggle list items of specified type at each of the selected lines. */
export function toggleList(view: EditorView, type: ListType) {
  const tree = markdownParser.parse(view.state.doc.toString())
  const startLine = view.state.doc.lineAt(view.state.selection.main.from)
  const endLine = view.state.doc.lineAt(view.state.selection.main.to)
  const changeSet = new MutableChangeSet(0)
  const src = view.state.doc.toString()
  let listIndex = 0
  for (let i = startLine.number; i <= endLine.number; i++) {
    const line = view.state.doc.line(i)
    const context = new Context(tree, src, line)
    const lineChanges = toggleListInner(context, listIndex, type)
    changeSet.merge(lineChanges)
    listIndex++
  }
  changeSet.dispatch(view, true)
}

function toggleListInner(context: Context, listIndex: number, type: ListType): MutableChangeSet {
  const node = resolveNodeAtPos(context.tree, context.lineEnd())
  const changeSet = context.makeChangeSet()
  if (isCodeText(node)) {
    const codeText = context.src.slice(node.from, node.to)
    const codeTree = markdownParser.parse(codeText)
    const codeContext = new Context(codeTree, codeText, context.line).withOffset(node.from)
    const codeChanges = toggleListInner(codeContext, listIndex, type)
    changeSet.merge(codeChanges)
  } else {
    const listInfo = detectList(node)
    if (listInfo != null && listInfo.listType === type) {
      changeSet.remove(listInfo.listMark.from, listInfo.listMark.to)
    } else if (listInfo != null && listInfo.listType !== type) {
      changeSet.replace(listInfo.listMark.from, listInfo.listMark.to, listMark(type, listIndex))
    } else if (listInfo == null) {
      changeSet.add(context.lineStart(), context.lineStart(), listMark(type, listIndex))
    }
  }
  return changeSet
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

// ==============
// === Quotes ===
// ==============

/** Toggle markdown quote mark at the first selected line. */
export function toggleQuote(view: EditorView) {
  const changeSet = new MutableChangeSet(0)
  const src = view.state.doc.toString()
  const tree = markdownParser.parse(src)
  const selectionPos = view.state.selection.main.from
  const lineStart = view.state.doc.lineAt(selectionPos).from
  const node = resolveNodeAtPos(tree, selectionPos)
  if (isCodeText(node)) {
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
  const quoteMark = findQuoteMark(node)
  if (quoteMark) {
    changeSet.remove(quoteMark.from, quoteMark.to)
  } else {
    changeSet.add(lineStart, lineStart, '> ')
  }
}

function findQuoteMark(node: SyntaxNode): { from: number; to: number } | null {
  const cursor = node.cursor()
  do {
    if (cursor.type.name === 'EnsoBlockquote') {
      const quoteMark = cursor.node.getChild('QuoteMark')
      if (quoteMark == null) return null
      return { from: quoteMark.from, to: quoteMark.to }
    }
  } while (cursor.parent())
  return null
}
