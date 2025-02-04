import { ChangeSpec } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { Tree } from '@lezer/common'
import { debugTree, markdownParser } from 'ydoc-shared/ast/ensoMarkdown'

/** Supported header levels. */
export type HeaderLevel = 1 | 2 | 3

export function toggleHeader(view: EditorView, level: HeaderLevel) {
  const startLine = view.state.doc.lineAt(view.state.selection.main.from)
  const endLine = view.state.doc.lineAt(view.state.selection.main.to)
  const tree = markdownParser.parse(view.state.doc.toString())
  const add = []
  const replace = []
  let remove = []
  for (let lineIndex = startLine.number; lineIndex <= endLine.number; lineIndex++) {
    const line = view.state.doc.line(lineIndex)
    const src = view.state.doc.toString()
    const result = toggleHeaderInner(tree, level, line.from, line.to, src, 0)
    add.push(...result.add)
    replace.push(...result.replace)
    remove.push(...result.remove)
  }
  if (add.length > 0) remove = []
  view.dispatch({ changes: [...add, ...remove, ...replace] })
}

interface ToggleChangeSet {
  add: ChangeSpec[]
  replace: ChangeSpec[]
  remove: ChangeSpec[]
}

function toggleHeaderInner(
  tree: Tree,
  level: number,
  lineStart: number,
  lineEnd: number,
  src: string,
  offset: number,
): ToggleChangeSet {
  const add = []
  const replace = []
  const remove = []
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
        remove.push({ from: headerMark.from + offset, to: headerMark.to + offset, insert: '' })
      } else {
        replace.push({
          from: headerMark.from + offset,
          to: headerMark.to + offset,
          insert: prefix + ' ',
        })
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
    add.push(...result.add)
    replace.push(...result.replace)
    remove.push(...result.remove)
  } else {
    add.push({ from: lineStart + offset, to: lineStart + offset, insert: prefix + ' ' })
  }
  return { add, replace, remove }
}

export function toggleQuote(view: EditorView) {
  const tree = markdownParser.parse(view.state.doc.toString())
  const selectionPos = view.state.selection.main.from
  let node = tree.resolve(selectionPos, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  const cursor = node.cursor()
  do {
    if (cursor.type.name === 'EnsoBlockquote') {
      const quoteMark = cursor.node.getChild('QuoteMark')
      if (quoteMark != null) {
        view.dispatch({ changes: [{ from: quoteMark.from, to: quoteMark.to, insert: '' }] })
        return
      }
    }
  } while (cursor.parent())
  const line = view.state.doc.lineAt(selectionPos)
  view.dispatch({ changes: [{ from: line.from, to: line.from, insert: '> ' }] })
}

export type ListType = 'unordered' | 'ordered'

export function toggleList(view: EditorView, type: ListType) {
  const tree = markdownParser.parse(view.state.doc.toString())
  const startLine = view.state.doc.lineAt(view.state.selection.main.from)
  const endLine = view.state.doc.lineAt(view.state.selection.main.to)
  const changes = []
  let listIndex = 0
  for (let i = startLine.number; i <= endLine.number; i++) {
    const line = view.state.doc.line(i)
    const src = view.state.doc.toString()
    const result = toggleListInner(tree, listIndex, line.from, line.to, src, type)
    changes.push(...result.add)
    changes.push(...result.replace)
    changes.push(...result.remove)
    listIndex++
  }
  view.dispatch({ changes })
}

function toggleListInner(
  tree: Tree,
  listIndex: number,
  lineStart: number,
  lineEnd: number,
  src: string,
  type: ListType,
): ToggleChangeSet {
  const add = []
  const replace = []
  const remove = []
  let node = tree.resolve(lineEnd, -1)
  if (node.type.name === 'Document' && node.firstChild != null) node = node.firstChild
  let listMark: false | { from: number; to: number } = false
  let listType: undefined | ListType = undefined
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
    remove.push({
      from: listMark.from,
      to: listType === 'ordered' ? listMark.to + 1 : listMark.to,
      insert: '',
    })
  } else if (listMark && listType !== type) {
    replace.push({
      from: listMark.from,
      to: listMark.to,
      insert: type === 'unordered' ? '-' : `${listIndex + 1}. `,
    })
  } else if (!listMark) {
    add.push({
      from: lineStart,
      to: lineStart,
      insert: type === 'unordered' ? '- ' : `${listIndex + 1}. `,
    })
  }
  return { add, replace, remove }
}
