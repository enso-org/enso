/** @file CodeMirror state operations for getting and setting inline formatting status. */
import { MarkdownEdit } from '@/components/MarkdownEditor/codemirror/formatting/markdownEdit'
import {
  MarkdownDocument,
  nodeExtensionOrExpansions,
  nodeSplitDelimiters,
} from '@/components/MarkdownEditor/markdown/markdownDocument'
import {
  analyzeMerges,
  analyzeRangeFormattingRemoval,
  analyzeSplits,
  visitContainedDelimiters,
} from '@/components/MarkdownEditor/markdown/trees'
import type {
  FormatNode,
  FormatStates,
  NormalizedRange,
} from '@/components/MarkdownEditor/markdown/types'
import { syntaxTree } from '@codemirror/language'
import type { EditorState, SelectionRange, TransactionSpec } from '@codemirror/state'
import * as iter from 'enso-common/src/utilities/data/iter'
import { Range } from 'ydoc-shared/util/data/range'
export type { FormatNode as InlineFormattingNode } from '@/components/MarkdownEditor/markdown/types'

/**
 * @returns `undefined` if it is not possible to apply formatting to the given range. Otherwise, for
 * each inline formatting type, a boolean suitable for a button state. The boolean will be `false`
 * if the format type could be applied to more of the content in the range, or `true` if the format
 * can be removed from the given range.
 */
export function getInlineFormatting(state: EditorState): FormatStates | undefined {
  const range = state.selection.main
  const md = new MarkdownDocument(state.doc, syntaxTree(state))
  return range.to === range.from ?
      md.pointFormatInfo(range.from)
    : md.rangeFormatInfo(selectionRange(range))
}

/** Add or remove a format type to the current selection. */
export function setInlineFormatting(
  state: EditorState,
  nodeType: FormatNode,
  value: boolean,
): TransactionSpec {
  const md = new MarkdownEdit(state.doc, syntaxTree(state))
  md.select(selectionRange(state.selection.main))
  md.visitFormattableRanges(selectionRange(state.selection.main), (range) =>
    setRangeFormatting(md, range, nodeType, value),
  )
  const changes = state.changes(md.changes)
  return {
    changes,
    // TODO SelectionMapping
    //  `SelectionRange.map` produces a "valid" new selection based on the old selection and the
    //  changes, but it isn't perfect. Once MarkdownEdit's selection-adjusting logic is consistently
    //  better than that sane default, we should switch to it and enable the checks of after-edit
    //  selection boundaries `inlineFormatting.test.ts`.
    // selection: rangeToSelection(md.adjustedSelection),
    selection: state.selection.main.map(changes),
  }
}

/** @returns Whether a link can be inserted. */
export function canInsertLink(state: EditorState): boolean {
  const md = new MarkdownEdit(state.doc, syntaxTree(state))
  const range = lastFormattableRange(md, selectionRange(state.selection.main))
  // Note: Once formatting link text is allowed, we will have to check that we aren't already inside a link here.
  return range !== undefined
}

/** Insert a link at the selection. */
export function insertLink(state: EditorState): TransactionSpec {
  const md = new MarkdownEdit(state.doc, syntaxTree(state))
  const range = lastFormattableRange(md, selectionRange(state.selection.main))
  if (range === undefined) {
    console.error('Cannot insert link: No formattable range')
    return {}
  }
  const beforeText = '['
  const afterText = '](https://)'
  const afterTextSelection = Range.unsafeFromBounds(
    afterText.indexOf('(') + 1,
    afterText.indexOf(')'),
  )
  let afterTextPos: number
  if (range.empty) {
    const defaultText = 'Link'
    md.insert(`${beforeText}${defaultText}${afterText}`, range.to)
    afterTextPos = range.to + beforeText.length + defaultText.length
  } else {
    md.select(Range.emptyAt(range.to))
    insertAround(md, range, beforeText, afterText)
    afterTextPos = md.adjustedSelection.to
  }
  return {
    changes: md.changes,
    selection: rangeToSelection(afterTextSelection.shift(afterTextPos)),
  }
}

function lastFormattableRange(md: MarkdownDocument, selection: Range): NormalizedRange | undefined {
  let range: NormalizedRange | undefined
  md.visitFormattableRanges(
    selection,
    (r) => (range = r),
    (pos) => (range = Range.emptyAt(pos) as NormalizedRange),
  )
  return range
}

/**
 * Insert the given strings around the specified text, splitting any inlining formatting nodes as
 * needed. If the range touches the boundary of the selection, the given strings will be inserted
 * outside it.
 */
function insertAround(md: MarkdownEdit, range: NormalizedRange, before: string, after: string) {
  const partlyOutside = analyzeSplits(md.tree, range)
  const { outside: closeBefore, inside: reopenInside } = nodeSplitDelimiters.from(
    md,
    partlyOutside.from,
  )
  const { outside: reopenAfter, inside: closeInside } = nodeSplitDelimiters.to(md, partlyOutside.to)
  const outsideRange = md.expandRangeSpaces(range)
  md.insert(closeBefore, outsideRange.from)
  md.insert(closeInside, range.to)
  md.insertAroundRangeOutsideSelection(before, after, range)
  md.insert(reopenInside, range.from)
  md.insert(reopenAfter, outsideRange.to)
}

function rangeToSelection(range: Range): { anchor: number; head: number } {
  return {
    anchor: range.from,
    head: range.to,
  }
}

function selectionRange(selection: SelectionRange): Range {
  return Range.unsafeFromBounds(selection.from, selection.to)
}

function setRangeFormatting(
  md: MarkdownEdit,
  range: NormalizedRange,
  nodeType: FormatNode,
  value: boolean,
) {
  const outsideRange = md.expandRangeSpaces(range)
  visitContainedDelimiters(range, md.tree, nodeType, md.remove.bind(md))
  if (value) addFormat(md, range, outsideRange, nodeType)
  else removeFormat(md, range, outsideRange, nodeType)
}

function addFormat(
  md: MarkdownEdit,
  range: NormalizedRange,
  outsideRange: Range,
  nodeType: FormatNode,
): void {
  const partlyOutside = analyzeSplits(md.tree, range)
  const adjacentlyOutside = analyzeMerges(md.tree, outsideRange, nodeType)
  const { mark, extendOrExpand } = nodeExtensionOrExpansions(
    md,
    partlyOutside,
    adjacentlyOutside,
    nodeType,
  )
  if (extendOrExpand.from) {
    md.remove(extendOrExpand.from)
  } else {
    const { outside: closeOutside, inside: reopenInside } = nodeSplitDelimiters.from(
      md,
      partlyOutside.from,
      nodeType,
    )
    md.insert(closeOutside, outsideRange.from)
    md.insert(mark + reopenInside, range.from)
  }
  if (extendOrExpand.to) {
    md.remove(extendOrExpand.to)
  } else {
    const { inside: closeInside, outside: reopenOutside } = nodeSplitDelimiters.to(
      md,
      partlyOutside.to,
      nodeType,
    )
    md.insert(reopenOutside, outsideRange.to)
    md.insert(closeInside + mark, range.to)
  }
  for (const { name, delimiter } of iter.chain(partlyOutside.from, partlyOutside.to))
    if (name === nodeType) md.remove(delimiter)
}

function removeFormat(
  md: MarkdownEdit,
  range: NormalizedRange,
  outsideRange: Range,
  nodeType: FormatNode,
): void {
  const { fromOutside, toOutside, outerNodesToClose, remove } = analyzeRangeFormattingRemoval(
    md.tree,
    range,
    nodeType,
  )
  const { outside: closeOutside, inside: reopenInside } = nodeSplitDelimiters.from(
    md,
    [...outerNodesToClose, ...fromOutside],
    nodeType,
  )
  const { inside: closeInside, outside: reopenOutside } = nodeSplitDelimiters.to(
    md,
    [...outerNodesToClose, ...toOutside],
    nodeType,
  )
  md.insert(closeOutside, outsideRange.from)
  md.insert(reopenInside, range.from)
  md.insert(closeInside, range.to)
  md.insert(reopenOutside, outsideRange.to)
  for (const { name, delimiter } of iter.chain(fromOutside, toOutside))
    if (name === nodeType) md.remove(delimiter)
  for (const r of remove) md.remove(r)
}
