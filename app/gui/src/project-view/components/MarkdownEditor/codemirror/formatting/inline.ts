/** @file CodeMirror state operations for getting and setting inline formatting status. */
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
import {
  type FormatNode,
  type FormatStates,
  type NormalizedRange,
} from '@/components/MarkdownEditor/markdown/types'
import { syntaxTree } from '@codemirror/language'
import {
  type ChangeSpec,
  type EditorState,
  type SelectionRange,
  type TransactionSpec,
} from '@codemirror/state'
import { Range } from 'ydoc-shared/util/data/range'
export { type FormatNode as InlineFormattingNode } from '@/components/MarkdownEditor/markdown/types'

/**
 * @returns `undefined` if it is not possible to apply formatting to the given range. Otherwise, for each inline
 * formatting type, a boolean suitable for a button state. The boolean will be `false` if the format type could be
 * applied to more of the content in the range, or `true` if the format can be removed from the given range.
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
  const changeBuilder = new MDChangeBuilder(state.doc, syntaxTree(state))
  changeBuilder.visitFormattableRanges(selectionRange(state.selection.main), (range) =>
    setRangeFormatting(changeBuilder, range, nodeType, value),
  )
  const changes = state.changes(changeBuilder.changes)
  return {
    changes,
    // TODO SelectionMapping
    selection: state.selection.main.map(changes),
  }
}

function selectionRange(selection: SelectionRange): Range {
  return Range.tryFromBounds(selection.from, selection.to)!
}

class MDChangeBuilder extends MarkdownDocument {
  readonly changes: ChangeSpec[] = []

  insert(insert: string, from: number) {
    if (insert) this.changes.push({ from, to: from, insert })
  }

  remove(range: Range | Range[]) {
    this.changes.push(range)
  }
}

function setRangeFormatting(
  md: MDChangeBuilder,
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
  md: MDChangeBuilder,
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
  md.remove(
    [...partlyOutside.from, ...partlyOutside.to]
      .filter(({ name }) => name === nodeType)
      .map(({ delimiter }) => delimiter),
  )
}

function removeFormat(
  md: MDChangeBuilder,
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
  md.remove(
    [...fromOutside, ...toOutside]
      .filter(({ name }) => name === nodeType)
      .map(({ delimiter }) => delimiter),
  )
  md.remove(remove)
}
