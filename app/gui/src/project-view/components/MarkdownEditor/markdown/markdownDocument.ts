import { TextDocument } from '@/components/MarkdownEditor/markdown/textDocument'
import {
  getUnformattableAncestor,
  isDelimiter,
  pointFormatAncestorInfo,
  splitNodesAt,
  splitRange,
  trimRangeDelimiters,
} from '@/components/MarkdownEditor/markdown/trees'
import {
  andFormatting,
  depthsToStates,
  type FormatDepths,
  type FormatNode,
  type FormatStates,
  nodeMarkToken,
  type NormalizedRange,
} from '@/components/MarkdownEditor/markdown/types'
import type { Text } from '@codemirror/state'
import type { Tree } from '@lezer/common'
import type { DeepReadonly } from 'vue'
import { Range } from 'ydoc-shared/util/data/range'

function sides<T>(f: (fromOrTo: 'from' | 'to') => T): { from: T; to: T } {
  return {
    from: f('from'),
    to: f('to'),
  }
}

/** A document with {@link Text} and an associated Markdown syntax {@link Tree}. */
export class MarkdownDocument extends TextDocument {
  /** Constructor. */
  constructor(
    doc: Text,
    readonly tree: Tree,
  ) {
    super(doc)
  }

  /**
   * @returns The formatting state for a cursor position, unless the syntax tree containing the position is not
   * formattable at any level--i.e., unless the cursor is inside a block type in which formatting markup is not
   * recognized.
   */
  pointFormatInfo(pos: number): FormatStates | undefined {
    const treeFormatInfo = pointFormatAncestorInfo(pos, this.tree)
    if (!treeFormatInfo) return
    const { formatDepth } = treeFormatInfo
    return depthsToStates(formatDepth)
  }

  /** @returns The current format states for the formattable parts of a given range of text. */
  rangeFormatInfo(range: Range): FormatStates | undefined {
    let formatting: FormatStates | undefined = undefined
    this.visitSubRanges(range, (range) => {
      const rangeFormatting = {
        Emphasis: this.checkRangeFormat(range, 'Emphasis'),
        StrongEmphasis: this.checkRangeFormat(range, 'StrongEmphasis'),
        Strikethrough: this.checkRangeFormat(range, 'Strikethrough'),
      }
      if (
        rangeFormatting.Emphasis != null &&
        rangeFormatting.StrongEmphasis != null &&
        rangeFormatting.Strikethrough != null
      )
        formatting =
          formatting ?
            andFormatting(formatting, rangeFormatting as FormatStates)
          : (rangeFormatting as FormatStates)
    })
    return formatting
  }

  /** Applies the given visitor to the formattable parts of the given range. */
  visitFormattableRanges(
    range: Range,
    /** Callback that will be applied to each inline-formattable subrange of the input. */
    visitRange: (range: NormalizedRange) => void,
    visitPos?: (pos: number) => void,
  ) {
    if (range.to === range.from) {
      const formatInfo = pointFormatAncestorInfo(range.from, this.tree)
      if (!formatInfo) return
      const { unformattable } = formatInfo
      const interpreted = this.interpretCursor(range.from, unformattable)
      if (typeof interpreted === 'number') {
        visitPos?.(interpreted)
        return
      }
      range = interpreted
    }
    this.visitSubRanges(range, visitRange)
  }

  /** @returns The document text in the range. */
  token(range: Range): string {
    return this.doc.sliceString(range.from, range.to)
  }

  private trimRange(range: Range) {
    return this.trimRangeSpaces(trimRangeDelimiters(range, this.tree))
  }

  private visitSubRanges(range: Range, visitRange: (range: NormalizedRange) => void) {
    splitRange(this.trimRange(range), this.tree, visitRange, this.trimRange.bind(this))
  }

  private interpretCursor(pos: number, inInlineUnformattableNode: boolean): number | Range {
    if (!inInlineUnformattableNode) {
      const cursorFormatting = this.cursorFormatInfo(pos)
      // If the cursor is free (except for cursor-formatting delimiters), it is suitable for point-formatting.
      if (cursorFormatting) return cursorFormatting.pos
    }
    return inInlineUnformattableNode ? getUnformattableAncestor(pos, this.tree) : this.wordAt(pos)
  }

  private checkRangeFormat(range: Range, nodeType: string): boolean | undefined {
    let foundDelimiters = false
    let foundContentOutsideDelimiters = false
    const visitor = new RangeGapVisitor(range.from, range.to, (range) => {
      if (this.token(range).match(/\S/)) foundContentOutsideDelimiters = true
    })
    this.tree.iterate({
      from: range.from,
      to: range.to,
      enter: (node) => {
        if (node.to === range.from || node.from === range.to) return false
        if (foundContentOutsideDelimiters) return false
        if (isDelimiter(node.name)) visitor.push(node.from, node.to)
        else if (node.name === nodeType) {
          foundDelimiters = true
          visitor.push(node.from, node.to)
          return false
        }
      },
    })
    if (!foundContentOutsideDelimiters) visitor.finish()
    const canAdd = foundContentOutsideDelimiters
    const canRemove = foundDelimiters
    if (!canAdd && !canRemove) return undefined
    return !canAdd
  }

  /**
   * Checks whether the cursor is in a word composed of characters that would all be parsed as delimiters if non-special
   * characters were in the center. Returns `undefined` if the cursor is flanked.
   *
   * This operation only examines text; the caller should first check that the cursor is not within an unformattable node.
   */
  private cursorFormatInfo(pos: number): { formatDepths: FormatDepths; pos: number } | undefined {
    // If the cursor is touching a group of cursor-formatting delimiters, find its center.
    const word = this.wordAt(pos)
    const formatPos = (word.from + word.to) >> 1

    const [textBefore, textAfter] = this.splitLine(formatPos)
    const formatDepths = parseCursorDelimiters(textBefore, textAfter)
    const someCursorFormatting =
      formatDepths && Object.values(formatDepths).some((depth) => depth > 0)
    return formatDepths ? { formatDepths, pos: someCursorFormatting ? formatPos : pos } : undefined
  }
}

/**
 * Determines whether to add formatting to a range by *removing* a delimiter. This is possible in two situations:
 * - A node of the desired format ends adjacent to the range. The node can be *extended* to include the range. The
 *   delimiter to remove is *outside* the (normalized) range.
 * - The range contains part of a node of the desired format. The node can be *expanded* to the whole range. The
 *   delimiter to remove is *inside* the (normalized) range.
 *
 * This function also decides the mark to use to open/close the format: If a node is being extended or expanded, the
 * same mark will be used; otherwise, the default for the format type.
 */
export function nodeExtensionOrExpansions(
  md: MarkdownDocument,
  partlyOutside: {
    from: { name: string; delimiter: Range }[]
    to: { name: string; delimiter: Range }[]
  },
  adjacentlyOutside: {
    from: Range | undefined
    to: Range | undefined
  },
  nodeType: FormatNode,
): {
  mark: string
  extendOrExpand: { from: Range | undefined; to: Range | undefined }
} {
  const defaultMark = nodeMarkToken(nodeType)
  const extendOrExpand = {
    from:
      extendDelimiter(md, adjacentlyOutside.from) ??
      expandDelimiter(md, partlyOutside.from, nodeType),
    to:
      extendDelimiter(md, adjacentlyOutside.to) ?? expandDelimiter(md, partlyOutside.to, nodeType),
  }
  // The `_` variant of `EmphasisMark` is not allowed within a word; to avoid creating that situation, we simply don't
  // extend/expand nodes using it. This is safe because node extension is only mandatory when the range boundary is
  // within a word, in which case it isn't possible to have an adjacent node with that mark type (node *expansion* is
  // never mandatory).
  const requiredMark = nodeType === 'Emphasis' ? defaultMark : undefined
  const mark = requiredMark ?? extendOrExpand.from?.token ?? extendOrExpand.to?.token ?? defaultMark
  if (extendOrExpand.from?.token !== mark) extendOrExpand.from = undefined
  if (extendOrExpand.to?.token !== mark) extendOrExpand.to = undefined
  return {
    mark,
    extendOrExpand: { from: extendOrExpand.from?.delimiter, to: extendOrExpand.to?.delimiter },
  }
}
function extendDelimiter(md: MarkdownDocument, delimiter: Range | undefined) {
  return (
    delimiter && {
      delimiter,
      token: md.token(delimiter),
    }
  )
}
function expandDelimiter(
  md: MarkdownDocument,
  nodes: { name: string; delimiter: Range }[],
  nodeType: FormatNode,
) {
  if (nodes[0]?.name !== nodeType) return
  const delimiter = nodes[0].delimiter
  return {
    delimiter,
    token: md.token(delimiter),
  }
}

export const nodeSplitDelimiters = sides(
  (fromOrTo) =>
    (
      md: MarkdownDocument,
      splittable: DeepReadonly<{ name: string; delimiter: Range }>[],
      nodeType?: string | undefined,
    ): { outside: string; inside: string } => {
      const { outside, inside } = splitNodesAt[fromOrTo](splittable, nodeType)
      const token = ({ delimiter }: { delimiter: Range }) => md.token(delimiter)
      return {
        outside: outside.map(token).join(''),
        inside: inside.map(token).join(''),
      }
    },
)

/** Recognize a balanced stack of delimiters around the cursor. */
function parseCursorDelimiters(
  textBeforeCursor: string,
  textAfterCursor: string,
): FormatDepths | undefined {
  // Note that we only support a particular, arbitrary nesting order: This is not a standard Markdown feature, and
  // users are not likely to create this cursor-flanking construct by typing delimiters; we only need to recognize the
  // order that the buttons can produce.
  const beforeCursor = textBeforeCursor.match(/(?:^|\s)(\**)((?:~~)*)$/)
  if (beforeCursor) {
    const stars = beforeCursor[1]!.length
    const squiggles = beforeCursor[2]!.length
    const afterCursor = textAfterCursor.match(/^((?:~~)*)(\**)(?:$|\s)/)
    if (afterCursor && squiggles === afterCursor[1]!.length && stars === afterCursor[2]!.length) {
      return {
        Emphasis: stars % 2,
        StrongEmphasis: stars >> 1,
        Strikethrough: squiggles >> 1,
      }
    }
  }
}

/** Accepts a sequence of ranges, and applies a visitor to the gaps between them. */
class RangeGapVisitor {
  private prevEnd: number
  constructor(
    from: number,
    private readonly to: number,
    private readonly emit: (range: Range) => void,
  ) {
    this.prevEnd = from
  }
  push(from: number, to: number) {
    this.flush(from)
    this.prevEnd = to
  }
  finish() {
    this.flush(this.to)
  }
  private flush(to: number) {
    if (this.prevEnd < to) this.emit(Range.unsafeFromBounds(this.prevEnd, to))
  }
}
