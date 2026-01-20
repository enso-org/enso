import { MarkdownDocument } from '@/components/MarkdownEditor/markdown/markdownDocument'
import type { ChangeSpec, Text } from '@codemirror/state'
import type { Tree } from '@lezer/common'
import { assert } from 'ydoc-shared/util/assert'
import { Range } from 'ydoc-shared/util/data/range'

/** Supports building a transaction editing a particular Markdown document state. */
export class MarkdownEdit extends MarkdownDocument {
  readonly changes: ChangeSpec[] = []
  adjustedSelection: Range = Range.empty

  /** Constructor. */
  constructor(
    text: Text,
    tree: Tree,
    public selection: Range = Range.empty,
  ) {
    super(text, tree)
    this.adjustedSelection = selection
  }

  /**
   * Insert the given strings around the specified range, without expanding the selection if the
   * insertion points coincide with its boundaries.
   */
  insertAroundRangeOutsideSelection(before: string, after: string, range: Range) {
    this.insert(before, range.from, 'outside-before')
    this.insert(after, range.to, 'outside-after')
  }

  /**
   * @param insert Text to insert.
   * @param from Position for inserted text to start, relative to document before any uncommitted
   * changes.
   * @param positionRelativeToSelection Determines the result when the insertion position is at the
   * boundary of the selection.
   * - 'inside': The selection will be expanded to include the inserted text.
   * - 'outside-before': The selection will not be expanded to include the inserted text. If the
   *   selection is 0-length, the insertion will be before it.
   * - 'outside-after': The selection will not be expanded to include the inserted text. If the
   *   selection is 0-length, the insertion will be after it.
   */
  insert(
    insert: string,
    from: number,
    positionRelativeToSelection: 'outside-before' | 'outside-after' | 'inside' = 'inside',
  ) {
    if (!insert) return
    this.changes.push({ from, to: from, insert })
    const atFrom = from === this.selection.from
    const atTo = from === this.selection.to
    const shiftFrom =
      from < this.selection.from ||
      (atFrom && !(positionRelativeToSelection !== 'outside-before' || !atTo))
    const shiftTo =
      from < this.selection.to ||
      (atTo && (positionRelativeToSelection !== 'outside-after' || !atFrom))
    assert(shiftTo || !shiftFrom)
    this.adjustedSelection = Range.unsafeFromBounds(
      this.adjustedSelection.from + (shiftFrom ? insert.length : 0),
      this.adjustedSelection.to + (shiftTo ? insert.length : 0),
    )
  }

  /** Delete the given range, specified as positions before any edits are committed. */
  remove(range: Range) {
    if (range.empty) return
    this.changes.push(range)
    this.adjustedSelection = Range.unsafeFromBounds(
      this.adjustedSelection.from - (range.from < this.selection.from ? range.length : 0),
      this.adjustedSelection.to - (range.to < this.selection.to ? range.length : 0),
    )
  }

  /**
   * Delete the given range, specified as positions before any edits are committed, and insert in
   * its place the given text.
   */
  replace(range: Range, text: string) {
    this.changes.push({ from: range.from, to: range.to, insert: text })
    // TODO: Adjust selection
  }

  /**
   * Set the selection to the given range, to be adjusted by further edits. Note selection
   * adjustment is not yet implemented for all operations on this type.
   */
  select(range: Range) {
    this.selection = range
    this.adjustedSelection = range
  }
}
