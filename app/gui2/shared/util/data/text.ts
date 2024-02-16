import diff from 'fast-diff'
import { rangeEncloses, rangeLength, type SourceRange } from '../../yjsModel'
import { assertEqual } from '../assert'

export type TextEdit = { range: SourceRange; insert: string }

/** Given text and a set of `TextEdit`s, return the result of applying the edits to the text. */
export function applyTextEdits(oldText: string, textEdits: TextEdit[]) {
  textEdits.sort((a, b) => a.range[0] - b.range[0])
  let start = 0
  let newText = ''
  for (const textEdit of textEdits) {
    newText += oldText.slice(start, textEdit.range[0])
    newText += textEdit.insert
    start = textEdit.range[1]
  }
  newText += oldText.slice(start)
  return newText
}

/** Given text before and after a change, return one possible set of `TextEdit`s describing the change. */
export function textChangeToEdits(before: string, after: string): TextEdit[] {
  const textEdits: TextEdit[] = []
  let nextEdit: undefined | TextEdit
  let pos = 0
  // Sequences fast-diff emits:
  // EQUAL, INSERT
  // EQUAL, DELETE
  // DELETE, EQUAL
  // DELETE, INSERT
  // INSERT, EQUAL
  for (const [op, text] of diff(before, after)) {
    switch (op) {
      case diff.INSERT:
        if (!nextEdit) nextEdit = { range: [pos, pos], insert: '' }
        nextEdit.insert = text
        break
      case diff.EQUAL:
        if (nextEdit) {
          textEdits.push(nextEdit)
          nextEdit = undefined
        }
        pos += text.length
        break
      case diff.DELETE: {
        if (nextEdit) textEdits.push(nextEdit)
        const endPos = pos + text.length
        nextEdit = { range: [pos, endPos], insert: '' }
        pos = endPos
        break
      }
    }
  }
  if (nextEdit) textEdits.push(nextEdit)
  assertEqual(applyTextEdits(before, textEdits), after)
  return textEdits
}

/** Translate a `TextEdit` by the specified offset. */
export function offsetEdit(textEdit: TextEdit, offset: number): TextEdit {
  return { ...textEdit, range: [textEdit.range[0] + offset, textEdit.range[1] + offset] }
}

/** Given:
 *  @param textEdits - A change described by a set of text edits.
 *  @param spansBefore - A collection of spans in the text before the edit.
 *  @returns - A sequence of: Each span from `spansBefore` paired with the smallest span of the text after the edit that
 *  contains all text that was in the original span and has not been deleted. */
export function applyTextEditsToSpans(textEdits: TextEdit[], spansBefore: SourceRange[]) {
  // Gather the start and end locations of all spans.
  const starts = new Array<number>()
  const ends = new Array<number>()
  for (const [start, end] of spansBefore) {
    starts.push(start)
    ends.push(end)
  }
  const numerically = (a: number, b: number) => a - b
  starts.sort(numerically)
  ends.sort(numerically)

  // Construct translations from old locations to new locations for all start and end points.
  const startMap = new Map<number, number>()
  const endMap = new Map<number, number>()
  let offset = 0
  for (const { range, insert } of textEdits) {
    while (starts[0] !== undefined) {
      if (starts[0] < range[0]) {
        startMap.set(starts[0], starts[0] + offset)
      } else if (starts[0] <= range[1]) {
        startMap.set(starts[0], range[0] + offset + insert.length)
      } else {
        break
      }
      starts.shift()
    }
    while (ends[0] !== undefined) {
      if (ends[0] <= range[0]) {
        endMap.set(ends[0], ends[0] + offset)
      } else if (ends[0] <= range[1]) {
        endMap.set(ends[0], range[0] + offset)
      } else {
        break
      }
      ends.shift()
    }
    offset += insert.length - rangeLength(range)
  }
  for (const start of starts) startMap.set(start, start + offset)
  for (const end of ends) endMap.set(end, end + offset)

  // Apply the translations to the map.
  const spansBeforeAndAfter = new Array<readonly [SourceRange, SourceRange]>()
  for (const spanBefore of spansBefore) {
    const startAfter = startMap.get(spanBefore[0])!
    const endAfter = endMap.get(spanBefore[1])!
    if (endAfter > startAfter) spansBeforeAndAfter.push([spanBefore, [startAfter, endAfter]])
  }
  return spansBeforeAndAfter
}

export interface SpanTree<NodeId> {
  id(): NodeId
  span(): SourceRange
  children(): IterableIterator<SpanTree<NodeId>>
}

/** Given a span tree and some ranges, for each range find the smallest node that fully encloses it.
 *  Return nodes paired with the ranges that are most closely enclosed by them.
 */
export function enclosingSpans<NodeId>(
  tree: SpanTree<NodeId>,
  ranges: SourceRange[],
  resultsOut?: [NodeId, SourceRange[]][],
) {
  const results = resultsOut ?? []
  for (const child of tree.children()) {
    const childSpan = child.span()
    const childRanges: SourceRange[] = []
    ranges = ranges.filter((range) => {
      if (rangeEncloses(childSpan, range)) {
        childRanges.push(range)
        return false
      }
      return true
    })
    if (childRanges.length) enclosingSpans(child, childRanges, results)
  }
  if (ranges.length) results.push([tree.id(), ranges])
  return results
}

/** Return the given range with any trailing spaces stripped. */
export function trimEnd(range: SourceRange, text: string): SourceRange {
  const trimmedLength = text.slice(range[0], range[1]).search(/ +$/)
  return trimmedLength === -1 ? range : [range[0], range[0] + trimmedLength]
}
