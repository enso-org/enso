import * as iter from 'enso-common/src/utilities/data/iter'
import diff from 'fast-diff'
import { type DeepReadonly } from '../../ast'

export interface SourceRange {
  readonly from: number
  readonly to: number
}
declare const brandSourceRangeKey: unique symbol
export type SourceRangeKey = string & { [brandSourceRangeKey]: never }

/** Serializes a {@link SourceRange}, making it suitable for use as a key in maps or sets. */
export function sourceRangeKey({ from, to }: SourceRange): SourceRangeKey {
  return `${from.toString(16)}:${to.toString(16)}` as SourceRangeKey
}
/** Deserializes a {@link SourceRange} that was serialized by {@link sourceRangeKey} */
export function sourceRangeFromKey(key: SourceRangeKey): SourceRange {
  const [from, to] = key.split(':').map((x) => parseInt(x, 16)) as [number, number]
  return { from, to }
}

/** @returns Whether the two ranges have the same start and end. */
export function rangeEquals(a: SourceRange, b: SourceRange): boolean {
  return a.from == b.from && a.to == b.to
}

/** @returns Whether the point `b` is within the range `a`. */
export function rangeIncludes(a: SourceRange, b: number): boolean {
  return a.from <= b && a.to >= b
}

/** @returns whether the point `b` is within the range `a`. */
export function rangeLength(a: SourceRange): number {
  return a.to - a.from
}

/** @returns whether range `a` fully contains range `b`. */
export function rangeEncloses(a: SourceRange, b: SourceRange): boolean {
  return a.from <= b.from && a.to >= b.to
}

/** @returns Whether the ranges meet. */
export function rangeIntersects(a: SourceRange, b: SourceRange): boolean {
  return a.from <= b.to && a.to >= b.from
}

/** Whether the given range is before the other range. */
export function rangeIsBefore(a: SourceRange, b: SourceRange): boolean {
  return a.to <= b.from
}

/** Describes how a change to text will affect document locations. */
export interface SourceRangeEditDesc extends SourceRange {
  insert: { length: number }
}

/** A change that can be applied to text. */
export interface SourceRangeEdit extends SourceRangeEditDesc {
  insert: string
}

/** Given text and a set of `TextEdit`s, return the result of applying the edits to the text. */
export function applyTextEdits(
  oldText: string,
  textEdits: ReadonlyArray<Readonly<SourceRangeEdit>>,
) {
  const editsOrdered = [...textEdits]
  editsOrdered.sort((a, b) => a.from - b.from)
  let start = 0
  let newText = ''
  for (const textEdit of editsOrdered) {
    newText += oldText.slice(start, textEdit.from)
    newText += textEdit.insert
    start = textEdit.to
  }
  newText += oldText.slice(start)
  return newText
}

/** Given text before and after a change, return one possible set of {@link SourceRangeEdit}s describing the change. */
export function textChangeToEdits(before: string, after: string): SourceRangeEdit[] {
  const textEdits: SourceRangeEdit[] = []
  let nextEdit: undefined | SourceRangeEdit
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
        if (!nextEdit) nextEdit = { from: pos, to: pos, insert: '' }
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
        nextEdit = { from: pos, to: endPos, insert: '' }
        pos = endPos
        break
      }
    }
  }
  if (nextEdit) textEdits.push(nextEdit)
  return textEdits
}

/** Translate a source range by the specified offset. */
export function translateRange<T extends SourceRange>(textEdit: T, offset: number): T {
  return { ...textEdit, from: textEdit.from + offset, to: textEdit.to + offset }
}

/**
 * Given:
 *  @param textEdits - A change described by a set of text edits.
 *  @param spansBefore - A collection of spans in the text before the edit.
 *  @returns - A sequence of: Each span from `spansBefore` paired with the smallest span of the text after the edit that
 *  contains all text that was in the original span and has not been deleted.
 */
export function applyTextEditsToSpans(
  textEdits: DeepReadonly<SourceRangeEditDesc[]>,
  spansBefore: DeepReadonly<SourceRange[]>,
) {
  // Gather start and end points.
  const numerically = (a: number, b: number) => a - b
  const starts = new iter.Resumable(spansBefore.map(({ from }) => from).sort(numerically))
  const ends = new iter.Resumable(spansBefore.map(({ to }) => to).sort(numerically))

  // Construct translations from old locations to new locations for all start and end points.
  const startMap = new Map<number, number>()
  const endMap = new Map<number, number>()
  let offset = 0
  for (const textEdit of textEdits) {
    const { from, to, insert } = textEdit
    starts.advanceWhile((start) => {
      if (start < from) {
        startMap.set(start, start + offset)
        return true
      } else if (start <= to) {
        startMap.set(start, from + offset + insert.length)
        return true
      }
      return false
    })
    ends.advanceWhile((end) => {
      if (end <= from) {
        endMap.set(end, end + offset)
        return true
      } else if (end <= to) {
        endMap.set(end, from + offset)
        return true
      }
      return false
    })
    offset += insert.length - rangeLength(textEdit)
  }
  starts.forEach((start) => startMap.set(start, start + offset))
  ends.forEach((end) => endMap.set(end, end + offset))

  // Apply the translations to the map.
  const spansBeforeAndAfter = new Array<readonly [SourceRange, SourceRange]>()
  for (const spanBefore of spansBefore) {
    const startAfter = startMap.get(spanBefore.from)!
    const endAfter = endMap.get(spanBefore.to)!
    if (endAfter > startAfter)
      spansBeforeAndAfter.push([spanBefore, { from: startAfter, to: endAfter }])
  }
  return spansBeforeAndAfter
}

export interface SpanTree<NodeId> {
  id(): NodeId
  span(): SourceRange
  children(): IterableIterator<SpanTree<NodeId>>
}

/**
 * Given a span tree and some ranges, for each range find the smallest node that fully encloses it.
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
export function trimEnd<T extends SourceRange>(range: T, text: string): T {
  const trimmedLength = text.slice(range.from, range.to).search(/ +$/)
  return trimmedLength === -1 ? range : (
      { ...range, from: range.from, to: range.from + trimmedLength }
    )
}
