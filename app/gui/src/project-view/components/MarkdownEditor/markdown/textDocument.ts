import {
  type DelimitersTrimmedRange,
  type TrimmedRange,
} from '@/components/MarkdownEditor/markdown/types'
import type { Text } from '@codemirror/state'
import { Range } from 'ydoc-shared/util/data/range'

/** Provides text-level operations for a Markdown document. */
export class TextDocument {
  /** Constructor. */
  constructor(readonly doc: Text) {}

  /** Trim leading/trailing spaces from the input. */
  trimRangeSpaces(range: Readonly<DelimitersTrimmedRange>): TrimmedRange {
    return (range.tryContract(
      spacesAfter(this.lineContentAfter(range.from)),
      spacesBefore(this.lineContentBefore(range.to)),
    ) ?? Range.emptyAt(range.from)) as TrimmedRange
  }

  /** Add leading/trailing spaces to the input. */
  expandRangeSpaces(range: Readonly<Range>): Range {
    return range.expand(
      spacesBefore(this.lineContentBefore(range.from)),
      spacesAfter(this.lineContentAfter(range.to)),
    )
  }

  /** Return the largest possible range of non-whitespace characters containing the point. */
  wordAt(pos: number): TrimmedRange {
    const [textBefore, textAfter] = this.splitLine(pos)
    return Range.emptyAt(pos).expand(
      nonSpacesBefore(textBefore),
      nonSpacesAfter(textAfter),
    ) as TrimmedRange
  }

  /**
   * @returns The text of the line containing the specified position, split into before and after the position.
   *
   * This is a combination of {@link lineContentBefore} and {@link lineContentBefore}, but more efficient than getting
   * them separately.
   */
  splitLine(pos: number): [string, string] {
    const line = this.doc.lineAt(pos)
    const lineOffset = pos - line.from
    const textBefore = line.text.slice(0, lineOffset)
    const textAfter = line.text.slice(lineOffset)
    return [textBefore, textAfter]
  }

  /**
   * @returns The part of the text of the line containing the specified position that is before the position.
   *
   * See also {@link splitLine}.
   */
  lineContentBefore(pos: number): string {
    const line = this.doc.lineAt(pos)
    return line.text.slice(0, pos - line.from)
  }

  /**
   * @returns The part of the text of the line containing the specified position that is after the position.
   *
   * See also {@link splitLine}.
   */
  lineContentAfter(pos: number): string {
    const line = this.doc.lineAt(pos)
    return line.text.slice(pos - line.from)
  }

  /** @returns The given range expanded to fully-include any lines it partially-includes. */
  expandRangeToFullLines(range: Range): Range {
    return Range.unsafeFromBounds(this.doc.lineAt(range.from).from, this.doc.lineAt(range.to).to)
  }
}

function spacesBefore(text: string): number {
  return text.match(/\s*$/)![0].length
}

function spacesAfter(text: string): number {
  return text.match(/^\s*/)![0].length
}

function nonSpacesBefore(text: string): number {
  return text.match(/\S*$/)![0].length
}

function nonSpacesAfter(text: string): number {
  return text.match(/^\S*/)![0].length
}
