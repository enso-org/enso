/** @file Inline formatting types and their basic operations. */
import * as objects from 'enso-common/src/utilities/data/object'
import { Range } from 'ydoc-shared/util/data/range'

declare const brandDelimitersTrimmed: unique symbol
/** A {@link Range} that doesn't start with an end delimiter or end with a start delimiter. */
export type DelimitersTrimmedRange = Range & { [brandDelimitersTrimmed]: true }

declare const brandTrimmed: unique symbol
/** A {@link DelimitersTrimmedRange} that doesn't start or end with a Unicode whitespace character. */
export type TrimmedRange = DelimitersTrimmedRange & { [brandTrimmed]: true }

declare const brandSeminormalized: unique symbol
/**
 * A {@link TrimmedRange} that:
 * - Is fully-contained within a single block, that is not of an unformattable type.
 * - Doesn't include any delimiter defining the block's type.
 * - Doesn't partially-contain any unformattable-inline node.
 */
export type SeminormalizedRange = TrimmedRange & { [brandSeminormalized]: true }

declare const brandNormalized: unique symbol
/**
 * A {@link SeminormalizedRange} that:
 * - For any format node that the range includes all the non mark-node content of, includes the whole node.
 * - Doesn't start or end with a mark node unless the range includes the whole format node.
 * - Is not strictly contained by an inline unformattable node.
 * - Is not empty.
 */
export type NormalizedRange = SeminormalizedRange & { [brandNormalized]: true }

export type FormatNode = 'Emphasis' | 'StrongEmphasis' | 'Strikethrough'

const MARK_TOKEN: Readonly<Record<FormatNode, string>> = {
  Emphasis: '*',
  StrongEmphasis: '**',
  Strikethrough: '~~',
}
/** @returns The preferred delimiter for the specified node type. */
export function nodeMarkToken(nodeType: FormatNode): string {
  const token = MARK_TOKEN[nodeType]
  if (token === undefined) {
    // This is handled explicitly so that if we expect something to be a format node, but it isn't, we fail more
    // gracefully than inserting `undefined`s into the document.
    console.error(`Invalid format node type: ${nodeType}`)
    return ''
  }
  return token
}

export type FormatStates = Record<FormatNode, boolean>
export type FormatDepths = Record<FormatNode, number>

function combineFormatting(
  a: Readonly<FormatStates>,
  b: Readonly<FormatStates>,
  f: (a: boolean, b: boolean) => boolean,
): FormatStates {
  return {
    Emphasis: f(a.Emphasis, b.Emphasis),
    StrongEmphasis: f(a.StrongEmphasis, b.StrongEmphasis),
    Strikethrough: f(a.Strikethrough, b.Strikethrough),
  }
}

/** @returns The formats that are set in both inputs. */
export function andFormatting(a: Readonly<FormatStates>, b: Readonly<FormatStates>): FormatStates {
  return combineFormatting(a, b, (a, b) => a && b)
}

/** @returns The formats that have non-zero depth in the input. */
export function depthsToStates(depths: Readonly<FormatDepths>): FormatStates {
  return objects.mapEntries(depths, (_k, v) => v > 0)
}

/** @returns A base case for {@FormatDepths}. */
export function zeroFormatDepths(): FormatDepths {
  return {
    Emphasis: 0,
    StrongEmphasis: 0,
    Strikethrough: 0,
  }
}
