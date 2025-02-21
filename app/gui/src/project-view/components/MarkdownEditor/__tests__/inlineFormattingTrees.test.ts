import { parseTestInput, printTestInput } from '@/components/MarkdownEditor/__tests__/testInput'
import {
  denormalizeRange,
  normalizeRange,
} from '@/components/MarkdownEditor/markdown/inlineFormatting/trees'
import {
  type NormalizedRange,
  type SeminormalizedRange,
} from '@/components/MarkdownEditor/markdown/inlineFormatting/types'
import { expect, test } from 'vitest'
import { ensoMarkdownParser } from 'ydoc-shared/ast/ensoMarkdown'

interface RangeNormalizationCase {
  unnormalized?: string
  normalized: string
  denormalized?: string
}

const rangeNormalizationCases: RangeNormalizationCase[] = [
  { unnormalized: '*|Some text|*', normalized: '|*Some text*|' },
  { unnormalized: '***|Very emphasized|***', normalized: '|***Very emphasized***|' },
  { unnormalized: 'Very ***|emphasized|***', normalized: 'Very |***emphasized***|' },
  // FIXME: Normalization bug.
  // { unnormalized: '***~~|Very formatted|~~***', normalized: '|***~~Very formatted~~***|' },
  { unnormalized: '*Some |text*|', normalized: '*Some |text|*', denormalized: '*Some |text*|' },
  { unnormalized: '|*Some| text*', normalized: '*|Some| text*', denormalized: '|*Some| text*' },
  { unnormalized: '|Some *text|*', normalized: '|Some *text*|' },
  {
    unnormalized: '~~*Some |text*|~~',
    normalized: '~~*Some |text|*~~',
    denormalized: '~~*Some |text*~~|',
  },
  { unnormalized: '*|Some* text|', normalized: '|*Some* text|' },
  { normalized: '*Some |formatted* text|' },
  { normalized: '|Some *formatted| text*' },
]

test.each(rangeNormalizationCases.filter((input) => 'unnormalized' in input))(
  'Range normalization: $unnormalized',
  ({ unnormalized, normalized }) => {
    const input = parseTestInput(unnormalized!)
    const result = normalizeRange(
      { from: input.selection.anchor, to: input.selection.head } as SeminormalizedRange,
      ensoMarkdownParser.parse(input.doc),
    )!
    expect(printTestInput(input.doc, { anchor: result.from, head: result.to })).toBe(normalized)
  },
)

test.each(rangeNormalizationCases)(
  'Range denormalization: $normalized',
  ({ normalized, denormalized }) => {
    const input = parseTestInput(normalized)
    const result = denormalizeRange(
      { from: input.selection.anchor, to: input.selection.head } as NormalizedRange,
      ensoMarkdownParser.parse(input.doc),
    )
    expect(printTestInput(input.doc, { anchor: result.from, head: result.to })).toBe(
      denormalized ?? normalized,
    )
  },
)

test.each(rangeNormalizationCases)(
  'Range normalization idempotent: $normalized',
  ({ normalized }) => {
    const input = parseTestInput(normalized)
    const result = normalizeRange(
      { from: input.selection.anchor, to: input.selection.head } as SeminormalizedRange,
      ensoMarkdownParser.parse(input.doc),
    )!
    expect(printTestInput(input.doc, { anchor: result.from, head: result.to })).toBe(normalized)
  },
)
