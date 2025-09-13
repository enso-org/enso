import { parseTestInput, printTestInput } from '@/components/MarkdownEditor/__tests__/testInput'
import { MarkdownDocument } from '@/components/MarkdownEditor/markdown/markdownDocument'
import {
  denormalizeRange,
  normalizeRange,
  splitRange,
  trimRangeDelimiters,
} from '@/components/MarkdownEditor/markdown/trees'
import {
  type NormalizedRange,
  type SeminormalizedRange,
} from '@/components/MarkdownEditor/markdown/types'
import { Text } from '@codemirror/state'
import { expect, test } from 'vitest'
import { ensoMarkdownParser } from 'ydoc-shared/ast/ensoMarkdown'
import { Range } from 'ydoc-shared/util/data/range'

interface RangeNormalizationCase {
  unnormalized?: string
  normalized: string
  denormalized?: string
}

const rangeNormalizationCases: RangeNormalizationCase[] = [
  { unnormalized: '*|Some text|*', normalized: '|*Some text*|' },
  { unnormalized: '***|Very emphasized|***', normalized: '|***Very emphasized***|' },
  { unnormalized: 'Very ***|emphasized|***', normalized: 'Very |***emphasized***|' },
  { unnormalized: '***~~|Very formatted|~~***', normalized: '|***~~Very formatted~~***|' },
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

test.each([
  {
    source: 'Some text |with *normal*| formatting',
    ranges: ['Some text |with *normal*| formatting'],
  },
  {
    source: 'Some text |with `inline-unformattable`| formatting',
    ranges: ['Some text |with `inline-unformattable`| formatting'],
  },
  {
    source: 'Partially-|selected `inline| unformattable`',
    ranges: ['Partially-|selected| `inline unformattable`'],
  },
  {
    source: '`Inline |unformattable` partially|-selected',
    ranges: ['`Inline unformattable` |partially|-selected'],
  },
  {
    source: '`Inline unformattable parts |before` and `after| selection`',
    ranges: ['`Inline unformattable parts before` |and| `after selection`'],
  },
  {
    source: [
      'Block |types',
      '# Block types',
      '## Block types',
      '### Block types',
      '> Block types',
      '1. Block types',
      '- Block| types',
    ].join('\n'),
    ranges: [
      'Block |types|\n# Block types\n## Block types\n### Block types\n> Block types\n1. Block types\n- Block types',
      'Block types\n# |Block types|\n## Block types\n### Block types\n> Block types\n1. Block types\n- Block types',
      'Block types\n# Block types\n## |Block types|\n### Block types\n> Block types\n1. Block types\n- Block types',
      'Block types\n# Block types\n## Block types\n### |Block types|\n> Block types\n1. Block types\n- Block types',
      'Block types\n# Block types\n## Block types\n### Block types\n> |Block types|\n1. Block types\n- Block types',
      'Block types\n# Block types\n## Block types\n### Block types\n> Block types\n1. |Block types|\n- Block types',
      'Block types\n# Block types\n## Block types\n### Block types\n> Block types\n1. Block types\n- |Block| types',
    ],
  },
])('Range-splitting', ({ source, ranges }) => {
  const input = parseTestInput(source)
  const md = new MarkdownDocument(Text.of([input.doc]), ensoMarkdownParser.parse(input.doc))
  const trim = (range: Range) => md.trimRangeSpaces(trimRangeDelimiters(range, md.tree))
  const selection = trim(Range.unsafeFromBounds(input.selection.anchor, input.selection.head))
  const rangesFound: Range[] = []
  splitRange(selection, md.tree, rangesFound.push.bind(rangesFound), trim)
  expect(
    rangesFound.map((range) => printTestInput(input.doc, { anchor: range.from, head: range.to })),
  ).toEqual(ranges)
})
