/**
 * @file Define the Enso Markdown dialect. We customize @lezer/markdown in two different ways: Changes that can easily
 * be made through its configuration interface are applied here. Changes that are impossible or overly complicated to
 * achieve via the public interface are implemented by modifying our implementation of the package.
 */
import {
  BlockContext,
  parser as commonmarkParser,
  Line,
  Strikethrough,
  Table,
  type BlockParser,
  type MarkdownConfig,
  type MarkdownExtension,
  type MarkdownParser,
} from '@lezer/markdown'

// noinspection JSUnusedGlobalSymbols (WebStorm thinks `endLeaf` is unused, unclear why)
/**
 * End any element when a newline is encountered. This parser operates on preprocessed Markdown that
 * has "prerendered" newlines: Before parsing, hard-wrapped lines within any block element are
 * concatenated, and the extra newlines between block elements are removed.
 */
const newlineEndsBlock: BlockParser = {
  name: 'NewlineEndsBlock',
  endLeaf: (_cx, line) =>
    !(line.text.startsWith('|') && line.text.length > 2 && line.text.endsWith('|')),
}

function isFirstLine(ctx: BlockContext): boolean {
  return ctx.prevLineEnd() === -1
}

/**
 * A parser for ‘metadata’ section of the Markdown docs.
 * A format similar to https://jekyllrb.com/docs/front-matter/.
 */
const YAMLFrontMatter: MarkdownConfig = {
  defineNodes: ['YAMLFrontMatter', 'YAMLMarker', 'YAMLContent'],
  parseBlock: [
    {
      name: 'YAMLFrontMatter',
      parse(ctx: BlockContext, line: Line) {
        if (!isFirstLine(ctx)) {
          return false
        }
        const regex = /^\s*---\s*$/
        const start = ctx.lineStart
        if (regex.test(line.text)) {
          const contentStart = ctx.lineStart + line.text.length
          const startMarker = ctx.elt('YAMLMarker', ctx.lineStart, contentStart)
          while (ctx.nextLine()) {
            if (regex.test(line.text)) {
              const content = ctx.elt('YAMLContent', contentStart, ctx.lineStart)
              const end = ctx.lineStart + line.text.length
              const endMarker = ctx.elt('YAMLMarker', ctx.lineStart, end)
              ctx.addElement(
                ctx.elt('YAMLFrontMatter', start, end, [startMarker, content, endMarker]),
              )
              ctx.nextLine()
              return true
            }
          }
          return true
        } else {
          return false
        }
      },
      before: 'LinkReference',
    },
  ],
}

const ensoMarkdownDialect = [
  Table,
  Strikethrough,
  YAMLFrontMatter,
  /**
   * When starting a bulleted list, the `SetextHeading` parser can match when a `-` has been typed
   * and a following space hasn't been entered yet; the resulting style changes are distracting. To
   * prevent this, we don't support setext headings; ATX headings seem to be much more popular
   * anyway.
   */
  { remove: ['SetextHeading'] },
]

const prerenderedRepresentation = { parseBlock: [newlineEndsBlock] }

/** {@link MarkdownExtension} for Markdown as used in the Enso documentation editor. */
export const ensoMarkdownExtension: MarkdownExtension = [
  ensoMarkdownDialect,
  prerenderedRepresentation,
]

/**
 * Headless {@link MarkdownParser} for the Markdown representation used by the Enso documentation
 * editor.
 *
 * This parses the working representation that the documentation editor operates on, with
 * "prerendered" newlines.
 */
export const ensoMarkdownParser: MarkdownParser = commonmarkParser.configure(ensoMarkdownExtension)

/**
 * Headless {@link MarkdownParser} for the Markdown dialect used in Enso documentation comments.
 *
 * This parses the "standard" representation as appears in Enso files.
 */
export const ensoStandardMarkdownParser: MarkdownParser =
  commonmarkParser.configure(ensoMarkdownDialect)
