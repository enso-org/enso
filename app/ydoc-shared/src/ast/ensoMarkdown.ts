/**
 * @file Define the Enso Markdown dialect. We customize @lezer/markdown in two different ways: Changes that can easily
 * be made through its configuration interface are applied here. Changes that are impossible or overly complicated to
 * achieve via the public interface are implemented by modifying our implementation of the package.
 */
import {
  parser as commonmarkParser,
  Strikethrough,
  Table,
  type BlockParser,
  type MarkdownExtension,
} from '@lezer/markdown'

/**
 * End any element when a newline is encountered. This parser operates on preprocessed Markdown that has "prerendered"
 * newlines: Before parsing, hard-wrapped lines within any block element are concatenated, and the extra newlines
 * between block elements are removed.
 */
const newlineEndsBlock: BlockParser = {
  name: 'NewlineEndsBlock',
  endLeaf: () => true,
}

/** @lezer/markdown extension for the Markdown dialect used in the Enso documentation editor. */
export const ensoMarkdownExtension: MarkdownExtension = [
  Table,
  Strikethrough,
  { parseBlock: [newlineEndsBlock] },
  /**
   * When starting a bulleted list, the `SetextHeading` parser can match when a `-` has been typed and a following space
   * hasn't been entered yet; the resulting style changes are distracting. To prevent this, we don't support setext
   * headings; ATX headings seem to be much more popular anyway.
   */
  { remove: ['SetextHeading'] },
]

/** Headless @lezer/markdown parser for the Markdown dialect used in the Enso documentation editor. */
export const ensoMarkdownParser = commonmarkParser.configure(ensoMarkdownExtension)
