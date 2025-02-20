import { LINE_BOUNDARIES } from 'enso-common/src/utilities/data/string'
import { markdownParser } from './ensoMarkdown'
import { xxHash128 } from './ffi'
import type { ConcreteChild, RawConcreteChild } from './print'
import { ensureUnspaced, firstChild, preferUnspaced, unspaced } from './print'
import { Token, TokenType } from './token'
import type { ConcreteRefs, DeepReadonly, DocLine, TextToken } from './tree'

/** Render a documentation line to concrete tokens. */
export function* docLineToConcrete(
  docLine: DeepReadonly<DocLine>,
  indent: string | null,
): IterableIterator<RawConcreteChild> {
  yield firstChild(docLine.docs.open)
  let prevType = undefined
  let extraIndent = ''
  for (const { token } of docLine.docs.elements) {
    if (token.node.tokenType_ === TokenType.Newline) {
      yield ensureUnspaced(token, false)
    } else {
      if (prevType === TokenType.Newline) {
        yield { whitespace: token.whitespace ?? indent + extraIndent, node: token.node }
      } else {
        if (prevType === undefined) {
          const leadingSpace = token.node.code_.match(/ */)
          extraIndent = '  ' + (leadingSpace ? leadingSpace[0] : '')
        }
        yield { whitespace: '', node: token.node }
      }
    }
    prevType = token.node.tokenType_
  }
  for (const newline of docLine.newlines) yield preferUnspaced(newline)
}

// === Markdown ===

/**
 * Render function documentation to concrete tokens. If the `markdown` content has the same value as when `docLine` was
 * parsed (as indicated by `hash`), the `docLine` will be used (preserving concrete formatting). If it is different, the
 * `markdown` text will be converted to source tokens.
 */
export function functionDocsToConcrete(
  markdown: string,
  hash: string | undefined,
  docLine: DeepReadonly<DocLine> | undefined,
  indent: string | null,
): Iterable<RawConcreteChild> | undefined {
  return (
    hash && docLine && xxHash128(markdown) === hash ? docLineToConcrete(docLine, indent)
    : markdown ? markdownYTextToTokens(markdown, (indent || '') + '   ')
    : undefined
  )
}

function markdownYTextToTokens(yText: string, indent: string): Iterable<ConcreteChild<Token>> {
  const tokensBuilder = new DocTokensBuilder(indent)
  standardizeMarkdown(yText, tokensBuilder)
  return tokensBuilder.build()
}

/**
 * Given Enso documentation comment tokens, returns a model of their Markdown content. This model abstracts away details
 * such as the locations of line breaks that are not paragraph breaks (e.g. lone newlines denoting hard-wrapping of the
 * source code).
 */
export function abstractMarkdown(elements: undefined | TextToken<ConcreteRefs>[]) {
  const { tags, rawMarkdown } = toRawMarkdown(elements)
  const markdown = [...tags, normalizeMarkdown(rawMarkdown)].join('\n')
  const hash = xxHash128(markdown)
  return { markdown, hash }
}

function indentLevel(whitespace: string) {
  return whitespace.length + whitespace.split('\t').length - 1
}

function toRawMarkdown(elements: undefined | TextToken<ConcreteRefs>[]) {
  const tags: string[] = []
  let readingTags = true
  const tokenWhitespace = ({ token: { whitespace } }: TextToken<ConcreteRefs>) => whitespace
  let minWhitespace = Infinity
  if (elements) {
    for (let i = 1; i < elements.length; i++) {
      const whitespace = tokenWhitespace(elements[i]!)
      if (whitespace) minWhitespace = Math.min(minWhitespace, indentLevel(whitespace))
    }
  }
  let rawMarkdown = ''
  ;(elements ?? []).forEach(({ token: { whitespace, node } }, i) => {
    if (node.tokenType_ === TokenType.Newline) {
      if (!readingTags) {
        rawMarkdown += '\n'
      }
    } else {
      let nodeCode = node.code()
      if (i === 0) nodeCode = nodeCode.trimStart()
      if (readingTags) {
        if (nodeCode.startsWith('ICON ')) {
          tags.push(nodeCode)
        } else {
          readingTags = false
        }
      }
      if (!readingTags && nodeCode) {
        if (whitespace && indentLevel(whitespace) > minWhitespace) {
          rawMarkdown += whitespace.replaceAll(/\t/g, '    ').slice(minWhitespace)
        }
        rawMarkdown += nodeCode
      }
    }
  })
  return { tags, rawMarkdown }
}

/**
 * Convert the Markdown input to a format with rendered-style linebreaks: Hard-wrapped lines within a paragraph will be
 * joined, and only a single linebreak character is used to separate paragraphs.
 */
export function normalizeMarkdown(rawMarkdown: string): string {
  let normalized = ''
  let prevTo = 0
  let prevName: string | undefined = undefined
  const cursor = markdownParser.parse(rawMarkdown).cursor()
  cursor.firstChild()
  do {
    if (prevTo < cursor.from) {
      const textBetween = rawMarkdown.slice(prevTo, cursor.from)
      normalized +=
        cursor.name === 'Paragraph' && prevName !== 'Table' ? textBetween.slice(0, -1) : textBetween
    }
    const text = rawMarkdown.slice(cursor.from, cursor.to)
    normalized += cursor.name === 'Paragraph' ? text.replaceAll(/ *\n */g, ' ') : text
    prevTo = cursor.to
    prevName = cursor.name
  } while (cursor.nextSibling())
  return normalized
}

function stringCollector() {
  let output = ''
  const collector = {
    text: (text: string) => (output += text),
    wrapText: (text: string) => (output += text),
    newline: () => (output += '\n'),
  }
  return { collector, output }
}

/**
 * Convert from "normalized" Markdown (with hard line-breaks removed) to the standard format, with paragraphs separated
 * by blank lines.
 */
export function normalizedMarkdownToStandard(normalizedMarkdown: string) {
  const { collector, output } = stringCollector()
  standardizeMarkdown(normalizedMarkdown, collector)
  return output
}

/**
 * Convert from "normalized" Markdown to the on-disk representation, with paragraphs hard-wrapped and separated by blank
 * lines.
 */
function standardizeMarkdown(normalizedMarkdown: string, textConsumer: TextConsumer) {
  let printingTags = true
  const cursor = markdownParser.parse(normalizedMarkdown).cursor()

  function standardizeDocument() {
    let prevTo = 0
    let prevName: string | undefined = undefined
    cursor.firstChild()
    do {
      if (prevTo < cursor.from) {
        const betweenText = normalizedMarkdown.slice(prevTo, cursor.from)
        for (const _match of betweenText.matchAll(LINE_BOUNDARIES)) {
          textConsumer.newline()
        }
        if (cursor.name === 'Paragraph' && prevName !== 'Table') {
          textConsumer.newline()
        }
      }
      const lines = normalizedMarkdown.slice(cursor.from, cursor.to).split(LINE_BOUNDARIES)
      if (cursor.name === 'Paragraph') {
        standardizeParagraph(lines)
      } else {
        lines.forEach((line, i) => {
          if (i > 0) textConsumer.newline()
          textConsumer.text(line)
        })
        printingTags = false
      }
      prevTo = cursor.to
      prevName = cursor.name
    } while (cursor.nextSibling())
  }

  function standardizeParagraph(lines: string[]) {
    let printingNonTags = false
    lines.forEach((line, i) => {
      if (printingTags) {
        if (cursor.name === 'Paragraph' && line.startsWith('ICON ')) {
          textConsumer.text(line)
        } else {
          printingTags = false
        }
      }
      if (!printingTags) {
        if (i > 0) {
          textConsumer.newline()
          if (printingNonTags) textConsumer.newline()
        }
        textConsumer.wrapText(line)
        printingNonTags = true
      }
    })
  }

  standardizeDocument()
}

interface TextConsumer {
  text: (text: string) => void
  wrapText: (text: string) => void
  newline: () => void
}

class DocTokensBuilder implements TextConsumer {
  private readonly tokens: ConcreteChild<Token>[] = [unspaced(Token.new('##', TokenType.TextStart))]

  constructor(private readonly indent: string) {}

  text(text: string): void {
    const whitespace = this.tokens.length === 1 ? ' ' : this.indent
    this.tokens.push({ whitespace, node: Token.new(text, TokenType.TextSection) })
  }

  wrapText(text: string): void {
    this.text(text)
  }

  newline(): void {
    this.tokens.push(unspaced(Token.new('\n', TokenType.Newline)))
  }

  build(): ConcreteChild<Token>[] {
    this.newline()
    return this.tokens
  }
}
