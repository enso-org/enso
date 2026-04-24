import * as Y from 'yjs'
import { ensoMarkdownParser, ensoStandardMarkdownParser } from './ensoMarkdown'
import { xxHash128 } from './ffi'
import type { ConcreteChild, RawConcreteChild } from './print'
import { firstChild, preferUnspaced, unspaced } from './print'
import { Token, TokenType } from './token'
import type { ConcreteRefs, DeepReadonly, DocLine, TextToken } from './tree'

/** See http://www.unicode.org/reports/tr18/#Line_Boundaries */
export const LINE_BOUNDARIES = /\r\n|[\n\v\f\r\x85\u2028\u2029]/g

// === AST logic ===

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
      yield preferUnspaced(token)
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

/**
 * Render function documentation to concrete tokens. If the `markdown` content has the same value as
 * when `docLine` was parsed (as indicated by `hash`), the `docLine` will be used (preserving
 * concrete formatting). If it is different, the `markdown` text will be converted to source tokens.
 */
export function functionDocsToConcrete(
  markdown: DeepReadonly<Y.Text>,
  hash: string | undefined,
  docLine: DeepReadonly<DocLine> | undefined,
  indent: string | null,
): Iterable<RawConcreteChild> | undefined {
  const markdownText = markdown.toString()
  if (hash && docLine && xxHash128(markdownText) === hash) return docLineToConcrete(docLine, indent)
  if (!markdownText) return
  const tokensBuilder = new DocTokensBuilder((indent || '') + '   ')
  standardizeMarkdown(markdownText, tokensBuilder)
  return tokensBuilder.build()
}

/**
 * Given Enso documentation comment tokens, returns a model of their Markdown content. This model
 * abstracts away details such as the locations of line breaks that are not paragraph breaks (e.g.
 * lone newlines denoting hard-wrapping of the source code).
 */
export function abstractMarkdown(elements: undefined | TextToken<ConcreteRefs>[]): {
  markdown: Y.Text
  hash: string
} {
  const { tags, rawMarkdown } = toRawMarkdown(elements)
  const markdown = [...tags, prerenderMarkdown(rawMarkdown)].join('\n')
  const hash = xxHash128(markdown)
  return { markdown: new Y.Text(markdown), hash }
}

function indentLevel(whitespace: string) {
  return whitespace.length + whitespace.split('\t').length - 1
}

function toRawMarkdown(elements: undefined | TextToken<ConcreteRefs>[]): {
  tags: string[]
  rawMarkdown: string
} {
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

// === Markdown ===

/**
 * @returns Whether following text after a soft break will be interpreted as a continuation of the
 * node, rather than a separate paragraph node.
 *
 * This must match the behavior covered by the "Soft break" tests in `ensoMarkdown.test.ts`.
 */
function requiresNewlineBeforeFollowingParagraph(nodeType: string) {
  switch (nodeType) {
    case 'ATXHeading1':
    case 'ATXHeading2':
    case 'ATXHeading3':
    case 'ATXHeading4':
    case 'ATXHeading5':
    case 'ATXHeading6':
    case 'Blockquote':
    case 'FencedCode':
    case 'Table':
      return false
    case 'Paragraph':
    case 'BulletList':
    case 'OrderedList':
      return true
    default:
      // The safer default is to treat the newline as creating a new element, not a wrapped
      // continuation of the previous element.
      return false
  }
}

/**
 * Convert the Markdown input to a format with "prerendered" linebreaks: Hard-wrapped lines within
 * a paragraph will be joined, and only a single linebreak character is used to separate paragraphs.
 */
export function prerenderMarkdown(markdown: string): string {
  let prerendered = ''
  const cursor = ensoStandardMarkdownParser.parse(markdown).cursor()
  cursor.firstChild()

  /** Remove the trailing newline from a block followed by the Paragraph block, if necessary. */
  function mergeSubsequentBlocks(
    currentNodeName: string,
    text: string,
    prevName: string | undefined,
  ) {
    if (
      currentNodeName === 'Paragraph' &&
      prevName &&
      requiresNewlineBeforeFollowingParagraph(prevName)
    ) {
      return text.slice(0, -1)
    }
    return text
  }

  function prerenderTree(prevTo: number) {
    let prevName: string | undefined = undefined
    do {
      if (prevTo < cursor.from) {
        const textBetween = markdown.slice(prevTo, cursor.from)
        prerendered += mergeSubsequentBlocks(cursor.name, textBetween, prevName)
      }
      const text = markdown.slice(cursor.from, cursor.to)
      if (cursor.name === 'Paragraph') {
        prerendered += text.replaceAll(/ *\n */g, ' ')
      } else if (!cursor.name.startsWith('ATXHeading') && cursor.firstChild()) {
        prerenderTree(cursor.from)
        cursor.parent()
      } else {
        prerendered += text
      }
      prevTo = cursor.to
      prevName = cursor.name
    } while (cursor.nextSibling())
  }

  prerenderTree(0)
  return prerendered
}

/**
 * Convert from our internal "prerendered" Markdown to the (more standard-compatible) on-disk
 * representation, with paragraphs hard-wrapped and separated by blank lines.
 */
function standardizeMarkdown(prerenderedMarkdown: string, textConsumer: TextConsumer): void {
  const cursor = ensoMarkdownParser.parse(prerenderedMarkdown).cursor()

  function standardizeDocument() {
    let prevTo = 0
    let prevName: string | undefined = undefined
    cursor.firstChild()
    do {
      if (prevTo < cursor.from) {
        const betweenText = prerenderedMarkdown.slice(prevTo, cursor.from)
        for (const _match of betweenText.matchAll(LINE_BOUNDARIES)) {
          textConsumer.newline()
        }
        if (cursor.name === 'Paragraph' && prevName === 'Paragraph') {
          textConsumer.newline()
        }
      }
      const lines = prerenderedMarkdown.slice(cursor.from, cursor.to).split(LINE_BOUNDARIES)
      if (cursor.name === 'Paragraph') {
        standardizeParagraph(lines)
      } else {
        lines.forEach((line, i) => {
          if (i > 0) textConsumer.newline()
          textConsumer.text(line)
        })
      }
      prevTo = cursor.to
      prevName = cursor.name
    } while (cursor.nextSibling())
  }

  function standardizeParagraph(lines: string[]) {
    let printingNonTags = false
    lines.forEach((line, i) => {
      if (i > 0) {
        textConsumer.newline()
        if (printingNonTags) textConsumer.newline()
      }
      textConsumer.wrapText(line)
      printingNonTags = true
    })
  }

  standardizeDocument()
}

// === AST utilities ===

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
