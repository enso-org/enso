import { Tree, TreeBuffer, TreeCursor } from '@lezer/common'
import {
  type BlockContext,
  type BlockParser,
  type DelimiterType,
  type InlineContext,
  type InlineDelimiter,
  type InlineParser,
  type Line,
  type MarkdownParser,
  type NodeSpec,
  parser as commonmarkParser,
  Element,
  Table,
} from '@lezer/markdown'
import { assertDefined } from '../util/assert'

/**
 *  Private lezer-markdown symbols used by lezer-markdown parsers we have customized versions of.
 */
declare module '@lezer/markdown' {
  export interface BlockContext {
    block: CompositeBlock
    stack: CompositeBlock[]
    readonly buffer: Buffer

    addNode: (block: number | Tree, from: number, to?: number) => void
    startContext: (type: number, start: number, value?: number) => void
  }

  export interface CompositeBlock {
    readonly type: number
    // Used for indentation in list items, markup character in lists
    readonly value: number
    readonly from: number
    readonly hash: number
    end: number
    readonly children: (Tree | TreeBuffer)[]
    readonly positions: number[]
  }

  export interface Buffer {
    content: number[]
    nodes: Tree[]

    write: (type: number, from: number, to: number, children?: number) => Buffer
    writeElements: (elts: readonly Element[], offset?: number) => Buffer
    finish: (type: number, length: number) => Tree
  }

  export interface InlineDelimiter {
    readonly type: DelimiterType
    readonly from: number
    readonly to: number
    side: Mark
  }

  export interface InlineContext {
    parts: (Element | InlineDelimiter | null)[]
  }
}

function getType({ parser }: { parser: MarkdownParser }, name: string) {
  const ty = parser.nodeSet.types.find((ty) => ty.name === name)
  assertDefined(ty)
  return ty.id
}

/** Parser override to include the space in the delimiter. */
const headerParser: BlockParser = {
  name: 'ATXHeading',
  parse: (cx, line) => {
    let size = isAtxHeading(line)
    if (size < 0) return false
    const level = size
    // If the character after the hashes is a space, treat it as part of the `HeaderMark`.
    if (isSpace(line.text.charCodeAt(size))) size += 1
    const off = line.pos
    const from = cx.lineStart + off
    // Trailing spaces at EOL
    const endOfSpace = skipSpaceBack(line.text, line.text.length, off)
    let after = endOfSpace
    // Trailing sequence of # (before EOL spaces)
    while (after > off && line.text.charCodeAt(after - 1) == line.next) after--
    if (after == endOfSpace || after == off || !isSpace(line.text.charCodeAt(after - 1)))
      after = line.text.length
    const headerMark = getType(cx, 'HeaderMark')
    const buf = cx.buffer
      .write(headerMark, 0, size)
      .writeElements(cx.parser.parseInline(line.text.slice(off + size, after), from + size), -from)
    if (after < line.text.length) buf.write(headerMark, after - off, endOfSpace - off)
    const node = buf.finish(getType(cx, `ATXHeading${level}`), line.text.length - off)
    cx.nextLine()
    cx.addNode(node, from)
    return true
  },
}

/** Parser override to include the space in the delimiter. */
const bulletList: BlockParser = {
  name: 'BulletList',
  parse: (cx, line) => {
    const size = isBulletList(line, cx, false)
    if (size < 0) return false
    const length = size + (isSpace(line.text.charCodeAt(line.pos + 1)) ? 1 : 0)
    const bulletList = getType(cx, 'BulletList')
    if (cx.block.type != bulletList) cx.startContext(bulletList, line.basePos, line.next)
    const newBase = getListIndent(line, line.pos + 1)
    cx.startContext(getType(cx, 'ListItem'), line.basePos, newBase - line.baseIndent)
    cx.addNode(getType(cx, 'ListMark'), cx.lineStart + line.pos, cx.lineStart + line.pos + length)
    line.moveBaseColumn(newBase)
    return null
  },
}

/** Parser override to include the space in the delimiter. */
const orderedList: BlockParser = {
  name: 'OrderedList',
  parse: (cx, line) => {
    const size = isOrderedList(line, cx, false)
    if (size < 0) return false
    const orderedList = getType(cx, 'OrderedList')
    if (cx.block.type != orderedList)
      cx.startContext(orderedList, line.basePos, line.text.charCodeAt(line.pos + size - 1))
    const newBase = getListIndent(line, line.pos + size)
    cx.startContext(getType(cx, 'ListItem'), line.basePos, newBase - line.baseIndent)
    cx.addNode(getType(cx, 'ListMark'), cx.lineStart + line.pos, cx.lineStart + line.pos + size)
    line.moveBaseColumn(newBase)
    return null
  },
}

const ENSO_BLOCKQUOTE_TYPE = 'EnsoBlockquote'

/** Parser override to include the space in the delimiter. */
const blockquoteParser: BlockParser = {
  name: ENSO_BLOCKQUOTE_TYPE,
  parse: (cx, line) => {
    const size = isBlockquote(line)
    if (size < 0) return false
    const type = getType(cx, ENSO_BLOCKQUOTE_TYPE)
    cx.startContext(type, line.pos)
    cx.addNode(getType(cx, 'QuoteMark'), cx.lineStart + line.pos, cx.lineStart + line.pos + size)
    line.moveBase(line.pos + size)
    return null
  },
  before: 'Blockquote',
}

/**
 * Replaces setext heading parser with a parser that never matches.
 *
 * When starting a bulleted list, the `SetextHeading` parser can match when a `-` has been typed and a following space
 * hasn't been entered yet; the resulting style changes are distracting. To prevent this, we don't support setext
 * headings; ATX headings seem to be much more popular anyway.
 */
const disableSetextHeading: BlockParser = {
  name: 'SetextHeading',
  parse: () => false,
}

const blockquoteNode: NodeSpec = {
  name: ENSO_BLOCKQUOTE_TYPE,
  block: true,
  composite: (cx, line) => {
    if (line.next != 62 /* '>' */) return false
    const size = isSpace(line.text.charCodeAt(line.pos + 1)) ? 2 : 1
    line.addMarker(
      elt(getType(cx, 'QuoteMark'), cx.lineStart + line.pos, cx.lineStart + line.pos + size),
    )
    line.moveBase(line.pos + size)
    //bl.end = cx.lineStart + line.text.length
    return true
  },
}

function elt(type: number, from: number, to: number, children?: readonly Element[]): Element {
  return new (Element as any)(type, from, to, children)
}

function isBlockquote(line: Line) {
  return (
    line.next != 62 /* '>' */ ? -1
    : line.text.charCodeAt(line.pos + 1) == 32 ? 2
    : 1
  )
}

function isBulletList(line: Line, cx: BlockContext, breaking: boolean) {
  return (
      (line.next == 45 || line.next == 43 || line.next == 42) /* '-+*' */ &&
        (line.pos == line.text.length - 1 || isSpace(line.text.charCodeAt(line.pos + 1))) &&
        (!breaking || inList(cx, 'BulletList') || line.skipSpace(line.pos + 2) < line.text.length)
    ) ?
      1
    : -1
}

function isOrderedList(line: Line, cx: BlockContext, breaking: boolean) {
  let pos = line.pos
  let next = line.next
  for (;;) {
    if (next >= 48 && next <= 57 /* '0-9' */) pos++
    else break
    if (pos == line.text.length) return -1
    next = line.text.charCodeAt(pos)
  }
  if (
    pos == line.pos ||
    pos > line.pos + 9 ||
    (next != 46 && next != 41) /* '.)' */ ||
    (pos < line.text.length - 1 && !isSpace(line.text.charCodeAt(pos + 1))) ||
    (breaking &&
      !inList(cx, 'OrderedList') &&
      (line.skipSpace(pos + 1) == line.text.length ||
        pos > line.pos + 1 ||
        line.next != 49)) /* '1' */
  )
    return -1
  return pos + 1 - line.pos
}

function inList(cx: BlockContext, typeName: string) {
  const type = getType(cx, typeName)
  for (let i = cx.stack.length - 1; i >= 0; i--) if (cx.stack[i]!.type == type) return true
  return false
}

function getListIndent(line: Line, pos: number) {
  const indentAfter = line.countIndent(pos, line.pos, line.indent)
  const indented = line.countIndent(line.skipSpace(pos), pos, indentAfter)
  return indented >= indentAfter + 5 ? indentAfter + 1 : indented
}

// === Link ===

const enum Mark {
  None = 0,
  Open = 1,
  Close = 2,
}

const LinkStart: DelimiterType = {}
const ImageStart: DelimiterType = {}

const linkParser: InlineParser = {
  name: 'Link',
  parse: (cx, next, start) => {
    return next == 91 /* '[' */ ? cx.addDelimiter(LinkStart, start, start + 1, true, false) : -1
  },
}

const imageParser: InlineParser = {
  name: 'Image',
  parse: (cx, next, start) => {
    return next == 33 /* '!' */ && cx.char(start + 1) == 91 /* '[' */ ?
        cx.addDelimiter(ImageStart, start, start + 2, true, false)
      : -1
  },
}

const linkEndParser: InlineParser = {
  name: 'LinkEnd',
  parse: (cx, next, start) => {
    if (next != 93 /* ']' */) return -1
    // Scanning back to the next link/image start marker
    const openDelim = cx.findOpeningDelimiter(LinkStart) ?? cx.findOpeningDelimiter(ImageStart)
    if (openDelim == null) return -1
    const part = cx.parts[openDelim] as InlineDelimiter
    // If this one has been set invalid (because it would produce
    // a nested link) or there's no valid link here ignore both.
    if (
      !part.side ||
      (cx.skipSpace(part.to) == start && !/[([]/.test(cx.slice(start + 1, start + 2)))
    ) {
      cx.parts[openDelim] = null
      return -1
    }
    // Finish the content and replace the entire range in
    // this.parts with the link/image node.
    const content = cx.takeContent(openDelim)
    const link = (cx.parts[openDelim] = finishLink(
      cx,
      content,
      part.type == LinkStart ? getType(cx, 'Link') : getType(cx, 'Image'),
      part.from,
      start + 1,
    ))
    // Set any open-link markers before this link to invalid.
    if (part.type == LinkStart)
      for (let j = 0; j < openDelim; j++) {
        const p = cx.parts[j]
        if (p != null && !(p instanceof Element) && p.type == LinkStart) p.side = Mark.None
      }
    return link.to
  },
}

function finishLink(
  cx: InlineContext,
  content: Element[],
  type: number,
  start: number,
  startPos: number,
) {
  const { text } = cx,
    next = cx.char(startPos)
  let endPos = startPos
  const LinkMarkType = getType(cx, 'LinkMark')
  const ImageType = getType(cx, 'Image')
  content.unshift(elt(LinkMarkType, start, start + (type == ImageType ? 2 : 1)))
  content.push(elt(LinkMarkType, startPos - 1, startPos))
  if (next == 40 /* '(' */) {
    let pos = cx.skipSpace(startPos + 1)
    const dest = parseURL(text, pos - cx.offset, cx.offset, getType(cx, 'URL'), LinkMarkType)
    let title
    if (dest) {
      const last = dest.at(-1)!
      pos = cx.skipSpace(last.to)
      // The destination and title must be separated by whitespace
      if (pos != last.to) {
        title = parseLinkTitle(text, pos - cx.offset, cx.offset, getType(cx, 'LinkTitle'))
        if (title) pos = cx.skipSpace(title.to)
      }
    }
    if (cx.char(pos) == 41 /* ')' */) {
      content.push(elt(LinkMarkType, startPos, startPos + 1))
      endPos = pos + 1
      if (dest) content.push(...dest)
      if (title) content.push(title)
      content.push(elt(LinkMarkType, pos, endPos))
    }
  } else if (next == 91 /* '[' */) {
    const label = parseLinkLabel(
      text,
      startPos - cx.offset,
      cx.offset,
      false,
      getType(cx, 'LinkLabelType'),
    )
    if (label) {
      content.push(label)
      endPos = label.to
    }
  }
  return elt(type, start, endPos, content)
}

// These return `null` when falling off the end of the input, `false`
// when parsing fails otherwise (for use in the incremental link
// reference parser).
function parseURL(
  text: string,
  start: number,
  offset: number,
  urlType: number,
  linkMarkType: number,
): null | false | Element[] {
  const next = text.charCodeAt(start)
  if (next == 60 /* '<' */) {
    for (let pos = start + 1; pos < text.length; pos++) {
      const ch = text.charCodeAt(pos)
      if (ch == 62 /* '>' */)
        return [
          elt(linkMarkType, start + offset, start + offset + 1),
          elt(urlType, start + offset + 1, pos + offset),
          elt(linkMarkType, pos + offset, pos + offset + 1),
        ]
      if (ch == 60 || ch == 10 /* '<\n' */) return false
    }
    return null
  } else {
    let depth = 0,
      pos = start
    for (let escaped = false; pos < text.length; pos++) {
      const ch = text.charCodeAt(pos)
      if (isSpace(ch)) {
        break
      } else if (escaped) {
        escaped = false
      } else if (ch == 40 /* '(' */) {
        depth++
      } else if (ch == 41 /* ')' */) {
        if (!depth) break
        depth--
      } else if (ch == 92 /* '\\' */) {
        escaped = true
      }
    }
    return (
      pos > start ? [elt(urlType, start + offset, pos + offset)]
      : pos == text.length ? null
      : false
    )
  }
}

function parseLinkTitle(
  text: string,
  start: number,
  offset: number,
  linkTitleType: number,
): null | false | Element {
  const next = text.charCodeAt(start)
  if (next != 39 && next != 34 && next != 40 /* '"\'(' */) return false
  const end = next == 40 ? 41 : next
  for (let pos = start + 1, escaped = false; pos < text.length; pos++) {
    const ch = text.charCodeAt(pos)
    if (escaped) escaped = false
    else if (ch == end) return elt(linkTitleType, start + offset, pos + 1 + offset)
    else if (ch == 92 /* '\\' */) escaped = true
  }
  return null
}

function parseLinkLabel(
  text: string,
  start: number,
  offset: number,
  requireNonWS: boolean,
  linkLabelType: number,
): null | false | Element {
  for (
    let escaped = false, pos = start + 1, end = Math.min(text.length, pos + 999);
    pos < end;
    pos++
  ) {
    const ch = text.charCodeAt(pos)
    if (escaped) escaped = false
    else if (ch == 93 /* ']' */)
      return requireNonWS ? false : elt(linkLabelType, start + offset, pos + 1 + offset)
    else {
      if (requireNonWS && !isSpace(ch)) requireNonWS = false
      if (ch == 91 /* '[' */) return false
      else if (ch == 92 /* '\\' */) escaped = true
    }
  }
  return null
}

// === Debugging ===

/** Represents the structure of a @{link Tree} in a JSON-compatible format. */
export type DebugTree = (string | DebugTree)[]

/** @returns A debug representation of the provided {@link Tree} */
export function debugTree(tree: { cursor: () => TreeCursor }, doc: string): DebugTree {
  const cursor = tree.cursor()
  let current: (string | DebugTree)[] = []
  const stack: (string | DebugTree)[][] = []
  cursor.iterate(
    (node) => {
      const child: (string | DebugTree)[] = [node.name]
      current.push(child)
      stack.push(current)
      current = child
    },
    (node) => {
      if (current.length === 1) current.push(doc.slice(node.from, node.to))
      current = stack.pop()!
    },
  )
  return current[0]! as DebugTree
}

// === Helpers ===

function skipSpaceBack(line: string, i: number, to: number) {
  while (i > to && isSpace(line.charCodeAt(i - 1))) i--
  return i
}

/** Returns the number of hash marks at the beginning of the line, or -1 if it is not in the range [1, 6] */
function isAtxHeading(line: Line) {
  if (line.next != 35 /* '#' */) return -1
  let pos = line.pos + 1
  while (pos < line.text.length && line.text.charCodeAt(pos) == 35) pos++
  if (pos < line.text.length && line.text.charCodeAt(pos) != 32) return -1
  const size = pos - line.pos
  return size > 6 ? -1 : size
}

function isSpace(ch: number) {
  return ch == 32 || ch == 9 || ch == 10 || ch == 13
}

const ensoMarkdownLanguageExtension = {
  parseBlock: [headerParser, bulletList, orderedList, blockquoteParser, disableSetextHeading],
  parseInline: [linkParser, imageParser, linkEndParser],
  defineNodes: [blockquoteNode],
}

/**
 * Lezer (CodeMirror) parser for the Enso documentation Markdown dialect.
 * Differences from CodeMirror's base Markdown language:
 * - It defines the flavor of Markdown supported in Enso documentation. Currently, this is mostly CommonMark except we
 *   don't support setext headings. Planned features include support for some GFM extensions.
 * - Many of the parsers differ from the `@lezer/markdown` parsers in their treatment of whitespace, in order to support
 *   a rendering mode where markup (and some associated spacing) is hidden.
 */
export const markdownParser: MarkdownParser = commonmarkParser.configure([
  Table,
  ensoMarkdownLanguageExtension,
])
