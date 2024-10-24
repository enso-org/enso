/**
 * @file
 *
 * Wysiwyg component that provides a WYSIWYG editor using Lexical.js.
 */

import type { RendererObject } from 'marked'
import { marked } from 'marked'
import { useMemo } from 'react'
import { BUTTON_STYLES, TEXT_STYLE } from '../AriaComponents'

/**
 * Props for the Wysiwyg component.
 */
export interface MarkdownViewerProps {
  /**
   * Markdown text to parse and display.
   */
  readonly text: string
}

const renderer: RendererObject = {
  /**
   * Renderer for heading
   */
  heading({ depth, tokens }) {
    return `<h${depth} class="${TEXT_STYLE({ variant: 'h1', className: 'my-2' })}">${this.parser.parseInline(tokens)}</h${depth}>`
  },
  /**
   * Renderer for paragraph
   */
  paragraph({ tokens }) {
    return `<p class="${TEXT_STYLE({ variant: 'body', className: 'my-1' })}">${this.parser.parseInline(tokens)}</p>`
  },
  /**
   * Renderer for list item
   */
  listitem({ tokens }) {
    return `<li class="${TEXT_STYLE({ variant: 'body' })}">${this.parser.parseInline(tokens)}</li>`
  },
  /**
   * Renderer for list
   */
  list({ items }) {
    return `<ul class="my-1 list-disc pl-3">${items.map((item) => this.listitem(item)).join('\n')}</ul>`
  },
  /**
   * Renderer for links
   */
  link({ href, tokens }) {
    return `<a href="${href}" target="_blank" rel="noopener noreferrer" class="${BUTTON_STYLES({ variant: 'link' }).base()}">${this.parser.parseInline(tokens)}</a>`
  },
  /**
   * Renderer for image
   */
  image({ href, title }) {
    return `<img src="${href}" alt="${title}" class="my-1 h-auto max-w-full" />`
  },
  /**
   * Renderer for code
   */
  code({ text }) {
    return `<code class="block my-1 p-2 bg-primary/5 rounded-lg max-w-full overflow-auto max-h-48" >
      <pre class="${TEXT_STYLE({ variant: 'body-sm' })}">${text}</pre>
    </code>`
  },
  /**
   * Renderer for blockquote
   */
  blockquote({ tokens }) {
    return `<blockquote class="${'relative my-1 pl-2 before:bg-primary/20 before:absolute before:left-0 before:top-0 before:h-full before:w-[1.5px] before:rounded-full'}">${this.parser.parse(tokens)}</blockquote>`
  },
}

/**
 * Markdown viewer component.
 * Parses markdown passed in as a `text` prop into HTML and displays it.
 */
export function MarkdownViewer(props: MarkdownViewerProps) {
  const { text } = props

  const markdownToHtml = useMemo(
    () => marked.use({ renderer }).parse(text, { async: false }),
    [text],
  )

  // eslint-disable-next-line @typescript-eslint/naming-convention
  return <div className="select-text" dangerouslySetInnerHTML={{ __html: markdownToHtml }} />
}
