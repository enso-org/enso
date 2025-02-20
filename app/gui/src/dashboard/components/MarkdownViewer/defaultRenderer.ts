/** @file The default renderer for Markdown. */
import type { RendererObject } from 'marked'
import { BUTTON_STYLES, TEXT_STYLE } from '../AriaComponents'

/** The default renderer for Markdown. */
export const DEFAULT_RENDERER: Readonly<RendererObject> = {
  /** The renderer for headings. */
  heading({ depth, tokens }) {
    const variant = depth === 1 ? 'h1' : 'subtitle'
    return `<h${depth} class="${TEXT_STYLE({ variant: variant, className: 'my-2' })}">${this.parser.parseInline(tokens)}</h${depth}>`
  },
  /** The renderer for paragraphs. */
  paragraph({ tokens }) {
    return `<p class="${TEXT_STYLE({ variant: 'body', className: 'my-1' })}">${this.parser.parseInline(tokens)}</p>`
  },
  /** The renderer for list items. */
  listitem({ tokens }) {
    return `<li class="${TEXT_STYLE({ variant: 'body' })}">${this.parser.parseInline(tokens)}</li>`
  },
  /** The renderer for lists. */
  list({ items }) {
    return `<ul class="my-1 list-disc pl-3">${items.map((item) => this.listitem(item)).join('\n')}</ul>`
  },
  /** The renderer for links. */
  link({ href, tokens }) {
    return `<a href="${href}" target="_blank" rel="noopener noreferrer" class="${BUTTON_STYLES({ variant: 'link' }).base()}">${this.parser.parseInline(tokens)}</a>`
  },
  /** The renderer for images. */
  image({ href, title, raw }) {
    const alt = title ?? ''

    return `
      <img src="${href}" alt="${alt}" class="my-1 h-auto max-w-full" data-raw=${raw}>
    `
  },
  /** The renderer for code. */
  code({ text }) {
    return `<code class="block my-1 p-2 bg-primary/5 rounded-lg max-w-full overflow-auto max-h-48" >
      <pre class="${TEXT_STYLE({ variant: 'body-sm' })}">${text}</pre>
    </code>`
  },
  /** The renderer for blockquotes. */
  blockquote({ tokens }) {
    return `<blockquote class="${'relative my-1 pl-2 before:bg-primary/20 before:absolute before:left-0 before:top-0 before:h-full before:w-[1.5px] before:rounded-full'}">${this.parser.parse(tokens)}</blockquote>`
  },
}
