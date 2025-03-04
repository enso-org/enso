/** @file A Markdown viewer component. */
import { vueComponent } from '#/utilities/vue'
import { type UrlTransformer } from '@/components/MarkdownEditor/imageUrlTransformer'

const MarkdownEditor = vueComponent(() => import('@/components/MarkdownEditor.vue'))

/** Props for a {@link MarkdownViewer}. */
export interface MarkdownViewerProps {
  /** Markdown markup to parse and display. */
  readonly text: string
  readonly imgUrlResolver: (relativePath: string) => Promise<string>
}

/**
 * Markdown viewer component.
 * Parses markdown passed in as a `text` prop into HTML and displays it.
 */
export function MarkdownViewer(props: MarkdownViewerProps) {
  const { text, imgUrlResolver } = props
  const transformImageUrl: UrlTransformer = (path: string) =>
    /^https?:/.test(path) ?
      Promise.resolve({ ok: true, value: { url: path } })
    : imgUrlResolver(path).then((url) => ({ ok: true, value: { url } }))

  return <MarkdownEditor content={text} transformImageUrl={transformImageUrl} toolbar={false} />
}
