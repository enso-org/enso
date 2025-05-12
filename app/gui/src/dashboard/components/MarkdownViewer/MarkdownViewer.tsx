/** @file A Markdown viewer component. */

import { useLogger } from '#/providers/LoggerProvider'
import { useText } from '#/providers/TextProvider'
import { resolveDocImageUrl } from '@/components/DocumentationEditor/images'
import { type UrlTransformer } from '@/components/MarkdownEditor/imageUrlTransformer'
import { Err, Ok } from '@/util/data/result'
import { type TestIdProps } from '../AriaComponents'
import { MarkdownEditor } from './defaultRenderer'

/** Props for a {@link MarkdownViewer}. */
export interface MarkdownViewerProps extends TestIdProps {
  /** Markdown markup to parse and display. */
  readonly text: any
  readonly toolbar?: boolean
  readonly imgUrlResolver: (relativePath: string) => Promise<string>
}

/**
 * Markdown viewer component.
 * Parses markdown passed in as a `text` prop into HTML and displays it.
 */
export function MarkdownViewer(props: MarkdownViewerProps) {
  const { text, imgUrlResolver, testId, toolbar = false } = props

  const logger = useLogger()
  const { getText } = useText()

  const transformImageUrl: UrlTransformer = (path: string) => {
    // In Enso Documentation, the relative paths are from module's directory
    // Here we always display docs from `src/Main.enso` module
    const resolvedUrl = resolveDocImageUrl(['src'], path)
    if (!resolvedUrl.ok) return Promise.resolve(resolvedUrl)
    if (resolvedUrl.value.type === 'url') {
      return Promise.resolve(Ok({ url: resolvedUrl.value.url.toString() }))
    } else {
      return imgUrlResolver(resolvedUrl.value.path).then(
        (url) => Ok({ url }),
        (error) => {
          logger.error(error)
          return Err(getText('arbitraryFetchImageError'))
        },
      )
    }
  }

  return (
    <MarkdownEditor
      content={text}
      transformImageUrl={transformImageUrl}
      toolbar={toolbar}
      data-testid={testId}
      contentTestId="cmContent"
    />
  )
}
