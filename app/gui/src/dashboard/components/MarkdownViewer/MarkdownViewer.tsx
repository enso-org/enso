/** @file A Markdown viewer component. */
import * as React from 'react'

import { useLogger } from '#/providers/LoggerProvider'
import { useText } from '#/providers/TextProvider'
import { type UrlTransformer } from '@/components/MarkdownEditor/imageUrlTransformer'
import { Err, Ok } from '@/util/data/result'
import { type TestIdProps } from '../AriaComponents'

/** Props for a {@link MarkdownViewer}. */
export interface MarkdownViewerProps extends TestIdProps {
  /** Markdown markup to parse and display. */
  readonly text: string
  readonly imgUrlResolver: (relativePath: string) => Promise<string>
}

const LazyMarkdownEditor = React.lazy(() =>
  import('#/components/MarkdownViewer/defaultRenderer').then(
    // eslint-disable-next-line @typescript-eslint/naming-convention
    ({ MarkdownEditor }) => MarkdownEditor,
  ),
)

/**
 * Markdown viewer component.
 * Parses markdown passed in as a `text` prop into HTML and displays it.
 */
export function MarkdownViewer(props: MarkdownViewerProps) {
  const { text, imgUrlResolver, testId } = props

  const logger = useLogger()
  const { getText } = useText()

  const transformImageUrl: UrlTransformer = (path: string) =>
    /^https?:/.test(path) ?
      Promise.resolve(Ok({ url: path }))
    : imgUrlResolver(path).then(
        (url) => Ok({ url }),
        (error) => {
          logger.error(error)
          return Err(getText('arbitraryFetchImageError'))
        },
      )

  return (
    <LazyMarkdownEditor
      content={text}
      transformImageUrl={transformImageUrl}
      toolbar={false}
      data-testid={testId}
      contentTestId="cmContent"
    />
  )
}
