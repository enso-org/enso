import type { UrlTransformer } from '@/components/MarkdownEditor/imageFiles/imageUrlTransformer'
import type { EditorView } from '@codemirror/view'
import { Err, Ok } from 'ydoc-shared/util/data/result'

export interface DocumentationImages {
  transformImageUrl: UrlTransformer
  tryUploadDroppedImage: (view: EditorView, event: DragEvent) => boolean
  tryUploadPastedImage: (view: EditorView, item: ClipboardItem) => boolean
  tryUploadImageFile: (view: EditorView) => Promise<void>
}

/**
 * Based of given location of the module, return the location of image based on imageUrl.
 * @returns path relative to project's root, or the full URL with schema.
 */
export function resolveDocImageUrl(modulePathSegments: string[], imageUrl: string) {
  const appliedUrl = URL.parse(imageUrl, `file:///${modulePathSegments.join('/')}}`)
  switch (appliedUrl?.protocol) {
    case null:
      return Err('Invalid image url')
    case 'file:':
      // Omit the starting '/'
      return Ok({ type: 'projectPath' as const, path: decodeURI(appliedUrl.pathname).substring(1) })
    case 'http:':
    case 'https:':
      return Ok({ type: 'url' as const, url: appliedUrl })
    default:
      return Err('Unsupported protocol')
  }
}
