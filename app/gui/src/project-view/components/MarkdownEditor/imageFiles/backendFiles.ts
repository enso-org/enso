import {
  resolveDocImageUrl,
  type DocumentationImages,
} from '@/components/MarkdownEditor/imageFiles/common'
import type { Opt } from '@/util/data/opt'
import type { ToValue } from '@/util/reactivity'
import type Backend from 'enso-common/src/services/Backend'
import type { ProjectId } from 'enso-common/src/services/Backend'
import { toValue } from 'vue'
import { Err, Ok } from 'ydoc-shared/util/data/result'

interface DocumentationImagesOptions {
  backend: ToValue<Opt<Backend>>
  projectId: ToValue<Opt<ProjectId>>
}

/** @returns Documentation image support based on the {@link Backend} API. */
export function useDocumentationImagesFromBackend(
  options: DocumentationImagesOptions,
): DocumentationImages {
  return {
    transformImageUrl: (path: string) => {
      const backend = toValue(options.backend)
      const projectId = toValue(options.projectId)
      if (backend == null) return Promise.resolve(Err('No backend available'))
      if (projectId == null) return Promise.resolve(Err('No project selected'))
      // In Enso Documentation, the relative paths are from module's directory
      // Here we always display docs from `src/Main.enso` module
      const resolvedUrl = resolveDocImageUrl(['src'], path)
      if (!resolvedUrl.ok) return Promise.resolve(resolvedUrl)
      if (resolvedUrl.value.type === 'url') {
        return Promise.resolve(Ok({ url: resolvedUrl.value.url.toString() }))
      } else {
        return backend.resolveProjectAssetPath(projectId, resolvedUrl.value.path).then(
          (url) => Ok({ url }),
          (error) => {
            console.error(error)
            return Err(error)
          },
        )
      }
    },
    tryUploadImageFile: async () => {},
    tryUploadDroppedImage: () => false,
    tryUploadPastedImage: () => {
      return false
    },
  }
}
