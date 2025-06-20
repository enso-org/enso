import type { OpenedProject } from '$/providers/openedProjects'
import { useDocumentationImagesFromBackend } from '@/components/MarkdownEditor/imageFiles/backendFiles'
import type { DocumentationImages } from '@/components/MarkdownEditor/imageFiles/common'
import { useDocumentationImagesFromProjectFiles } from '@/components/MarkdownEditor/imageFiles/projectFiles'
import { createContextStore } from '@/providers'
import { useProjectFiles } from '@/stores/projectFiles'
import type { Opt } from '@/util/data/opt'
import type { ToValue } from '@/util/reactivity'
import { identity } from '@vueuse/core'
import type Backend from 'enso-common/src/services/Backend'
import type { ProjectId } from 'enso-common/src/services/Backend'
import { effectScope, type Ref, ref, toRef, watch, type WatchSource } from 'vue'

interface DocumentationImagesOptions {
  openedProject: WatchSource<Opt<OpenedProject>>
  backend: ToValue<Opt<Backend>>
  projectId: ToValue<Opt<ProjectId>>
}

const [provide, useDocumentationImages] = createContextStore(
  'DocumentationImages',
  identity<Readonly<Ref<DocumentationImages>>>,
)

export { useDocumentationImages }

/** Provides support for loading and uploading project images. */
export function provideDocumentationImages({
  openedProject,
  backend,
  projectId,
}: DocumentationImagesOptions) {
  const handlers = ref<DocumentationImages>()

  watch(
    openedProject,
    (openedProject, _, onCleanup) => {
      const scope = effectScope()
      scope.run(() => {
        if (openedProject != null) {
          const { store, graph } = openedProject
          handlers.value = useDocumentationImagesFromProjectFiles(
            toRef(graph, 'modulePath'),
            useProjectFiles(store),
          )
        } else {
          handlers.value = useDocumentationImagesFromBackend({ backend, projectId })
        }
      })
      onCleanup(() => scope.stop())
    },
    { immediate: true },
  )

  provide(handlers as Ref<DocumentationImages>)
}
