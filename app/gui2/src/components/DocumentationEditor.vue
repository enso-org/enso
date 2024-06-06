<script setup lang="ts">
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { fetcherUrlTransformer } from '@/components/MarkdownEditor/imageUrlTransformer'
import { useProjectStore } from '@/stores/project'
import type { Path, Uuid } from 'shared/languageServerTypes'
import { Err, Ok, mapOk, withContext, type Result } from 'shared/util/data/result'

const documentation = defineModel<string>({ required: true })

const projectStore = useProjectStore()
const { transformImageUrl } = useDocumentationImages(
  projectStore.projectRootId,
  projectStore.readFileBinary,
)

function useDocumentationImages(
  projectRootId: Promise<Uuid | undefined>,
  readFileBinary: (path: Path) => Promise<Result<Blob>>,
) {
  async function urlToPath(url: string): Promise<Result<Path> | undefined> {
    if (url.includes('//')) {
      // Not a relative URL, custom fetching not needed.
      return undefined
    } else {
      return relativeUrlToPath(url)
    }
  }

  async function relativeUrlToPath(url: string): Promise<Result<Path>> {
    const rootId = await projectRootId
    if (!rootId) {
      return Err('Cannot find project root.')
    }
    if (url.startsWith('/')) {
      // Relative to project root.
      return Ok({ rootId, segments: url.slice(1).split('/') })
    } else {
      // Relative to current module.
      const segments = url.split('/')
      if (segments[0] === '..') {
        return Ok({ rootId, segments: segments.slice(1) })
      } else {
        return Ok({ rootId, segments: ['src', ...segments] })
      }
    }
  }

  function pathUniqueId(path: Path) {
    return path.rootId + ':' + path.segments.join('/')
  }

  function pathDebugRepr(path: Path) {
    return pathUniqueId(path)
  }

  const transformImageUrl = fetcherUrlTransformer(
    async (url: string) => {
      const path = await urlToPath(url)
      if (!path) return
      return withContext(
        () => `Locating documentation image (${url})`,
        () => mapOk(path, (path) => ({ location: path, uniqueId: pathUniqueId(path) })),
      )
    },
    async (path) => {
      return withContext(
        () => `Loading documentation image (${pathDebugRepr(path)})`,
        async () => await readFileBinary(path),
      )
    },
  )

  return { transformImageUrl }
}
</script>

<template>
  <MarkdownEditor v-model="documentation" :transformImageUrl="transformImageUrl" />
</template>
