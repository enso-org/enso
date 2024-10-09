<script setup lang="ts">
import FullscreenButton from '@/components/FullscreenButton.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { fetcherUrlTransformer } from '@/components/MarkdownEditor/imageUrlTransformer'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { ToValue } from '@/util/reactivity'
import { ref, toRef, toValue, watch } from 'vue'
import type { Path } from 'ydoc-shared/languageServerTypes'
import { Err, Ok, mapOk, withContext, type Result } from 'ydoc-shared/util/data/result'

const documentation = defineModel<string>({ required: true })
const emit = defineEmits<{
  'update:fullscreen': [boolean]
}>()

const toolbarElement = ref<HTMLElement>()

const graphStore = useGraphStore()
const projectStore = useProjectStore()
const { transformImageUrl } = useDocumentationImages(
  toRef(graphStore, 'modulePath'),
  projectStore.readFileBinary,
)

function useDocumentationImages(
  modulePath: ToValue<Path | undefined>,
  readFileBinary: (path: Path) => Promise<Result<Blob>>,
) {
  async function urlToPath(url: string): Promise<Result<Path> | undefined> {
    const modulePathValue = toValue(modulePath)
    if (!modulePathValue) {
      return Err('Current module path is unknown.')
    }
    const appliedUrl = new URL(url, `file:///${modulePathValue.segments.join('/')}`)
    if (appliedUrl.protocol === 'file:') {
      const segments = appliedUrl.pathname.split('/')
      return Ok({ rootId: modulePathValue.rootId, segments })
    } else {
      // Not a relative URL, custom fetching not needed.
      return undefined
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

const fullscreen = ref(false)
const fullscreenAnimating = ref(false)

watch(
  () => fullscreen.value || fullscreenAnimating.value,
  (fullscreenOrAnimating) => emit('update:fullscreen', fullscreenOrAnimating),
)
</script>

<template>
  <WithFullscreenMode :fullscreen="fullscreen" @update:animating="fullscreenAnimating = $event">
    <div class="DocumentationEditor">
      <div ref="toolbarElement" class="toolbar">
        <FullscreenButton v-model="fullscreen" />
      </div>
      <div class="scrollArea">
        <MarkdownEditor
          v-model="documentation"
          :transformImageUrl="transformImageUrl"
          :toolbarContainer="toolbarElement"
        />
      </div>
    </div>
  </WithFullscreenMode>
</template>

<style scoped>
.DocumentationEditor {
  display: flex;
  flex-direction: column;
  background-color: #fff;
  height: 100%;
  width: 100%;
}

.scrollArea {
  width: 100%;
  overflow-y: auto;
  padding-left: 10px;
  /* Prevent touchpad back gesture, which can be triggered while panning. */
  overscroll-behavior-x: none;
  flex-grow: 1;
}

.toolbar {
  height: 48px;
  padding-left: 16px;
  flex-shrink: 0;

  display: flex;
  align-items: center;
  flex-direction: row;
  gap: 8px;
}
</style>
