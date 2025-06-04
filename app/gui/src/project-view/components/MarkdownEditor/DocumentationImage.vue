<script setup lang="ts">
import {
  injectDocumentationImageUrlTransformer,
  type TransformUrlResult,
} from '@/components/MarkdownEditor/imageUrlTransformer'
import { computedAsync } from '@vueuse/core'
import { computed, onUnmounted, type Ref } from 'vue'
import { Ok } from 'ydoc-shared/util/data/result'

const DEFAULT_ALT_TEXT = 'Image'

const props = defineProps<{
  src: string
  alt: string
}>()

const urlTransformer = injectDocumentationImageUrlTransformer(true)

// NOTE: Garbage-collecting image data when the `src` changes is not implemented. Current users of `DocumentationImage`
// don't change the `src` after creating an image.
const data: Ref<TransformUrlResult | undefined> =
  urlTransformer ?
    computedAsync(() => urlTransformer.transformUrl(props.src), undefined, {
      onError: console.error,
    })
  : computed(() => Ok({ url: props.src }))

const title = computed(() =>
  data.value == null ? 'Loading'
  : !data.value.ok ? data.value.error.message()
  : props.alt !== DEFAULT_ALT_TEXT ? props.alt
  : '',
)

const alt = props.alt ? props.alt : DEFAULT_ALT_TEXT

const isYouTubeVideo = computed(() =>
  props.src.match(/https:\/\/www\.youtube(-nocookie)?\.com\/embed\/[^/]+/),
)

onUnmounted(() => {
  if (data.value?.ok) data.value.value.dispose?.()
})
</script>

<template>
  <div v-if="isYouTubeVideo" class="youtube-video-container">
    <div>
      <iframe
        :src="data?.ok ? data.value.url : ''"
        :title="title"
        allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
        referrerpolicy="strict-origin-when-cross-origin"
        allowfullscreen
      >
      </iframe>
    </div>
  </div>
  <img
    v-else
    :src="data?.ok ? data.value.url : ''"
    :alt="alt"
    :title="title"
    :class="{ uploading: data?.ok && data.value.uploading?.value }"
  />
</template>
<style scoped>
.youtube-video-container {
  width: 100%;
  min-width: 400px;
  max-width: 800px;
}

.youtube-video-container div {
  position: relative;
  width: 100%;
  overflow: hidden;
  padding-top: 56.25%;
}

.youtube-video-container div iframe {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
}
</style>
