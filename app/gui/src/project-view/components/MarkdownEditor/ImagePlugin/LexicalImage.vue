<script setup lang="ts">
import {
  injectLexicalImageUrlTransformer,
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

const urlTransformer = injectLexicalImageUrlTransformer(true)

// NOTE: Garbage-collecting image data when the `src` changes is not implemented. Current users of `LexicalImage` don't
// change the `src` after creating an image.
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

onUnmounted(() => {
  if (data.value?.ok) data.value.value.dispose?.()
})
</script>

<template>
  <img :src="data?.ok ? data.value.url : ''" :alt="alt" :title="title" />
</template>
