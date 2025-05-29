<script setup lang="ts">
import {
  provideDocumentationImageUrlTransformer,
  type UrlTransformer,
} from '@/components/MarkdownEditor/imageUrlTransformer'
import { Vec2 } from '@/util/data/vec2'
import { ComponentInstance, computed, defineAsyncComponent, ref, toRef } from 'vue'
import * as Y from 'yjs'

const props = withDefaults(
  defineProps<{
    content: Y.Text | string
    // eslint-disable-next-line vue/require-default-prop
    transformImageUrl?: UrlTransformer | undefined
    toolbar?: boolean
    // eslint-disable-next-line vue/require-default-prop
    contentTestId?: string | undefined
  }>(),
  { toolbar: true },
)
defineOptions({
  inheritAttrs: false,
})

const inner = ref<ComponentInstance<typeof LazyMarkdownEditor>>()

const LazyMarkdownEditor = defineAsyncComponent(
  () => import('@/components/MarkdownEditor/MarkdownEditorImpl.vue'),
)

provideDocumentationImageUrlTransformer(toRef(props, 'transformImageUrl'))

defineExpose({
  loaded: computed(() => inner.value != null),
  putText: (text: string) => inner.value?.putText(text),
  putTextAtCoord: (text: string, coords: Vec2) => inner.value?.putTextAtCoords(text, coords),
  bold: () => inner.value?.bold(),
  italic: () => inner.value?.italic(),
  header1: () => inner.value?.header1(),
  header2: () => inner.value?.header2(),
  header3: () => inner.value?.header3(),
  paragraph: () => inner.value?.paragraph(),
  link: () => inner.value?.link(),
})
</script>

<template>
  <Suspense>
    <LazyMarkdownEditor
      ref="inner"
      v-bind="$attrs"
      class="flex-1"
      :content="props.content"
      :toolbar="props.toolbar"
      :contentTestId="props.contentTestId"
    >
      <template #toolbarLeft>
        <slot name="toolbarLeft" />
      </template>
      <template #toolbarRight>
        <slot name="toolbarRight" />
      </template>
      <template #belowToolbar>
        <slot name="belowToolbar" />
      </template>
    </LazyMarkdownEditor>
  </Suspense>
</template>
