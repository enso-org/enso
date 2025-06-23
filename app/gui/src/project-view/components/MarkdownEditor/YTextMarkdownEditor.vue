<script setup lang="ts">
import { useYTextSync } from '@/util/codemirror'
import { EditorView } from '@codemirror/view'
import { defineAsyncComponent } from 'vue'
import * as Y from 'yjs'

const props = defineProps<{
  content: Y.Text | undefined
  toolbar?: boolean
  readonly?: boolean
  contentTestId?: string
}>()

defineOptions({
  inheritAttrs: false,
})

const syncExt = (view: EditorView) => {
  const { syncExt, connectSync } = useYTextSync(() => props.content)
  connectSync(view)
  return syncExt
}

const LazyMarkdownEditor = defineAsyncComponent(
  () => import('@/components/MarkdownEditor/MarkdownEditorImpl.vue'),
)
</script>

<template>
  <Suspense>
    <LazyMarkdownEditor
      v-bind="$attrs"
      :extensions="syncExt"
      :toolbar="toolbar"
      :readonly="readonly"
      :contentTestId="contentTestId"
      class="flex-1"
    >
      <template #belowToolbar>
        <slot name="belowToolbar" />
      </template>
    </LazyMarkdownEditor>
  </Suspense>
</template>
