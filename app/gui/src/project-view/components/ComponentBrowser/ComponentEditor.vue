<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import type { ComponentBrowserMode, Usage } from '@/components/ComponentBrowser/input'
import SvgIcon from '@/components/SvgIcon.vue'
import { useGraphStore } from '@/stores/graph'
import { useCodeMirror, useStringSync } from '@/util/codemirror'
import { DEFAULT_ICON, iconOfNode, suggestionEntryToIcon } from '@/util/getIconName'
import { qnLastSegment } from '@/util/qualifiedName'
import { computed, useTemplateRef, watch, type ComponentInstance, type DeepReadonly } from 'vue'
import { Range } from 'ydoc-shared/util/data/range'

const content = defineModel<DeepReadonly<{ text: string; selection: Range | undefined }>>({
  required: true,
})
const props = defineProps<{
  usage: Usage
  mode: ComponentBrowserMode
  nodeColor: string
}>()

const graphStore = useGraphStore()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')

const { syncExt, connectSync } = useStringSync()
const { editorView } = useCodeMirror(editorRoot, {
  extensions: [syncExt],
  contentTestId: 'component-editor-content',
  lineMode: 'single',
})

const { onUserAction, setText } = connectSync(editorView)
onUserAction(
  (text, selection) =>
    (content.value = {
      text,
      selection: Range.unsafeFromBounds(selection.from, selection.to),
    }),
)
watch(content, ({ text, selection }) => setText(text, selection))

const icon = computed(() => {
  if (props.mode.mode === 'componentBrowsing') return 'find'
  if (props.usage.type === 'editNode') {
    return iconOfNode(props.usage.node, graphStore.db)
  }
  if (props.mode.mode === 'codeEditing' && props.mode.appliedSuggestion) {
    return suggestionEntryToIcon(props.mode.appliedSuggestion)
  }
  return DEFAULT_ICON
})

const label = computed(() => {
  if (props.mode.mode !== 'componentBrowsing') return undefined
  if (props.mode.filter.selfArg == null) return 'Input Components'
  if (props.mode.filter.selfArg.type === 'known' && props.mode.filter.selfArg.typename.path) {
    return `${qnLastSegment(props.mode.filter.selfArg.typename.path)} Components`
  }

  return undefined
})

const focus = editorView.focus.bind(editorView)

defineExpose({
  blur: editorView.contentDOM.blur.bind(editorView.contentDOM),
  focus,
  /**
   * Focus the editor asynchronously.
   *
   * THe editor cannot be focused until after it is mounted, because it is inserted into the DOM
   * dynamically. This function focuses the editor when it is ready.
   */
  delayedFocus: () => setTimeout(focus),
})

const rootStyle = computed(() => {
  return {
    '--node-group-color': props.nodeColor,
  }
})
</script>

<template>
  <div class="ComponentEditor define-node-colors" :style="rootStyle">
    <div :class="{ componentEditorIcon: true, port: props.mode.mode !== 'componentBrowsing' }">
      <SvgIcon :name="icon" />
    </div>
    <span v-if="label" class="selfArgInfo" data-testid="component-editor-label" v-text="label" />
    <SvgIcon v-if="label" class="selfArgInfoArrow" name="folder_closed" />
    <CodeMirrorRoot ref="editorRoot" />
  </div>
</template>

<style scoped>
.ComponentEditor {
  --port-padding: 4px;
  --icon-size: 16px;
  border-radius: 22px;
  background-color: var(--background-color);
  padding: var(--component-editor-padding);
  display: flex;
  flex-direction: row;
  gap: 8px;
  align-items: center;
}

:deep(.cm-editor) {
  flex-grow: 1;
}

.componentEditorIcon {
  position: relative;
  text-align: center;
  border-radius: var(--radius-full);
  padding: var(--port-padding);
  margin: 0;
  isolation: isolate;
  &.port {
    background-color: var(--color-node-port);
    color: white;
  }
}

.selfArgInfoArrow {
  margin: 0 -4px;
}
</style>
