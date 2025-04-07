<script setup lang="ts">
import CodeMirrorInlineRoot from '@/components/CodeMirrorInlineRoot.vue'
import ComponentEditorLabel from '@/components/ComponentBrowser/ComponentEditorLabel.vue'
import type { ComponentBrowserMode, Usage } from '@/components/ComponentBrowser/input'
import SvgIcon from '@/components/SvgIcon.vue'
import { useGraphStore } from '@/stores/graph'
import { useCodeMirror, useStringSync } from '@/util/codemirror'
import { DEFAULT_ICON, iconOfNode, suggestionEntryToIcon } from '@/util/getIconName'
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

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorInlineRoot>>('editorRoot')

const { syncExt, connectSync } = useStringSync()
const { editorView } = useCodeMirror(editorRoot, {
  extensions: [syncExt],
  contentTestId: 'component-editor-content',
  singleLine: true,
})

const { onUserAction } = connectSync(editorView)
onUserAction(
  (text, selection) =>
    (content.value = {
      text,
      selection: Range.unsafeFromBounds(selection.from, selection.to),
    }),
)
watch(content, ({ text, selection }) =>
  editorView.dispatch({
    changes: { from: 0, to: editorView.state.doc.length, insert: text },
    selection: selection ? { anchor: selection.from, head: selection.to } : { anchor: 0 },
  }),
)

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
    <ComponentEditorLabel
      v-if="props.mode.mode === 'componentBrowsing'"
      :selfArg="props.mode.filter.selfArg"
    />
    <SvgIcon
      v-if="props.mode.mode === 'componentBrowsing'"
      class="selfArgInfoArrow"
      name="folder_closed"
    />
    <CodeMirrorInlineRoot ref="editorRoot" />
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
