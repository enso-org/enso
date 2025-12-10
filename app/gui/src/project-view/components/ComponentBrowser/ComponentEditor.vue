<script setup lang="ts">
import { useGraphStore } from '$/components/WithCurrentProject.vue'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import ComponentTypeLabel from '@/components/ComponentBrowser/ComponentTypeLabel.vue'
import type { ComponentBrowserMode, Usage } from '@/components/ComponentBrowser/input'
import SvgIcon from '@/components/SvgIcon.vue'
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

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')

const { syncExt, setText } = useStringSync({
  onUserAction: (text, selection) =>
    (content.value = {
      text,
      selection: Range.unsafeFromBounds(selection.from, selection.to),
    }),
})
const { editorView } = useCodeMirror(editorRoot, {
  extensions: [syncExt],
  contentTestId: 'component-editor-content',
  lineMode: 'single',
})

watch(content, ({ text, selection }) => setText(editorView, text, selection), { immediate: true })

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
    <div class="componentEditorContent">
      <CodeMirrorRoot ref="editorRoot" class="componentEditorInput" />
      <div v-if="props.mode.mode === 'componentBrowsing'" class="typeLabel">
        <ComponentTypeLabel
          testId="component-editor-label"
          :typeInfo="
            props.mode.filter.selfArg?.type === 'known' ?
              props.mode.filter.selfArg.typeInfo
            : undefined
          "
          :unknownLabel="props.mode.filter.selfArg == null ? 'Input' : undefined"
        />
      </div>
    </div>
  </div>
</template>

<style scoped>
.ComponentEditor {
  --port-padding: 4px;
  --icon-size: 16px;
  border-radius: 22px;
  background-color: var(--background-color);
  /*noinspection CssUnresolvedCustomProperty*/
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
    background-color: var(--color-edge-from-node);
    color: white;
  }
}

.componentEditorContent {
  display: flex;
  width: 100%;
  flex-direction: row;
  align-items: center;
}

.componentEditorInput {
  flex-grow: 1;
}

.typeLabel {
  margin: 0 0px;
}
</style>
