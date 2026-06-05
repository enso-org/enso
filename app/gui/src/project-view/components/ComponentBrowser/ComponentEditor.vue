<script setup lang="ts">
import { useGraphStore } from '$/components/WithCurrentProject.vue'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import ComponentTypeLabel from '@/components/ComponentBrowser/ComponentTypeLabel.vue'
import type {
  ComponentBrowserInterpretation,
  ComponentBrowserMode,
  Usage,
} from '@/components/ComponentBrowser/input'
import ModeMenu from '@/components/ComponentBrowser/ModeMenu.vue'
import { useCodeMirror, useStringSync } from '@/util/codemirror'
import { DEFAULT_ICON, iconOfNode, suggestionEntryToIcon } from '@/util/getIconName'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed, useTemplateRef, watch, type ComponentInstance, type DeepReadonly } from 'vue'
import { Range } from 'ydoc-shared/util/data/range'

const content = defineModel<DeepReadonly<{ text: string; selection: Range | undefined }>>({
  required: true,
})
const props = defineProps<{
  usage: Usage
  interpretation: ComponentBrowserInterpretation
  selectedMode: ComponentBrowserMode
  modeLocked: boolean
  aiAvailable: boolean
  nodeColor: string
}>()
const emit = defineEmits<{ 'update:selectedMode': [mode: ComponentBrowserMode] }>()

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

const codeEditIcon = computed<Icon>(() => {
  if (props.usage.type === 'editNode') {
    return iconOfNode(props.usage.node, graphStore.db)
  }
  if (props.interpretation.mode === 'codeEditing' && props.interpretation.appliedSuggestion) {
    return suggestionEntryToIcon(props.interpretation.appliedSuggestion)
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
    <ModeMenu
      :selectedMode="props.selectedMode"
      :aiAvailable="props.aiAvailable"
      :modeLocked="props.modeLocked"
      :codeEditIcon="codeEditIcon"
      :asPort="props.interpretation.mode !== 'componentBrowsing'"
      @update:selectedMode="emit('update:selectedMode', $event)"
    />
    <div class="componentEditorContent">
      <CodeMirrorRoot ref="editorRoot" class="componentEditorInput" />
      <div v-if="props.interpretation.mode === 'componentBrowsing'" class="typeLabel">
        <ComponentTypeLabel
          testId="component-editor-label"
          :typeInfo="
            props.interpretation.filter.selfArg?.type === 'known' ?
              props.interpretation.filter.selfArg.typeInfo
            : undefined
          "
          :unknownLabel="props.interpretation.filter.selfArg == null ? 'Input' : undefined"
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
