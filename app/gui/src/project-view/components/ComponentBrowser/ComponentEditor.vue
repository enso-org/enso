<script setup lang="ts">
import type { ComponentBrowserMode, Usage } from '@/components/ComponentBrowser/input'
import SvgIcon from '@/components/SvgIcon.vue'
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { useGraphStore } from '@/stores/graph'
import { DEFAULT_ICON, iconOfNode, suggestionEntryToIcon } from '@/util/getIconName'
import { qnLastSegment } from '@/util/qualifiedName'
import { computed, ref, watch, type DeepReadonly } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'
import { type Range } from 'ydoc-shared/util/data/range'

const content = defineModel<DeepReadonly<{ text: string; selection: Range | undefined }>>({
  required: true,
})
const props = defineProps<{
  usage: Usage
  mode: ComponentBrowserMode
  nodeColor: string
}>()

const inputField = ref<ComponentExposed<typeof AutoSizedInput>>()

const fieldContent = ref<{ text: string; selection: Range | undefined }>({
  text: '',
  selection: undefined,
})

watch(content, (newContent) => {
  fieldContent.value = newContent
})
watch(
  [() => fieldContent.value.text, () => fieldContent.value.selection],
  ([newText, newSelection]) => {
    content.value = {
      text: newText,
      selection: newSelection,
    }
  },
)

const graphStore = useGraphStore()

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

const selfTypeName = computed(() => {
  if (
    props.mode.mode === 'componentBrowsing' &&
    props.mode.filter.selfArg?.type === 'known' &&
    props.mode.filter.selfArg.typename.path
  ) {
    return qnLastSegment(props.mode.filter.selfArg.typename.path)
  }
  return undefined
})

defineExpose({
  blur: () => inputField.value?.blur(),
  focus: () => inputField.value?.focus(),
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
    <span v-if="selfTypeName != null" class="selfArgInfo">{{ selfTypeName }} Components</span>
    <SvgIcon v-if="selfTypeName != null" class="selfArgInfoArrow" name="folder_closed" />
    <AutoSizedInput
      ref="inputField"
      v-model="fieldContent.text"
      v-model:selection="fieldContent.selection"
      autocomplete="off"
      class="inputField"
      :acceptOnEnter="false"
      @pointerdown.stop
      @pointerup.stop
      @click.stop
    />
  </div>
</template>

<style scoped>
.ComponentEditor {
  --port-padding: 6px;
  --icon-size: 16px;
  border-radius: 22px;
  background-color: var(--background-color);
  padding: 0 var(--component-editor-padding);
  height: 44px;
  display: flex;
  flex-direction: row;
  gap: 8px;
  align-items: center;
}

.inputField {
  border: none;
  outline: none;
  background: none;
  font: inherit;
  text-align: left;
  flex-grow: 1;
}

.componentEditorIcon {
  position: relative;
  text-align: center;
  border-radius: var(--radius-full);
  padding: var(--port-padding);
  margin: 0 0 0 calc(0px - var(--port-padding));
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
