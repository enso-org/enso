<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import AutoSizedInput, { type Range } from '@/components/widgets/AutoSizedInput.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed, ref, watch, type DeepReadonly } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'

const content = defineModel<DeepReadonly<{ text: string; selection: Range | undefined }>>({
  required: true,
})
const props = defineProps<{
  icon: Icon | undefined
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
    <div v-if="props.icon" class="iconPort">
      <SvgIcon :name="props.icon" class="nodeIcon" />
    </div>
    <span 
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
  --icon-height: 16px;
  --icon-text-gap: 6px;
  border-radius: 50%;
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

.iconPort {
  position: relative;
  text-align: center;
  border-radius: var(--radius-full);
  padding: var(--port-padding);
  margin: 0 var(--icon-text-gap) 0 calc(0px - var(--port-padding));
  background-color: var(--color-node-port);
  isolation: isolate;
}

.nodeIcon {
  color: white;
  width: var(--icon-height);
  height: var(--icon-height);
}

.buttonPanel {
  display: flex;
  flex-direction: row;
  gap: 8px;
  flex-grow: 0;
}
</style>
