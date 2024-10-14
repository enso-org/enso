<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import AutoSizedInput, { type Range } from '@/components/widgets/AutoSizedInput.vue'
import type { useNavigator } from '@/composables/navigator'
import type { Icon } from '@/util/iconName'
import { computed, ref, watch, type DeepReadonly } from 'vue'
import { ComponentExposed } from 'vue-component-type-helpers'

const content = defineModel<DeepReadonly<{ text: string; selection: Range | undefined }>>({
  required: true,
})
const props = defineProps<{
  navigator: ReturnType<typeof useNavigator>
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
    '--node-color-primary': props.nodeColor,
    '--port-edge-width': `${4 * props.navigator.scale}px`,
  }
})
</script>

<template>
  <div class="ComponentEditor" :style="rootStyle">
    <div v-if="props.icon" class="iconPort">
      <SvgIcon :name="props.icon" class="nodeIcon" />
    </div>
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
    <div class="buttonPanel">
      <slot></slot>
    </div>
  </div>
</template>

<style scoped>
.ComponentEditor {
  --node-color-port: color-mix(in oklab, var(--node-color-primary) 85%, white 15%);
  --port-padding: 6px;
  --icon-height: 16px;
  --icon-text-gap: 6px;
  border-radius: var(--radius-default);
  background-color: var(--background-color);
  padding: 0 var(--component-editor-padding);
  height: 40px;
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
  background-color: var(--node-color-port);
  isolation: isolate;
}

.iconPort::before {
  content: '';
  position: absolute;
  top: calc(var(--port-padding) - var(--component-editor-padding));
  width: var(--port-edge-width);
  height: calc(var(--component-editor-padding) - var(--port-padding) + var(--icon-height) / 2);
  transform: translate(-50%, 0);
  background-color: var(--node-color-port);
  z-index: -1;
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
