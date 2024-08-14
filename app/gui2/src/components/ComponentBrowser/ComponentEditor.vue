<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { useEvent } from '@/composables/events'
import type { useNavigator } from '@/composables/navigator'
import type { Icon } from '@/util/iconName'
import { computed, ref, watch, type DeepReadonly } from 'vue'

type Range = { start: number; end: number }

const content = defineModel<DeepReadonly<{ text: string; selection: Range | undefined }>>({
  required: true,
})
const props = defineProps<{
  navigator: ReturnType<typeof useNavigator>
  icon: Icon | undefined
  nodeColor: string
}>()

const inputField = ref<HTMLInputElement>()
const fieldText = ref<string>('')
const fieldSelection = ref<Range>()

watch(content, ({ text: newText, selection: newPos }) => {
  fieldText.value = newText
  if (inputField.value == null) return
  inputField.value.value = newText
  // If boundaries didn't change, don't overwrite selection dir.
  if (
    inputField.value.selectionStart !== newPos?.start ||
    inputField.value.selectionEnd !== newPos?.end
  )
    inputField.value.setSelectionRange(newPos?.start ?? null, newPos?.end ?? null)
})

watch(fieldText, readInputFieldSelection)

watch([fieldText, fieldSelection], ([newText, newSelection]) => {
  content.value = {
    text: newText,
    selection: newSelection,
  }
})

function readInputFieldSelection() {
  if (
    inputField.value != null &&
    inputField.value.selectionStart != null &&
    inputField.value.selectionEnd != null
  ) {
    fieldSelection.value = {
      start: inputField.value.selectionStart,
      end: inputField.value.selectionEnd,
    }
  } else {
    fieldSelection.value = undefined
  }
}

// HTMLInputElement's same event is not supported in chrome yet. We just react for any
// selectionchange in the document and check if the input selection changed.
// BUT some operations like deleting does not emit 'selectionChange':
// https://bugs.chromium.org/p/chromium/issues/detail?id=725890
// Therefore we must also refresh selection after changing input.
useEvent(document, 'selectionchange', readInputFieldSelection)

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
    <input
      ref="inputField"
      v-model="fieldText"
      autocomplete="off"
      class="inputField"
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
  width: 100%;
  height: 40px;
  display: flex;
  flex-direction: row;
  align-items: center;
}

.inputField {
  border: none;
  outline: none;
  min-width: 0;
  flex-grow: 1;
  background: none;
  font: inherit;
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
  flex-shrink: 0;
  flex-grow: 0;
  gap: 8px;
}
</style>
