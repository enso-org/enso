<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { useTemplateRef, watchEffect } from 'vue'

defineProps<{ containerStyle?: Record<string, string> }>()
const emit = defineEmits<{ 'update:toolbarElement': [toolbarElement: HTMLElement] }>()

const fullscreen = defineModel<boolean>('fullscreen', { required: true })

const toolbarElement = useTemplateRef('toolbarElement')

watchEffect(() => {
  if (toolbarElement.value != null) {
    emit('update:toolbarElement', toolbarElement.value)
  }
})
</script>

<template>
  <WithFullscreenMode v-model="fullscreen">
    <div :style="containerStyle" class="contentInner">
      <div class="toolbar">
        <div ref="toolbarElement" class="componentToolbar" />
        <ActionButton class="toolbarButton" action="panel.fullscreen" />
        <ActionButton v-if="!fullscreen" class="toolbarButton" action="panel.close" />
      </div>
      <slot :toolbarElement="toolbarElement" />
    </div>
  </WithFullscreenMode>
</template>

<style scoped>
.contentInner {
  width: 100%;
  height: 100%;
  padding: 1rem;
  display: flex;
  flex-direction: column;
  gap: 1rem;
  background-color: var(--color-dashboard-background);
}

.toolbar {
  display: flex;
  flex-direction: row;
  align-items: start;
  justify-content: end;
  gap: 8px;
}

.toolbarButton {
  margin-top: 4px;
  margin-bottom: 4px;
}

.componentToolbar {
  flex-grow: 1;
}
</style>
