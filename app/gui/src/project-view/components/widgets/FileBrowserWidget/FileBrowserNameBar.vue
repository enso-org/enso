<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'

const filenameInputContents = defineModel<string>({ required: true })

const emit = defineEmits<{
  accept: []
}>()
</script>

<template>
  <div class="FileBrowserNameBar">
    <input
      v-model="filenameInputContents"
      class="fileNameInput"
      @pointerdown.stop
      @click.stop
      @contextmenu.stop
      @keydown.backspace.stop
      @keydown.delete.stop
      @keydown.arrow-left.stop
      @keydown.arrow-right.stop
      @keydown.enter.stop="emit('accept')"
    />
    <SvgButton
      class="FileBrowserButton"
      label="Ok"
      :disabled="!filenameInputContents"
      @click.stop="emit('accept')"
    />
  </div>
</template>

<style scoped>
.FileBrowserNameBar {
  width: 100%;
  display: flex;
  flex-direction: row;
  padding: var(--border-width) 0 0 0;
  gap: var(--border-width);
}

.fileNameInput {
  border-radius: var(--border-radius-inner);
  height: calc(var(--border-radius-inner) * 2);
  padding: 0 8px;
  background-color: var(--color-frame-selected-bg);
  flex-grow: 1;
  appearance: textfield;
  -moz-appearance: textfield;
  user-select: all;
}
</style>
