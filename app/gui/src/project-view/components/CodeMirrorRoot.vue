<script setup lang="ts">
import { ref } from 'vue'

const rootElement = ref<HTMLElement>()
defineExpose({ rootElement })
</script>

<template>
  <div
    ref="rootElement"
    class="CodeMirror"
    @keydown.arrow-left.stop
    @keydown.arrow-right.stop
    @keydown.arrow-up.stop
    @keydown.arrow-down.stop
    @keydown.enter.stop
    @keydown.backspace.stop
    @keydown.delete.stop
    @wheel.stop.passive
    @contextmenu.stop
  ></div>
</template>

<style scoped>
.CodeMirror {
  width: 100%;
  height: 100%;
}

:deep(.cm-scroller) {
  /* Prevent touchpad back gesture, which can be triggered while panning. */
  overscroll-behavior: none;
}

:deep(.cm-editor) {
  position: relative;
  width: 100%;
  height: 100%;
  outline: none;
}
</style>
