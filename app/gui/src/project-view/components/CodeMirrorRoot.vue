<script setup lang="ts">
import { ref } from 'vue'

const rootElement = ref<HTMLElement>()
defineExpose({ rootElement })
</script>

<template>
  <div ref="rootElement" class="CodeMirrorRoot" @wheel.stop.passive @contextmenu.stop>
    <slot />
  </div>
</template>

<!--suppress CssUnusedSymbol-->
<style scoped>
.CodeMirrorRoot {
  display: contents;
}

:deep(.cm-scroller) {
  /* Prevent touchpad back gesture, which can be triggered while panning. */
  overscroll-behavior: none;
  /* By default it uses `monospace`, but even when we want the editor to be monospace we use more specific fonts. */
  font-family: unset;
}

:deep(.cm-editor) {
  position: relative;
  contain: content;
  height: 100%;
  width: 100%;
  outline: none;
}
</style>
