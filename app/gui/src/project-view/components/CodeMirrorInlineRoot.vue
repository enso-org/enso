<script setup lang="ts">
import { ref } from 'vue'

const rootElement = ref<HTMLElement>()
defineExpose({ rootElement })
</script>

<template>
  <div
    ref="rootElement"
    class="CodeMirrorInlineRoot"
    @contextmenu.stop
    @pointerdown.stop
    @pointerup.stop
    @click.stop
  >
    <slot />
  </div>
</template>

<!--suppress CssUnusedSymbol-->
<style scoped>
.CodeMirrorInlineRoot {
  display: contents;
}

:deep(.cm-scroller) {
  display: contents;
  /* By default it uses `monospace`, but even when we want the editor to be monospace we use more specific fonts. */
  font-family: unset;
}

:deep(.cm-editor) {
  outline: none;
  margin: 0;
  min-width: 1px;
}

:deep(.cm-line) {
  padding: 0;
}
</style>
