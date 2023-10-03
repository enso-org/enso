<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import GraphEditor from '@/components/GraphEditor.vue'
</script>

<template>
  <Story title="Editor" group="graph" responsive-disabled>
    <GraphEditor />
  </Story>
</template>

<style>
:root {
  --color-text: var(--histoire-contrast-color);
}

.viewport {
  height: 100vh;
  width: 100vw;
}
</style>
