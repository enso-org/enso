<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import GraphEditor from '@/components/GraphEditor.vue'
</script>

<template>
  <Story title="Editor" group="graph" :layout="{ type: 'grid', width: 250 }">
    <GraphEditor />
  </Story>
</template>
