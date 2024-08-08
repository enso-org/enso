<script setup lang="ts">
import { ref } from 'vue'

import TopBar from '@/components/TopBar.vue'

const showColorPicker = ref(false)
const showCodeEditor = ref(false)
const showDocumentationEditor = ref(false)
</script>

<template>
  <Story title="Top Bar" group="graph" :layout="{ type: 'grid', width: 500 }" autoPropsDisabled>
    <div style="height: 48px">
      <TopBar
        v-model:showColorPicker="showColorPicker"
        v-model:showCodeEditor="showCodeEditor"
        v-model:showDocumentationEditor="showDocumentationEditor"
        :zoomLevel="1"
        :componentsSelected="1"
      />
    </div>
    <template #controls></template>
  </Story>
</template>
