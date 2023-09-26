<script lang="ts">
export const name = '<name here>'
export const inputType = '<allowed input type(s) here>'

interface Data {
  dataType: 'here'
}
</script>

<script setup lang="ts">
import { onMounted } from 'vue'

import VisualizationContainer from 'builtins/VisualizationContainer.vue'
// Optional: add your own external dependencies. The @ts-expect-error is required because TypeScript
// does not allow HTTP imports.
// @ts-expect-error
import dependency from 'http://<js dependency here>'

const props = defineProps<{
  data: Data
}>()
const emit = defineEmits<{
  // Optional:
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

// Optional:
onMounted(() => {
  emit('update:preprocessor', '<module path here>', '<method name here>', '<optional args here>')
})
</script>

<template>
  <VisualizationContainer>
    <!-- <content here> -->
  </VisualizationContainer>
</template>

<style scoped>
/* Optional */
@import url('<style>');
@import url('<dependencies>');
@import url('<here>');
</style>
