<script lang="ts">
export const name = '<name here>'
export const inputType = '<allowed input type(s) here>'

interface Data {
  dataType: 'here'
}
</script>

<script setup lang="ts">
import { onMounted } from 'vue'

import { VisualizationContainer } from 'builtins'
// Optional: add your own external dependencies.
// import dependency from 'https://<js dependency here>'
//
// When using typescript, you can also specify typings for your dependencies by locally declaring
// the module in a `.d.ts` file in the same directory as this file:
// ```ts
// module 'https://<js dependency here>' {
//   export * from '<locally installed typings here>'
// }
// ```

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
    {{ props.data }}
  </VisualizationContainer>
</template>

<style scoped>
/* Optional */
@import url('<style>');
@import url('<dependencies>');
@import url('<here>');
</style>
