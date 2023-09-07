<script lang="ts">
export const name = '<name here>'
export const inputType = '<allowed input type(s) here>'

// Optional: Move the type `Data` here if it errors when placed in the `script setup` block.
</script>

<script setup lang="ts">
import { computed, onMounted } from 'vue'
import VisualizationContainer from './VisualizationContainer.vue'

const props = defineProps<{
  data: Data | string
}>()
const emit = defineEmits<{
  // Optional:
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

// Optional:
onMounted(() => {
  emit('update:preprocessor', '<module path here>', '<method name here>', '<optional args here>')
})

type Data = { dataType: 'here' }

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <VisualizationContainer :="<any>$attrs">
    <!-- <content here> -->
  </VisualizationContainer>
</template>

<style scoped></style>
