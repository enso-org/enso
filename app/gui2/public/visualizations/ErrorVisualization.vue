<script lang="ts">
export const name = 'Error'
export const inputType = 'Any'

interface Data {
  kind?: ErrorKind
  message: string
}

enum ErrorKind {
  panic = 'Panic',
  dataflow = 'Dataflow',
  warning = 'Warning',
}
</script>

<script setup lang="ts">
import VisualizationContainer from 'builtins/VisualizationContainer.vue'

import { computed, onMounted } from 'vue'

const props = defineProps<{ data: Data | string }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Preprocessor', 'error_preprocessor')
})

const CLASSES: Record<ErrorKind, string> = {
  [ErrorKind.dataflow]: 'error-dataflow',
  [ErrorKind.panic]: 'error-panic',
  [ErrorKind.warning]: 'error-warning',
}

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <VisualizationContainer :="<any>$attrs" :below-toolbar="true">
    <div
      class="ErrorVisualization"
      :class="data.kind != null ? CLASSES[data.kind] : ''"
      v-text="data.message"
    ></div>
  </VisualizationContainer>
</template>

<style scoped>
.ErrorVisualization {
  white-space: pre;
  padding: 8px;
}

.error-dataflow {
  color: var(--color-error-dataflow);
}

.error-panic {
  color: var(--color-error-panic);
}

.error-warning {
  color: var(--color-error-warning);
}
</style>
