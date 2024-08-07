<script lang="ts">
import type { IHeaderParams } from 'ag-grid-community'
import { ref, watch } from 'vue'

export interface HeaderParams {
  nameSetter?: (newName: string) => void
  virtualColumn?: boolean
  onHeaderEditingStarted?: (stop: (cancel: boolean) => void) => void
  onHeaderEditingStopped?: () => void
}
</script>

<script setup lang="ts">
const props = defineProps<{
  params: IHeaderParams & HeaderParams
}>()

const editing = ref(false)
const inputElement = ref<HTMLInputElement>()

watch(editing, (newVal, oldVal) => {
  if (newVal && !oldVal) {
    props.params.onHeaderEditingStarted?.((cancel: boolean) => {
      if (cancel) editing.value = false
      else acceptNewName()
    })
  } else {
    props.params.onHeaderEditingStopped?.()
  }
})

watch(inputElement, (newVal, oldVal) => {
  if (newVal != null && oldVal == null) {
    // Whenever input field appears, focus and select text
    newVal.focus()
    newVal.select()
  }
})

function acceptNewName() {
  if (inputElement.value == null) {
    console.error('Tried to accept header new name without input element!')
    return
  }
  console.log('Accepting', inputElement.value.value)
  props.params.nameSetter?.(inputElement.value.value)
  editing.value = false
}
</script>

<template>
  <div class="ag-cell-label-container" role="presentation" @pointerdown.stop @click.stop>
    <div class="ag-header-cell-label" role="presentation">
      <input
        v-if="editing"
        ref="inputElement"
        class="ag-input-field-input ag-text-field-input"
        :value="params.displayName"
        @change="acceptNewName()"
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.arrow-up.stop
        @keydown.arrow-down.stop
      />
      <span
        v-else
        class="ag-header-cell-text"
        :class="{ virtualColumn: params.virtualColumn === true }"
        @click="editing = params.nameSetter != null"
        >{{ params.displayName }}</span
      >
    </div>
  </div>
</template>

<style>
.virtualColumn {
  color: rgba(0, 0, 0, 0.5);
}
</style>
