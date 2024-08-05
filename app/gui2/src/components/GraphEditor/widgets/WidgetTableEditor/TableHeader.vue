<script lang="ts">
import type { IHeaderParams } from 'ag-grid-community'
import { ref } from 'vue'

export interface HeaderParams {
  nameSetter?: (newName: string) => void
  virtualColumn?: boolean
}
</script>

<script setup lang="ts">
const props = defineProps<{
  params: IHeaderParams & HeaderParams
}>()

const editing = ref(false)

function changeName(newName: string) {
  console.log('Setting new column name', props.params.column.getColId(), newName)
  props.params.nameSetter?.(newName)
  editing.value = false
  props.params.api.stopEditing
}
</script>

<template>
  <div class="ag-cell-label-container" role="presentation" @pointerdown.stop @click.stop>
    <div class="ag-header-cell-label" role="presentation">
      <input
        v-if="editing"
        class="ag-input-field-input ag-text-field-input"
        :value="params.displayName"
        @change="changeName(($event.target as HTMLInputElement).value)"
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.arrow-up.stop
        @keydown.arrow-down.stop
      />
      <span
        v-else
        class="ag-header-cell-text"
        :class="{ virtualColumn: params.virtualColumn === true }"
        @dblclick="editing = params.nameSetter != null"
        v-text="params.displayName"
      ></span>
    </div>
  </div>
</template>

<style>
.virtualColumn {
  color: rgba(0, 0, 0, 0.5);
}
</style>
