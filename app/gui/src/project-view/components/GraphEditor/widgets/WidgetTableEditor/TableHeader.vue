<script lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import type { IHeaderParams } from 'ag-grid-community'
import { computed, ref, watch } from 'vue'

/**
 * Column-specific header parameters, set in particular column's definitions.
 */
export interface ColumnSpecificParams {
  columnParams:
    | {
        type: 'astColumn'
        /** Setter called when column name is changed by the user. */
        nameSetter: (newName: string) => void
      }
    | { type: 'newColumn'; enabled?: boolean; newColumnRequested: () => void }
    | { type: 'rowIndexColumn' }
}

/**
 * General header parameters, set in default column configuration.
 */
export interface HeaderParams {
  /** The id of column whose header is currently edited. */
  editedColId?: string | undefined
  /** Callback called when editing this column is requested. */
  onHeaderEditingStarted: (colId: string, revertChanges: () => void) => void
  /** Callback called when editing of this column should be finished. */
  onHeaderEditingStopped: (colId: string) => void
}
</script>

<script setup lang="ts">
const props = defineProps<IHeaderParams & HeaderParams & ColumnSpecificParams>()

const editing = computed(() => props.editedColId === props.column.getColId())
watch(editing, (newVal) => {
  if (!newVal) {
    acceptNewName()
  }
})

const inputElement = ref<HTMLInputElement>()

function emitEditStart() {
  props.onHeaderEditingStarted?.(props.column.getColId(), () => {
    if (inputElement.value) {
      inputElement.value.value = props.displayName
    }
  })
}

function emitEditEnd() {
  props.onHeaderEditingStopped?.(props.column.getColId())
}

watch(inputElement, (newVal, oldVal) => {
  if (newVal != null) {
    // Whenever input field appears, focus and select text
    newVal.focus()
    newVal.select()
  }
})

function acceptNewName() {
  if (props.columnParams.type !== 'astColumn') {
    console.error("Tried to accept header new name where it's not editable!")
    return
  }
  if (inputElement.value == null) {
    console.error('Tried to accept header new name without input element!')
    return
  }
  if (inputElement.value.value !== props.displayName)
    props.columnParams.nameSetter(inputElement.value.value)
  if (editing.value) emitEditEnd()
}

function onMouseClick(event: MouseEvent) {
  if (!editing.value && props.columnParams.type === 'astColumn') {
    emitEditStart()
  } else {
    event.stopPropagation()
  }
}

function onMouseRightClick(event: MouseEvent) {
  if (!editing.value) {
    props.showColumnMenuAfterMouseClick(event)
    event.preventDefault()
    event.stopPropagation()
  }
}
</script>

<template>
  <SvgButton
    v-if="columnParams.type === 'newColumn'"
    class="addColumnButton"
    name="add"
    title="Add new column"
    :disabled="!(columnParams.enabled ?? true)"
    @click.stop="columnParams.newColumnRequested()"
  />
  <div
    v-else
    class="ag-cell-label-container"
    role="presentation"
    @pointerdown.stop
    @click="onMouseClick"
    @click.right="onMouseRightClick"
  >
    <div class="ag-header-cell-label" role="presentation">
      <input
        v-if="editing"
        ref="inputElement"
        class="ag-input-field-input ag-text-field-input"
        :value="displayName"
        @change="acceptNewName"
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.arrow-up.stop
        @keydown.arrow-down.stop
      />
      <span
        v-else
        class="ag-header-cell-text"
        :class="{ virtualColumn: columnParams.type !== 'astColumn' }"
        >{{ displayName }}</span
      >
    </div>
  </div>
</template>

<style scoped>
.addColumnButton {
  margin-left: 10px;
}

.virtualColumn {
  color: rgba(0, 0, 0, 0.5);
}
</style>
