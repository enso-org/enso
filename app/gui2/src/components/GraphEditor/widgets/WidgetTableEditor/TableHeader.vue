<script lang="ts">
import type { IHeaderParams } from 'ag-grid-community'
import { onUnmounted, ref, watch } from 'vue'

/** Parameters recognized by this header component.
 *
 * They are set through `headerComponentParams` option in AGGrid column definition.
 */
export interface HeaderParams {
  /** Setter called when column name is changed by the user. */
  nameSetter?: (newName: string) => void
  /** Column is virtual if it is not represented in the AST. Such column might be used
   * to create new one.
   */
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

watch(editing, (newVal) => {
  if (newVal) {
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
  props.params.nameSetter?.(inputElement.value.value)
  editing.value = false
}

function onMouseClick() {
  if (!editing.value && props.params.nameSetter != null) {
    editing.value = true
  }
}

function onMouseRightClick(event: MouseEvent) {
  if (!editing.value) {
    props.params.showColumnMenuAfterMouseClick(event)
  }
}
</script>

<template>
  <div
    class="ag-cell-label-container"
    role="presentation"
    @pointerdown.stop
    @click.stop
    @click="onMouseClick"
    @click.right="onMouseRightClick"
  >
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
