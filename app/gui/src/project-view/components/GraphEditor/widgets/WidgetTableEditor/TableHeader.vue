<script lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { provideTooltipRegistry, TooltipRegistry } from '@/providers/tooltipState'
import type { IHeaderParams } from 'ag-grid-community'
import { computed, ref, watch } from 'vue'

export interface HeaderEditHandlers {
  /** Setter called when column name is changed by the user. */
  nameSetter: (newName: string) => void
  onHeaderEditingStarted?: (stop: (cancel: boolean) => void) => void
  onHeaderEditingStopped?: () => void
}

/**
 * Parameters recognized by this header component.
 *
 * They are set through `headerComponentParams` option in AGGrid column definition.
 */
export type HeaderParams = { tooltipRegistry: TooltipRegistry } & (
  | {
      type: 'astColumn'
      editHandlers: HeaderEditHandlers
    }
  | { type: 'newColumn'; newColumnRequested: () => void }
  | { type: 'rowIndexColumn ' }
)
</script>

<script setup lang="ts">
const props = defineProps<{
  params: IHeaderParams & HeaderParams
}>()

// console.log(props.params.tooltipRegistry)
// provideTooltipRegistry._mock(props.params.tooltipRegistry)
provideTooltipRegistry()

const editing = ref(false)
const inputElement = ref<HTMLInputElement>()
const editHandlers = computed(() =>
  props.params.type === 'astColumn' ? props.params.editHandlers : undefined,
)

watch(editing, (newVal) => {
  if (newVal) {
    editHandlers.value?.onHeaderEditingStarted?.((cancel: boolean) => {
      if (cancel) editing.value = false
      else acceptNewName()
    })
  } else {
    editHandlers.value?.onHeaderEditingStopped?.()
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
  if (editHandlers.value == null) {
    console.warn("Tried to accept header new name where it's not editable!")
    return
  }
  if (inputElement.value == null) {
    console.warn('Tried to accept header new name without input element!')
    return
  }
  editHandlers.value.nameSetter(inputElement.value.value)
  editing.value = false
}

function onMouseClick(event: MouseEvent) {
  if (!editing.value && props.params.type === 'astColumn') {
    editing.value = true
    event.stopPropagation()
  }
}

function onMouseRightClick(event: MouseEvent) {
  if (!editing.value) {
    props.params.showColumnMenuAfterMouseClick(event)
    event.preventDefault()
    event.stopPropagation()
  }
}
</script>

<template>
  <SvgButton
    v-if="params.type === 'newColumn'"
    class="addColumnButton"
    name="add"
    title="Add new column"
    @click.stop="params.newColumnRequested()"
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
        :class="{ virtualColumn: params.type !== 'astColumn' }"
        >{{ params.displayName }}</span
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
