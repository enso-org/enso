<script lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { provideTooltipRegistry, type TooltipRegistry } from '@/providers/tooltipRegistry'
import type { IHeaderParams } from 'ag-grid-community'
import { computed, onMounted, onUnmounted, Raw, Ref, ref, toRef, watch } from 'vue'

export type ColumnSpecificProps =
  | {
      type: 'astColumn'
      /** Setter called when column name is changed by the user. */
      nameSetter: (newName: string) => void
    }
  | { type: 'newColumn'; enabled?: boolean; newColumnRequested: () => void }
  | { type: 'rowIndexColumn' }

export interface GeneralProps {
  /**
   * AgGrid mounts header components as separate "App", so we don't have access to any context.
   * Threfore the tooltip registry must be provided by props.
   */
  tooltipRegistry: TooltipRegistry
  /**
   * The id of column whose header is currently edited.
   */
  editedColId: string | undefined
  onHeaderEditingStarted?: (colId: string, revertChanges: () => void) => void
  onHeaderEditingStopped?: (colId: string) => void
}

/**
 * General (column-intependent) parameters recognized by this header component.
 *
 * They are set through `headerComponentParams` option in AGGrid default column definition.
 */
export interface GeneralHeaderParams {
  general: Raw<Ref<GeneralProps>>
}

/**
 * Column-specific parameters recognized by this header component.
 *
 * They are set through `headerComponentParams` option in AGGrid column definition.
 */
export interface ColumnSpecificHeaderParams {
  columnSpecific: Raw<Ref<ColumnSpecificProps>>
}
</script>

<script setup lang="ts">
const props = defineProps<{
  params: IHeaderParams & GeneralHeaderParams & ColumnSpecificHeaderParams
}>()
const generalProps = toRef(() => props.params.general.value)
const columnProps = toRef(() => props.params.columnSpecific.value)

/** Re-provide tooltipRegistry. See `tooltipRegistry` docs in {@link HeaderParams} */
provideTooltipRegistry.provideConstructed(generalProps.value.tooltipRegistry)

const editing = computed(() => generalProps.value.editedColId === props.params.column.getColId())
const inputElement = ref<HTMLInputElement>()

function emitEditStart() {
  generalProps.value.onHeaderEditingStarted?.(props.params.column.getColId(), () => {
    if (inputElement.value) {
      inputElement.value.value = props.params.displayName
    }
  })
}

function emitEditEnd() {
  generalProps.value.onHeaderEditingStopped?.(props.params.column.getColId())
}

watch(editing, (newVal) => {
  if (!newVal) {
    acceptNewName()
  }
})

watch(
  inputElement,
  (newVal, oldVal) => {
    if (newVal != null && oldVal == null) {
      // Whenever input field appears, focus and select text
      newVal.focus()
      newVal.select()
    }
  },
  { immediate: true },
)

function acceptNewName() {
  if (columnProps.value.type !== 'astColumn') {
    console.error("Tried to accept header new name where it's not editable!")
    return
  }
  if (inputElement.value == null) {
    console.error('Tried to accept header new name without input element!')
    return
  }
  columnProps.value.nameSetter(inputElement.value.value)
  if (editing.value) emitEditEnd()
}

function onMouseClick(event: MouseEvent) {
  if (!editing.value && columnProps.value.type === 'astColumn') {
    emitEditStart()
  } else {
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
onMounted(() => {
  console.log('MOUTED', props.params.displayName)
})
onUnmounted(() => {
  console.log('UNMOUTED', props.params.displayName)
})
</script>

<template>
  <SvgButton
    v-if="columnProps.type === 'newColumn'"
    class="addColumnButton"
    name="add"
    title="Add new column"
    :disabled="!(columnProps.enabled ?? true)"
    @click.stop="columnProps.newColumnRequested()"
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
        @blur="emitEditEnd()"
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.arrow-up.stop
        @keydown.arrow-down.stop
      />
      <span
        v-else
        class="ag-header-cell-text"
        :class="{ virtualColumn: columnProps.type !== 'astColumn' }"
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
