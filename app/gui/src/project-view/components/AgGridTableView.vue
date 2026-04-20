<script lang="ts">
export { commonContextMenuActions, type MenuItem } from '@/components/shared/AgGridTableView.vue'
</script>

<script setup lang="ts" generic="TData, TValue">
/** AG Grid table view component for use outside of custom elements. */
import {
  default as AgGridTableView,
  type AgGridTableViewProps,
} from '@/components/shared/AgGridTableView.vue'
import { computed, ref } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'

type GridView = typeof AgGridTableView<TData, TValue>

const grid = ref<ComponentExposed<GridView>>()

defineProps<AgGridTableViewProps<TData, TValue>>()
defineExpose({
  gridApi: computed(() => grid.value?.gridApi),
  forceGridRefresh: () => grid.value?.forceGridRefresh,
})
defineOptions({ inheritAttrs: false })
</script>

<template>
  <Suspense suspensible>
    <AgGridTableView ref="grid" v-bind="{ ...$props, ...$attrs }" />
  </Suspense>
</template>

<style src="@ag-grid-community/styles/ag-grid.css" />
<style src="@ag-grid-community/styles/ag-theme-alpine.css" />
<style src="@/components/shared/AgGridTableView/tableViewStyle.css" />
