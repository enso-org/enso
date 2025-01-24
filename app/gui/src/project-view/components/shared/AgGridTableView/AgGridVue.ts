/**
 * @file Copy of https://github.com/ag-grid/ag-grid/blob/v32.3.3/packages/ag-grid-vue3/src/AgGridVue.ts
 * with special overrides for Vue components removed; we handle them in a better way.
 *
 * Original file licenced under The MIT License:
 *
 * Copyright (c) 2015-2024 AG GRID LTD
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
// This is not our code, so I just blocked failing lints.
/* eslint-disable vue/require-default-prop */
/* eslint-disable vue/require-prop-types */
/* eslint-disable vue/order-in-components */

import type { AgEventType, GridApi, GridOptions, IRowNode, Module } from 'ag-grid-community'
import {
  ALWAYS_SYNC_GLOBAL_EVENTS,
  ComponentUtil,
  _combineAttributesAndGridOptions,
  _processOnChange,
  _warnOnce,
  createGrid,
} from 'ag-grid-community'
import type { PropType } from 'vue'
import { defineComponent, getCurrentInstance, h, markRaw, toRaw } from 'vue'

import type { Properties } from './Utils'
import { convertToRaw, getAgGridProperties } from './Utils'

const ROW_DATA_EVENTS: Set<string> = new Set([
  'rowDataUpdated',
  'cellValueChanged',
  'rowValueChanged',
])
const DATA_MODEL_ATTR_NAME = 'onUpdate:modelValue' // emit name would be update:ModelValue
const DATA_MODEL_EMIT_NAME = 'update:modelValue'

const [props, computed, watch] = getAgGridProperties()

export const AgGridVue = defineComponent({
  render() {
    return h('div')
  },
  props: {
    gridOptions: {
      type: Object as PropType<GridOptions>,
      default: () => ({}) as GridOptions,
    },
    componentDependencies: {
      type: Array as PropType<string[]>,
      default: () => [],
    },
    plugins: [],
    modules: {
      type: Array as PropType<Module[]>,
      default: () => [],
    },
    modelValue: {
      type: Array,
      default: undefined,
      required: false,
    },
    ...props,
  },
  data(): {
    api: GridApi | undefined
    gridCreated: boolean
    isDestroyed: boolean
    gridReadyFired: boolean
    emitRowModel?: (() => void | null) | undefined
    batchTimeout: number | null
    batchChanges: { [key: string]: any }
  } {
    return {
      api: undefined,
      gridCreated: false,
      isDestroyed: false,
      gridReadyFired: false,
      emitRowModel: undefined,
      batchTimeout: null,
      batchChanges: markRaw({}),
    }
  },
  computed,
  watch,
  methods: {
    globalEventListenerFactory(restrictToSyncOnly?: boolean) {
      return (eventType: AgEventType) => {
        if (this.isDestroyed) {
          return
        }

        if (eventType === 'gridReady') {
          this.gridReadyFired = true
        }

        const alwaysSync = ALWAYS_SYNC_GLOBAL_EVENTS.has(eventType)
        if ((alwaysSync && !restrictToSyncOnly) || (!alwaysSync && restrictToSyncOnly)) {
          return
        }

        this.updateModelIfUsed(eventType)
      }
    },
    processChanges(propertyName: string, currentValue: any, previousValue: any) {
      if (this.gridCreated) {
        if (this.skipChange(propertyName, currentValue, previousValue)) {
          return
        }

        const options: Properties = {
          [propertyName]:
            propertyName === 'rowData' ?
              Object.isFrozen(currentValue) ?
                currentValue
              : markRaw(toRaw(currentValue))
            : currentValue,
        }
        // decouple the row data - if we don't when the grid changes row data directly that'll trigger this component to react to rowData changes,
        // which can reset grid state (ie row selection)
        _processOnChange(options, this.api as any)
      }
    },
    checkForBindingConflicts() {
      const thisAsAny = this as any
      if (
        ((thisAsAny.rowData && thisAsAny.rowData !== 'AG-VUE-OMITTED-PROPERTY') ||
          this.gridOptions.rowData) &&
        thisAsAny.modelValue
      ) {
        _warnOnce('Using both rowData and v-model. rowData will be ignored.')
      }
    },
    getRowData(): any[] {
      const rowData: any[] = []
      this.api?.forEachNode((rowNode: IRowNode) => {
        rowData.push(rowNode.data)
      })
      return rowData
    },
    updateModelIfUsed(eventType: string) {
      if (
        this.gridReadyFired &&
        this.$attrs[DATA_MODEL_ATTR_NAME] &&
        ROW_DATA_EVENTS.has(eventType)
      ) {
        if (this.emitRowModel) {
          this.emitRowModel()
        }
      }
    },
    getRowDataBasedOnBindings() {
      const thisAsAny = this as any

      const rowData = thisAsAny.modelValue
      return (
        rowData ? rowData
        : thisAsAny.rowData ? thisAsAny.rowData
        : thisAsAny.gridOptions.rowData
      )
    },
    getProvides() {
      let instance = getCurrentInstance() as any
      let provides = {}

      while (instance) {
        if (instance && instance.provides) {
          provides = { ...provides, ...instance.provides }
        }

        instance = instance.parent
      }

      return provides
    },
    /*
     * Prevents an infinite loop when using v-model for the rowData
     */
    skipChange(propertyName: string, currentValue: any, previousValue: any) {
      if (this.gridReadyFired && propertyName === 'rowData' && this.$attrs[DATA_MODEL_ATTR_NAME]) {
        if (currentValue === previousValue) {
          return true
        }

        if (currentValue && previousValue) {
          const currentRowData = currentValue as any[]
          const previousRowData = previousValue as any[]
          if (currentRowData.length === previousRowData.length) {
            for (let i = 0; i < currentRowData.length; i++) {
              if (currentRowData[i] !== previousRowData[i]) {
                return false
              }
            }
            return true
          }
        }
      }

      return false
    },
    debounce(func: () => void, delay: number) {
      let timeout: number
      return () => {
        const later = function () {
          func()
        }
        window.clearTimeout(timeout)
        timeout = window.setTimeout(later, delay)
      }
    },
  },
  mounted() {
    // we debounce the model update to prevent a flood of updates in the event there are many individual
    // cell/row updates
    this.emitRowModel = this.debounce(() => {
      this.$emit(DATA_MODEL_EMIT_NAME, Object.freeze(this.getRowData()))
    }, 20)

    // the gridOptions we pass to the grid don't need to be reactive (and shouldn't be - it'll cause issues
    // with mergeDeep for example
    const gridOptions = markRaw(_combineAttributesAndGridOptions(toRaw(this.gridOptions), this))

    this.checkForBindingConflicts()

    const rowData = this.getRowDataBasedOnBindings()
    if (rowData !== ComponentUtil.VUE_OMITTED_PROPERTY) {
      gridOptions.rowData = convertToRaw(rowData)
    }

    const gridParams = {
      globalEventListener: this.globalEventListenerFactory().bind(this),
      globalSyncEventListener: this.globalEventListenerFactory(true).bind(this),
      modules: this.modules,
    }

    this.api = createGrid(this.$el as HTMLElement, gridOptions, gridParams)
    this.gridCreated = true
  },
  unmounted() {
    if (this.gridCreated) {
      this.api?.destroy()
      this.isDestroyed = true
    }
  },
})
