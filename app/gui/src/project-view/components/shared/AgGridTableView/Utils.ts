/**
 * @file Copy of https://github.com/ag-grid/ag-grid/blob/v32.3.3/packages/ag-grid-vue3/src/Utils.ts
 * Used by our version of AgGridVue.ts.
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

import { ComponentUtil, _processOnChange } from 'ag-grid-community'
import { markRaw, toRaw } from 'vue'

export const kebabProperty = (property: string) => {
  return property.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()
}

export const kebabNameToAttrEventName = (kebabName: string) => {
  // grid-ready for example would become onGrid-ready in Vue
  return `on${kebabName.charAt(0).toUpperCase()}${kebabName.substring(1, kebabName.length)}`
}

export const convertToRaw = (value: any) =>
  value ?
    Object.isFrozen(value) ?
      value
    : markRaw(toRaw(value))
  : value

export interface Properties {
  [propertyName: string]: any
}

export const getAgGridProperties = (): [Properties, Properties, Properties] => {
  const props: Properties = {}

  // for example, 'grid-ready' would become 'onGrid-ready': undefined
  // without this emitting events results in a warning
  // and adding 'grid-ready' (and variations of this to the emits option in AgGridVue doesn't help either)
  const eventNameAsProps = ComponentUtil.PUBLIC_EVENTS.map((eventName: string) =>
    kebabNameToAttrEventName(kebabProperty(eventName)),
  )
  eventNameAsProps.forEach((eventName: string) => (props[eventName] = undefined))

  const computed: Properties = {}

  const watch: Properties = {
    modelValue: {
      handler(currentValue: any, previousValue: any) {
        if (!this.gridCreated || !this.api) {
          return
        }

        /*
         * Prevents an infinite loop when using v-model for the rowData
         */
        if (currentValue === previousValue) {
          return
        }
        if (currentValue && previousValue) {
          if (currentValue.length === previousValue.length) {
            if (currentValue.every((item: any, index: number) => item === previousValue[index])) {
              return
            }
          }
        }

        _processOnChange({ rowData: currentValue }, this.api)
      },
      deep: true,
    },
  }

  ComponentUtil.ALL_PROPERTIES.filter((propertyName: string) => propertyName != 'gridOptions') // dealt with in AgGridVue itself
    .forEach((propertyName: string) => {
      props[propertyName] = {
        default: ComponentUtil.VUE_OMITTED_PROPERTY,
      }

      watch[propertyName] = {
        handler(currentValue: any, _previousValue: any) {
          let currValue = currentValue

          if (propertyName === 'rowData' && currentValue != ComponentUtil.VUE_OMITTED_PROPERTY) {
            // Prevent the grids internal edits from being reactive
            currValue = convertToRaw(currentValue)
          }

          this.batchChanges[propertyName] =
            currValue === ComponentUtil.VUE_OMITTED_PROPERTY ? undefined : currValue
          if (this.batchTimeout == null) {
            this.batchTimeout = setTimeout(() => {
              // Clear the timeout before processing the changes in case processChanges triggers another change.
              this.batchTimeout = null
              _processOnChange(this.batchChanges, this.api)
              this.batchChanges = markRaw({})
            }, 0)
          }
        },
        deep: true,
      }
    })

  return [props, computed, watch]
}
