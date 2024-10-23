<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import {
  CustomDropdownItemsKey,
  type CustomDropdownItem,
} from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import FileBrowserWidget from '@/components/widgets/FileBrowserWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed, h } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const item: CustomDropdownItem = {
  label: 'Choose file from cloud...',
  onClick: ({ setActivity, close }) => {
    setActivity(
      h(FileBrowserWidget, {
        onPathSelected: (path: string) => {
          props.onUpdate({
            portUpdate: { value: Ast.TextLiteral.new(path), origin: props.input.portId },
          })
          close()
        },
      }),
    )
  },
}

const innerWidgetInput = computed(() => {
  const existingItems = props.input[CustomDropdownItemsKey] ?? []
  return {
    ...props.input,
    [CustomDropdownItemsKey]: [...existingItems, item],
  }
})
</script>

<script lang="ts">
const FILE_MODULE = 'Standard.Base.System.File'
const FILE_TYPE = FILE_MODULE + '.File'
const WRITABLE_FILE_MODULE = 'Standard.Base.System.File.Generic.Writable_File'
const WRITABLE_FILE_TYPE = WRITABLE_FILE_MODULE + '.Writable_File'

export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 49,
    score: (props) => {
      if (props.input.dynamicConfig?.kind === 'File_Browse') return Score.Perfect
      const reprType = props.input[ArgumentInfoKey]?.info?.reprType
      if (reprType?.includes(FILE_TYPE) || reprType?.includes(WRITABLE_FILE_TYPE))
        return Score.Perfect
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetCloudBrowser">
    <NodeWidget :input="innerWidgetInput" />
  </div>
</template>
