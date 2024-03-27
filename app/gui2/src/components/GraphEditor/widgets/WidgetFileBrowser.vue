<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import {
  CustomDropdownItemsKey,
  type CustomDropdownItem,
} from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const insertAsFileConstructor = computed(() => {
  const reprType = props.input[ArgumentInfoKey]?.info?.reprType
  if (reprType == null) return false
  const textTypePos = reprType.indexOf(TEXT_TYPE)
  const fileTypePos = reprType.indexOf(FILE_TYPE)
  // If both types are present, check if TEXT_TYPE comes after FILE_TYPE.
  if (fileTypePos !== -1 && (textTypePos === -1 || textTypePos > fileTypePos)) {
    return true
  } else {
    return false
  }
})
const strictlyFile = computed(() => props.input.dynamicConfig?.kind === 'File_Browse')
const strictlyDirectory = computed(() => props.input.dynamicConfig?.kind === 'Folder_Browse')
const label = computed(() => (strictlyDirectory.value ? 'Choose directory…' : 'Choose file…'))

const FILE_CONSTRUCTOR = FILE_TYPE + '.new'
const FILE_SHORT_CONSTRUCTOR = 'File.new'

function makeValue(edit: Ast.MutableModule, useFileConstructor: boolean, path: string): Ast.Owned {
  if (useFileConstructor) {
    const arg = Ast.TextLiteral.new(path, edit)
    const requiredImport = {
      kind: 'Unqualified',
      from: FILE_MODULE,
      import: 'File',
    } as RequiredImport
    const conflicts = graph.addMissingImports(edit, [requiredImport])
    const constructor = conflicts != null ? FILE_CONSTRUCTOR : FILE_SHORT_CONSTRUCTOR
    const constructorAst = Ast.PropertyAccess.tryParse(constructor, edit)
    if (constructorAst == null) {
      throw new Error(`Failed to parse constructor as AST: ${constructor}`)
    }
    return Ast.App.new(edit, constructorAst, undefined, arg)
  } else {
    return Ast.TextLiteral.new(path, edit)
  }
}

const onClick = async () => {
  const kind =
    strictlyDirectory.value ? 'directory'
    : strictlyFile.value ? 'file'
    : 'default'
  const selected = await window.fileBrowserApi.openFileBrowser(kind)
  if (selected != null && selected[0] != null) {
    const edit = graph.startEdit()
    const value = makeValue(edit, insertAsFileConstructor.value, selected[0])
    props.onUpdate({
      edit,
      portUpdate: {
        value,
        origin: props.input.portId,
      },
    })
  }
}

const item = computed<CustomDropdownItem>(() => ({
  label: label.value,
  onClick,
}))

const innerWidgetInput = computed(() => {
  const existingItems = props.input[CustomDropdownItemsKey] ?? []
  return {
    ...props.input,
    [CustomDropdownItemsKey]: [...existingItems, item.value],
  }
})
</script>

<script lang="ts">
const TEXT_TYPE = 'Standard.Base.Data.Text.Text'
const FILE_MODULE = 'Standard.Base.System.File'
const FILE_TYPE = FILE_MODULE + '.File'

export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 49,
  score: (props) => {
    if (
      props.input.dynamicConfig?.kind === 'File_Browse' ||
      props.input.dynamicConfig?.kind === 'Folder_Browse'
    )
      return Score.Perfect
    if (props.input[ArgumentInfoKey]?.info?.reprType.includes(FILE_TYPE)) return Score.Perfect
    return Score.Mismatch
  },
})
</script>

<template>
  <div class="WidgetFileBrowser">
    <NodeWidget :input="innerWidgetInput" />
  </div>
</template>

<style scoped>
.WidgetFileBrowser {
  display: flex;
  flex-direction: row;
}
</style>
