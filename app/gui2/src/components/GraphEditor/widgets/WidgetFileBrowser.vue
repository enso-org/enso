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
import { Pattern } from '@/util/ast/match'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed } from 'vue'
import { TextLiteral } from 'ydoc-shared/ast'

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
const dialogKind = computed(() => {
  switch (props.input.dynamicConfig?.kind) {
    case 'File_Browse':
      return props.input.dynamicConfig.existing_only ? 'file' : 'filePath'
    case 'Folder_Browse':
      return 'directory'
    default:
      if (props.input[ArgumentInfoKey]?.info?.reprType.includes(WRITABLE_FILE_TYPE)) {
        return 'filePath'
      } else {
        return 'default'
      }
  }
})
const label = computed(() => {
  switch (dialogKind.value) {
    case 'directory':
      return 'Choose directory…'
    case 'filePath':
      return 'Choose path…'
    default:
      return 'Choose file…'
  }
})

const fileConPattern = Pattern.parse(`${FILE_TYPE}.new __`)
const fileShortConPattern = Pattern.parse(`File.new __`)
const currentPath = computed(() => {
  if (typeof props.input.value === 'string') {
    return props.input.value
  } else if (props.input.value) {
    const expression = props.input.value.innerExpression()
    const match = fileShortConPattern.match(expression) ?? fileConPattern.match(expression)
    const pathAst =
      match && match[0] ? expression.module.get(match[0]).innerExpression() : expression
    if (pathAst instanceof TextLiteral) {
      return pathAst.rawTextContent
    }
  }
  return undefined
})

function makeValue(edit: Ast.MutableModule, useFileConstructor: boolean, path: string): Ast.Owned {
  if (useFileConstructor) {
    const arg = Ast.TextLiteral.new(path, edit)
    const requiredImport = {
      kind: 'Unqualified',
      from: FILE_MODULE,
      import: 'File',
    } as RequiredImport
    const conflicts = graph.addMissingImports(edit, [requiredImport])
    const pattern = conflicts ? fileConPattern : fileShortConPattern
    return pattern.instantiate(edit, [arg])
  } else {
    return Ast.TextLiteral.new(path, edit)
  }
}

const onClick = async () => {
  if (!window.fileBrowserApi) {
    console.error('File browser not supported!')
  } else {
    const selected = await window.fileBrowserApi.openFileBrowser(
      dialogKind.value,
      currentPath.value,
    )
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
const WRITABLE_FILE_MODULE = 'Standard.Base.System.File.Generic.Writable_File'
const WRITABLE_FILE_TYPE = WRITABLE_FILE_MODULE + '.Writable_File'

export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 49,
    score: (props) => {
      if (
        props.input.dynamicConfig?.kind === 'File_Browse' ||
        props.input.dynamicConfig?.kind === 'Folder_Browse'
      )
        return Score.Perfect
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
