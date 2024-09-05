<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { FileUploadKey } from '@/components/GraphEditor/widgets/WidgetFileUploadProgress.vue'
import {
  CustomDropdownItemsKey,
  type CustomDropdownItem,
} from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import { injectKeyboard } from '@/providers/keyboard'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed } from 'vue'
import { TextLiteral } from 'ydoc-shared/ast'
import { assertDefined } from 'ydoc-shared/util/assert'
import { uploadedExpressionPath, Uploader } from '../upload'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const projectStore = useProjectStore()
const keyboard = injectKeyboard(true)

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

const uploadStatus = computed(() => {
  let uploads = projectStore.awareness.allUploads()
  if (uploads.length == 0) return undefined
  return uploads.find(([, file]) => file.portId === props.input.portId)
})

const style = computed(() => {
  const status = uploadStatus.value
  if (status) {
    return {
      '--upload-progress': `${status[1].sizePercentage}%`,
    }
  } else {
    return {}
  }
})

const onClick = async () => {
  if (!window.fileBrowserApi) {
    const selected = await openFileDialog(dialogKind.value, false)
    const rootId = await projectStore.projectRootId
    assertDefined(rootId)
    if (selected != null && selected[0] != null) {
      const uploader = Uploader.Create(
        projectStore.lsRpcConnection,
        projectStore.dataConnection,
        rootId,
        projectStore.awareness,
        selected[0],
        projectStore.isOnLocalBackend,
        keyboard?.shift ?? false,
        projectStore.executionContext.getStackTop(),
      )

      const uploadResult = await uploader.upload({ portId: props.input.portId })
      if (uploadResult.ok) {
        const edit = graph.startEdit()
        props.onUpdate({
          edit,
          portUpdate: {
            value: uploadedExpressionPath(uploadResult.value),
            origin: props.input.portId,
          },
        })
      }
    }
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

/**
 * Open "file open" system dialog using a temporary file input DOM node.
 *
 * his function must be called from a user activation event (ie an onclick event),
 * otherwise the dispatchEvent will have no effect.
 */

function openFileDialog(dialogKind: string, multiple: boolean): Promise<FileList | null> {
  return new Promise((resolve) => {
    var inputElement = document.createElement('input')
    inputElement.type = 'file'
    inputElement.multiple = multiple
    if (dialogKind === 'directory') inputElement.webkitdirectory = true
    inputElement.addEventListener('change', () => resolve(inputElement.files))
    inputElement.dispatchEvent(new MouseEvent('click'))
  })
}

const item = computed<CustomDropdownItem>(() => ({
  label: label.value,
  onClick,
}))

const innerWidgetInput = computed(() => {
  const existingItems = props.input[CustomDropdownItemsKey] ?? []
  const upload = uploadStatus.value
  return {
    ...props.input,
    [CustomDropdownItemsKey]: [...existingItems, item.value],
    ...(upload ?
      {
        [FileUploadKey]: {
          name: upload[0],
          file: upload[1],
        },
      }
    : {}),
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
  <div class="WidgetFileBrowser" :style="style">
    <NodeWidget :input="innerWidgetInput" />
  </div>
</template>

<style scoped>
.WidgetFileBrowser {
  display: flex;
  flex-direction: row;
}
</style>
