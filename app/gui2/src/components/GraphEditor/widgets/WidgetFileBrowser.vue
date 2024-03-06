<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { asNot } from '@/util/data/types.ts'
import { provideCustomDropdownItems } from '@/providers/customDropdownItems'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import type { TokenId } from '@/util/ast/abstract.ts'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed, ref } from 'vue'
import { Ast } from '@/util/ast'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'

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
const label = computed(() => strictlyDirectory.value ? 'Choose directory…' : 'Choose file…')

const FILE_CONSTRUCTOR = FILE_TYPE + '.new'
const FILE_SHORT_CONSTRUCTOR = 'File.new'

function makeValue(edit: Ast.MutableModule, useFileConstructor: boolean, path: string): Ast.Owned {
  if (useFileConstructor) {
    const arg = Ast.TextLiteral.new(path, edit)
    const requiredImport = {
      kind: 'Unqualified',
      from: FILE_MODULE,
      import: 'File'
    } as RequiredImport
    const conflicts = graph.addMissingImports(edit, [requiredImport])
    const constructor = conflicts != null ? FILE_CONSTRUCTOR : FILE_SHORT_CONSTRUCTOR
    return Ast.parse(`${constructor} ${arg.code()}`, edit)
  } else {
    return Ast.TextLiteral.new(path, edit)
  }
}

provideCustomDropdownItems({
  items: [label],
  onClick: async (_idx: number) => {
    const kind = strictlyDirectory.value ? 'directory' : strictlyFile.value ? 'file' : 'any'
    const selected = await window.fileBrowserApi.openFileBrowser(kind)
    if (selected != null && selected[0] != null) {
      const edit = graph.startEdit()
      const value = makeValue(edit, insertAsFileConstructor.value, selected[0])
      props.onUpdate({
        edit,
        portUpdate: {
          value,
          origin: asNot<TokenId>(props.input.portId),
        }
      })
    }
  }
})

const innerWidgetInput = computed(() => {
  const info = props.input[ArgumentInfoKey]?.info
  // Ensure tagValues is initialized, so a child dropdown widget will be created.
  if (info != null && info.tagValues == null) {
    info.tagValues = []
  }
  return props.input
})
</script>

<script lang="ts">
const TEXT_TYPE = 'Standard.Base.Data.Text.Text'
const FILE_MODULE = 'Standard.Base.System.File'
const FILE_TYPE = FILE_MODULE + '.File'

export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 49,
  score: (props) => {
    if (props.input.dynamicConfig?.kind === 'File_Browse' || props.input.dynamicConfig?.kind === 'Folder_Browse') return Score.Perfect
    if (props.input[ArgumentInfoKey]?.info?.reprType.includes(FILE_TYPE)) return Score.Perfect
    return Score.Mismatch
  },
})

declare global {
  interface Window {
    fileBrowserApi: FileBrowserApi
  }
}

/** `window.fileBrowserApi` is a context bridge to the main process, when we're running in an
 * Electron context.
 *
 * # Safety
 *
 * We're assuming that the main process has exposed the `fileBrowserApi` context bridge (see
 * `app/ide-desktop/lib/client/src/preload.ts` for details), and that it contains the functions defined in this
 * interface.
 */
interface FileBrowserApi {
  /** Select path for local file or directory using the system file browser. */
  readonly openFileBrowser: (kind: 'file' | 'directory' | 'any') => Promise<string[] | undefined>
}
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
