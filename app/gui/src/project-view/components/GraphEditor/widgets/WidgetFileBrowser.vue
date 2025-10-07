<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { requiredImportsByProjectPath } from '$/providers/openedProjects/module/imports'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import {
  SUPPORTED_DYNAMIC_CONFIG_KINDS,
  SUPPORTED_TYPES,
  useBrowserTypeInfo,
  useCurrentPath,
  useSetPath,
  useTextSecrets,
} from '@/components/GraphEditor/widgets/WidgetFileBrowser/browsableTypes'
import { useCloudBrowser } from '@/components/GraphEditor/widgets/WidgetFileBrowser/cloudBrowser'
import { useLocalBrowser } from '@/components/GraphEditor/widgets/WidgetFileBrowser/localBrowser'
import { withDropdownItems } from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import {
  type CustomDropdownItem,
  ExpressionTag,
} from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const { suggestionDb, module, graph } = useCurrentProject()

const reprType = computed(() => props.input[ArgumentInfoKey]?.info?.reprType)

const typeInfo = useBrowserTypeInfo({
  reprType,
  dynamicConfig: () => props.input.dynamicConfig,
})

const currentPathAst = useCurrentPath({
  typeInfo,
  input: () => props.input.value,
  getMethodPointer: (id) => graph.value.db.getMethodCallInfo(id)?.methodCall.methodPointer,
})

const dialogKind = computed(() => {
  const types = typeInfo.value.types
  return (
    types.file ? 'file'
    : types.directory ? 'directory'
    : types.secret ? 'secret'
    : 'file'
  )
})

const currentPath = computed(() => {
  if (!currentPathAst.value) return
  if (dialogKind.value === 'secret' && currentPathAst.value.type !== 'secret') return
  if (dialogKind.value !== 'secret' && currentPathAst.value.type === 'secret') return
  return currentPathAst.value.path.rawTextContent
})

const makeSetPathUpdate = useSetPath({
  currentPath: currentPathAst,
  preferRawPath: () => !!typeInfo.value.rawPath?.prefer,
  portId: () => props.input.portId,
  addMissingConstructorImports: (edit, type) =>
    module.value.addMissingImports(
      edit,
      requiredImportsByProjectPath(suggestionDb.value.entries, type, true),
    ) == null,
})

function setPath(type: 'file' | 'secret', path: string) {
  module.value.edit((edit) => props.updateCallback(makeSetPathUpdate(type, path, edit)))
}

const write = computed(() => typeInfo.value.write)

const fileTypes = computed(() => {
  if (props.input.dynamicConfig?.kind === 'File_Browse') {
    return props.input.dynamicConfig?.file_types
  } else {
    return undefined
  }
})

const localBrowserItems = useLocalBrowser({ dialogKind, write, currentPath, setPath, fileTypes })
const cloudBrowserItems = useCloudBrowser({ dialogKind, write, currentPath, setPath, fileTypes })
const textSecretsItems = useTextSecrets({ dialogKind, reprType })

const items = computed((): (CustomDropdownItem | ExpressionTag)[] => [
  ...localBrowserItems.value,
  ...cloudBrowserItems.value,
  ...textSecretsItems.value,
])

const innerWidgetInput = computed(() => withDropdownItems(props.input, items.value))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 49,
    score: (props) => {
      const reprType = props.input[ArgumentInfoKey]?.info?.reprType
      return (
          (props.input.dynamicConfig &&
            SUPPORTED_DYNAMIC_CONFIG_KINDS.includes(props.input.dynamicConfig.kind)) ||
            (reprType && SUPPORTED_TYPES.some((type) => reprType.includes(type)))
        ) ?
          Score.Perfect
        : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="innerWidgetInput" />
</template>
