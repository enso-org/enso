<script setup lang="ts">
import LoadingErrorVisualization from '@/components/visualizations/LoadingErrorVisualization.vue'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import type { Tree } from '@/generated/ast'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import {
  DEFAULT_VISUALIZATION_CONFIGURATION,
  DEFAULT_VISUALIZATION_IDENTIFIER,
  useVisualizationStore,
  type Visualization,
} from '@/stores/visualization'
import type { URLString } from '@/stores/visualization/compilerMessaging'
import type { AstExtended } from '@/util/ast'
import { toError } from '@/util/error'
import type { Icon } from '@/util/iconName'
import type { Opt } from '@/util/opt'
import type { Vec2 } from '@/util/vec2'
import { computedAsync } from '@vueuse/core'
import * as random from 'lib0/random'
import { OutboundPayload, VisualizationUpdate } from 'shared/binaryProtocol'
import type { Uuid } from 'shared/languageServerTypes'
import type { ExprId, VisualizationIdentifier } from 'shared/yjsModel'
import { computed, onErrorCaptured, ref, shallowRef, watch, watchEffect } from 'vue'

const props = defineProps<{
  currentType: Opt<VisualizationIdentifier>
  isCircularMenuVisible: boolean
  nodeSize: Vec2
  typename?: string | undefined
  pattern: AstExtended<Tree.Tree, true> | undefined
  nodeId: string // Should be `ExprId`, but Vue generates the wrong type.
  expressionId?: ExprId | undefined
  data?: any | undefined
}>()
const emit = defineEmits<{
  setVisualizationId: [id: VisualizationIdentifier]
  setVisualizationVisible: [visible: boolean]
}>()

const visPreprocessor = ref(DEFAULT_VISUALIZATION_CONFIGURATION)
const error = ref<Error>()

const projectStore = useProjectStore()
const graphStore = useGraphStore()
const visualizationStore = useVisualizationStore()

const expressionInfo = computed(
  () => props.expressionId && graphStore.db.getExpressionInfo(props.expressionId),
)
const typeName = computed(() => expressionInfo.value?.typename ?? 'Any')

/** The Visualization ID for the method to get the default visualization. */
const defaultVisualizationVisualizationId = random.uuidv4() as Uuid
const defaultVisualization = computedAsync(async () => {
  if (props.currentType) return
  if (!props.expressionId) return
  const ident = props.pattern?.repr()
  if (!ident) return
  const ls = await projectStore.lsRpcConnection
  const dataServer = await projectStore.dataConnection
  ls.executeExpression(
    projectStore.executionContext.id,
    defaultVisualizationVisualizationId,
    props.nodeId as ExprId,
    `${ident}.default_visualization.to_js_object.to_json`,
  )
  return new Promise<VisualizationIdentifier | undefined>((resolve) => {
    const onVisualizationUpdate = (payload: VisualizationUpdate, uuid: Uuid | null) => {
      if (uuid !== defaultVisualizationVisualizationId) return
      dataServer.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, onVisualizationUpdate)
      const data = payload.dataString()
      const parsed: { library: { name: string } | null; name: string } | undefined = data
        ? JSON.parse(data)
        : undefined
      resolve(
        parsed && {
          name: parsed.name,
          module:
            parsed.library == null
              ? { kind: 'Builtin' }
              : { kind: 'Library', name: parsed.library.name },
        },
      )
    }
    dataServer.on(`${OutboundPayload.VISUALIZATION_UPDATE}`, onVisualizationUpdate)
  })
})

const currentType = computed(() => {
  if (props.currentType) return props.currentType
  if (defaultVisualization.value) return defaultVisualization.value
  const [id] = visualizationStore.types(typeName.value)
  return id
})

const visualization = shallowRef<Visualization>()
const icon = ref<Icon | URLString>()

onErrorCaptured((vueError) => {
  error.value = vueError
  return false
})

const visualizationData = projectStore.useVisualizationData(() => {
  return props.data == null && props.expressionId != null
    ? {
        ...visPreprocessor.value,
        expressionId: props.expressionId,
      }
    : null
})

const effectiveVisualizationData = computed(() =>
  error.value
    ? { name: currentType.value?.name, error: error.value }
    : props.data ?? visualizationData.value,
)

function updatePreprocessor(
  visualizationModule: string,
  expression: string,
  ...positionalArgumentsExpressions: string[]
) {
  visPreprocessor.value = { visualizationModule, expression, positionalArgumentsExpressions }
}

function switchToDefaultPreprocessor() {
  visPreprocessor.value = DEFAULT_VISUALIZATION_CONFIGURATION
}

watch(
  () => [currentType.value, visualization.value],
  () => (error.value = undefined),
)

watchEffect(async () => {
  if (currentType.value == null) return
  visualization.value = undefined
  icon.value = undefined
  try {
    const module = await visualizationStore.get(currentType.value).value
    if (module) {
      if (module.defaultPreprocessor != null) {
        updatePreprocessor(...module.defaultPreprocessor)
      } else {
        switchToDefaultPreprocessor()
      }
      visualization.value = module.default
      icon.value = module.icon
    } else {
      switch (currentType.value.module.kind) {
        case 'Builtin': {
          error.value = new Error(
            `The builtin visualization '${currentType.value.name}' was not found.`,
          )
          break
        }
        case 'CurrentProject': {
          error.value = new Error(
            `The visualization '${currentType.value.name}' was not found in the current project.`,
          )
          break
        }
        case 'Library': {
          error.value = new Error(
            `The visualization '${currentType.value.name}' was not found in the library '${currentType.value.module.name}'.`,
          )
          break
        }
      }
    }
  } catch (caughtError) {
    error.value = toError(caughtError)
  }
})

provideVisualizationConfig({
  fullscreen: false,
  width: null,
  height: 150,
  get types() {
    return visualizationStore.types(props.typename)
  },
  get isCircularMenuVisible() {
    return props.isCircularMenuVisible
  },
  get nodeSize() {
    return props.nodeSize
  },
  get currentType() {
    return currentType.value ?? DEFAULT_VISUALIZATION_IDENTIFIER
  },
  get icon() {
    return icon.value
  },
  hide: () => emit('setVisualizationVisible', false),
  updateType: (id) => emit('setVisualizationId', id),
})

const effectiveVisualization = computed(() => {
  if (error.value) {
    return LoadingErrorVisualization
  }
  if (!visualization.value || effectiveVisualizationData.value == null) {
    return LoadingVisualization
  }
  return visualization.value
})
</script>

<template>
  <div class="GraphVisualization">
    <Suspense>
      <template #fallback><LoadingVisualization :data="{}" /></template>
      <component
        :is="effectiveVisualization"
        :data="effectiveVisualizationData"
        @update:preprocessor="updatePreprocessor"
      />
    </Suspense>
  </div>
</template>
