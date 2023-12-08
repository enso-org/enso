<script setup lang="ts">
import LoadingErrorVisualization from '@/components/visualizations/LoadingErrorVisualization.vue'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore, type NodeVisualizationConfiguration } from '@/stores/project'
import {
  DEFAULT_VISUALIZATION_CONFIGURATION,
  DEFAULT_VISUALIZATION_IDENTIFIER,
  useVisualizationStore,
  type VisualizationDataSource,
} from '@/stores/visualization'
import type { Visualization } from '@/stores/visualization/runtimeTypes'
import { toError } from '@/util/error'
import type { Icon } from '@/util/iconName'
import type { Opt } from '@/util/opt'
import { Rect } from '@/util/rect'
import type { Result } from '@/util/result'
import type { URLString } from '@/util/urlString'
import { Vec2 } from '@/util/vec2'
import { computedAsync } from '@vueuse/core'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import {
  computed,
  onErrorCaptured,
  onUnmounted,
  ref,
  shallowRef,
  watch,
  watchEffect,
  type ShallowRef,
} from 'vue'

const TOP_WITHOUT_TOOLBAR_PX = 36
const TOP_WITH_TOOLBAR_PX = 72

const props = defineProps<{
  currentType?: Opt<VisualizationIdentifier>
  isCircularMenuVisible: boolean
  nodePosition: Vec2
  nodeSize: Vec2
  scale: number
  typename?: string | undefined
  dataSource?: VisualizationDataSource | undefined
  data?: any | undefined
}>()
const emit = defineEmits<{
  'update:rect': [rect: Rect | undefined]
  'update:id': [id: VisualizationIdentifier]
  'update:visible': [visible: boolean]
}>()

const visPreprocessor = ref(DEFAULT_VISUALIZATION_CONFIGURATION)
const vueError = ref<Error>()

const projectStore = useProjectStore()
const graphStore = useGraphStore()
const visualizationStore = useVisualizationStore()

const expressionInfo = computed(() =>
  props.dataSource?.type === 'node'
    ? graphStore.db.getExpressionInfo(props.dataSource.nodeId)
    : undefined,
)
const typeName = computed(() => expressionInfo.value?.typename ?? 'Any')

const configForGettingDefaultVisualization = computed<NodeVisualizationConfiguration | undefined>(
  () => {
    if (props.currentType) return
    if (props.dataSource?.type !== 'node') return
    return {
      visualizationModule: 'Standard.Visualization.Helpers',
      expression: 'a -> a.default_visualization.to_js_object.to_json',
      expressionId: props.dataSource.nodeId,
    }
  },
)

const defaultVisualizationRaw = projectStore.useVisualizationData(
  configForGettingDefaultVisualization,
) as ShallowRef<Result<{ library: { name: string } | null; name: string } | undefined>>

const defaultVisualization = computed<VisualizationIdentifier | undefined>(() => {
  const raw = defaultVisualizationRaw.value
  if (!raw?.ok || !raw.value) return
  return {
    name: raw.value.name,
    module:
      raw.value.library == null
        ? { kind: 'Builtin' }
        : { kind: 'Library', name: raw.value.library.name },
  }
})

const currentType = computed(() => {
  if (props.currentType) return props.currentType
  if (defaultVisualization.value) return defaultVisualization.value
  const [id] = visualizationStore.types(typeName.value)
  return id
})

const visualization = shallowRef<Visualization>()
const icon = ref<Icon | URLString>()

onErrorCaptured((error) => {
  vueError.value = error
  return false
})

const visualizationData = projectStore.useVisualizationData(() => {
  return props.data == null && props.dataSource?.type === 'node'
    ? {
        ...visPreprocessor.value,
        expressionId: props.dataSource.nodeId,
      }
    : null
})

const expressionVisualizationData = computedAsync(() => {
  if (props.dataSource?.type !== 'expression') return
  const preprocessor = visPreprocessor.value
  const args = preprocessor.positionalArgumentsExpressions
  const argsCode = args.length ? `(${args.join(') (')})` : ''
  // TODO[ao]: it work with builtin visualization, but does not work in general case.
  // Tracked in https://github.com/orgs/enso-org/discussions/6832#discussioncomment-7754474.
  const preprocessorCode = `${preprocessor.visualizationModule}.${preprocessor.expression} _ ${argsCode}`
  const expression = `${preprocessorCode} <| ${props.dataSource.expression}`
  return projectStore.executeExpression(props.dataSource.contextId, expression)
})

const effectiveVisualizationData = computed(() => {
  const name = currentType.value?.name
  if (props.data) return props.data
  if (vueError.value) return { name, error: vueError.value }
  if (visualizationData.value && !visualizationData.value.ok)
    return { name, error: new Error(visualizationData.value.error.payload) }
  if (expressionVisualizationData.value && !expressionVisualizationData.value.ok)
    return { name, error: new Error(expressionVisualizationData.value.error.payload) }
  return visualizationData.value?.value ?? expressionVisualizationData.value?.value
})

function updatePreprocessor(
  visualizationModule: string,
  expression: string,
  ...positionalArgumentsExpressions: string[]
) {
  visPreprocessor.value = { visualizationModule, expression, positionalArgumentsExpressions }
}
// Required to work around janky Vue definitions for the type of a Visualization
const updatePreprocessor_ = updatePreprocessor as (...args: unknown[]) => void

function switchToDefaultPreprocessor() {
  visPreprocessor.value = DEFAULT_VISUALIZATION_CONFIGURATION
}

watch(
  () => [currentType.value, visualization.value],
  () => (vueError.value = undefined),
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
          vueError.value = new Error(
            `The builtin visualization '${currentType.value.name}' was not found.`,
          )
          break
        }
        case 'CurrentProject': {
          vueError.value = new Error(
            `The visualization '${currentType.value.name}' was not found in the current project.`,
          )
          break
        }
        case 'Library': {
          vueError.value = new Error(
            `The visualization '${currentType.value.name}' was not found in the library '${currentType.value.module.name}'.`,
          )
          break
        }
      }
    }
  } catch (caughtError) {
    vueError.value = toError(caughtError)
  }
})

const isBelowToolbar = ref(false)
let width = ref<number | null>(null)
let height = ref(150)

watchEffect(() =>
  emit(
    'update:rect',
    new Rect(
      props.nodePosition,
      new Vec2(
        width.value ?? props.nodeSize.x,
        height.value + (isBelowToolbar.value ? TOP_WITH_TOOLBAR_PX : TOP_WITHOUT_TOOLBAR_PX),
      ),
    ),
  ),
)

onUnmounted(() => emit('update:rect', undefined))

provideVisualizationConfig({
  fullscreen: false,
  get scale() {
    return props.scale
  },
  get width() {
    return width.value
  },
  set width(value) {
    width.value = value
  },
  get height() {
    return height.value
  },
  set height(value) {
    height.value = value
  },
  get isBelowToolbar() {
    return isBelowToolbar.value
  },
  set isBelowToolbar(value) {
    isBelowToolbar.value = value
  },
  get types() {
    return Array.from(visualizationStore.types(props.typename))
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
  hide: () => emit('update:visible', false),
  updateType: (id) => emit('update:id', id),
})

const effectiveVisualization = computed(() => {
  if (
    vueError.value ||
    (visualizationData.value && !visualizationData.value.ok) ||
    (expressionVisualizationData.value && !expressionVisualizationData.value.ok)
  ) {
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
        @update:preprocessor="updatePreprocessor_"
      />
    </Suspense>
  </div>
</template>
