<script setup lang="ts">
import LoadingErrorVisualization from '@/components/visualizations/LoadingErrorVisualization.vue'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { useProjectStore, type NodeVisualizationConfiguration } from '@/stores/project'
import {
  DEFAULT_VISUALIZATION_CONFIGURATION,
  DEFAULT_VISUALIZATION_IDENTIFIER,
  useVisualizationStore,
  type VisualizationDataSource,
} from '@/stores/visualization'
import type { Visualization } from '@/stores/visualization/runtimeTypes'
import { Ast } from '@/util/ast'
import { toError } from '@/util/data/error'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import type { Result } from '@/util/data/result'
import type { URLString } from '@/util/data/urlString'
import { Vec2 } from '@/util/data/vec2'
import type { Icon } from '@/util/iconName'
import { computedAsync } from '@vueuse/core'
import { isIdentifier } from 'shared/ast'
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

// Used for testing.
type RawDataSource = { type: 'raw'; data: any }

const props = defineProps<{
  currentType?: Opt<VisualizationIdentifier>
  isCircularMenuVisible: boolean
  nodePosition: Vec2
  nodeSize: Vec2
  scale: number
  typename?: string | undefined
  dataSource: VisualizationDataSource | RawDataSource | undefined
}>()
const emit = defineEmits<{
  'update:rect': [rect: Rect | undefined]
  'update:id': [id: VisualizationIdentifier]
  'update:visible': [visible: boolean]
}>()

const visPreprocessor = ref(DEFAULT_VISUALIZATION_CONFIGURATION)
const vueError = ref<Error>()

const projectStore = useProjectStore()
const visualizationStore = useVisualizationStore()

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

const defaultVisualizationForCurrentNodeSource = computed<VisualizationIdentifier | undefined>(
  () => {
    const raw = defaultVisualizationRaw.value
    if (!raw?.ok || !raw.value) return
    return {
      name: raw.value.name,
      module:
        raw.value.library == null
          ? { kind: 'Builtin' }
          : { kind: 'Library', name: raw.value.library.name },
    }
  },
)

const currentType = computed(() => {
  if (props.currentType) return props.currentType
  if (defaultVisualizationForCurrentNodeSource.value)
    return defaultVisualizationForCurrentNodeSource.value
  const [id] = visualizationStore.types(props.typename)
  return id
})

const visualization = shallowRef<Visualization>()
const icon = ref<Icon | URLString>()

onErrorCaptured((error) => {
  vueError.value = error
  return false
})

const nodeVisualizationData = projectStore.useVisualizationData(() => {
  if (props.dataSource?.type !== 'node') return
  return {
    ...visPreprocessor.value,
    expressionId: props.dataSource.nodeId,
  }
})

const expressionVisualizationData = computedAsync(() => {
  if (props.dataSource?.type !== 'expression') return
  if (preprocessorLoading.value) return
  const preprocessor = visPreprocessor.value
  const args = preprocessor.positionalArgumentsExpressions
  const tempModule = Ast.MutableModule.Transient()
  const preprocessorModule = Ast.parse(preprocessor.visualizationModule, tempModule)
  // TODO[ao]: it work with builtin visualization, but does not work in general case.
  // Tracked in https://github.com/orgs/enso-org/discussions/6832#discussioncomment-7754474.
  if (!isIdentifier(preprocessor.expression)) {
    console.error(`Unsupported visualization preprocessor definition`, preprocessor)
    return
  }
  const preprocessorQn = Ast.PropertyAccess.new(
    tempModule,
    preprocessorModule,
    preprocessor.expression,
  )
  const preprocessorInvocation = Ast.App.PositionalSequence(preprocessorQn, [
    Ast.Wildcard.new(tempModule),
    ...args.map((arg) => Ast.Group.new(tempModule, Ast.parse(arg, tempModule))),
  ])
  const rhs = Ast.parse(props.dataSource.expression, tempModule)
  const expression = Ast.OprApp.new(tempModule, preprocessorInvocation, '<|', rhs)
  return projectStore.executeExpression(props.dataSource.contextId, expression.code())
})

const effectiveVisualizationData = computed(() => {
  const name = currentType.value?.name
  if (props.dataSource?.type === 'raw') return props.dataSource.data
  if (vueError.value) return { name, error: vueError.value }
  const visualizationData = nodeVisualizationData.value ?? expressionVisualizationData.value
  if (!visualizationData) return
  if (visualizationData.ok) return visualizationData.value
  else return { name, error: new Error(visualizationData.error.payload) }
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

// Flag used to prevent rendering the visualization with a stale preprocessor while the new preprocessor is being
// prepared asynchronously.
const preprocessorLoading = ref(false)
watchEffect(async () => {
  preprocessorLoading.value = true
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
  preprocessorLoading.value = false
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
    (nodeVisualizationData.value && !nodeVisualizationData.value.ok) ||
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
