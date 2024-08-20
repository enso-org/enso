<script setup lang="ts">
import { visualizationBindings } from '@/bindings'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import LoadingErrorVisualization from '@/components/visualizations/LoadingErrorVisualization.vue'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import { focusIsIn, useEvent } from '@/composables/events'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { useProjectStore } from '@/stores/project'
import { type NodeVisualizationConfiguration } from '@/stores/project/executionContext'
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
import {
  computed,
  nextTick,
  onErrorCaptured,
  onUnmounted,
  ref,
  shallowRef,
  watch,
  watchEffect,
  type ShallowRef,
} from 'vue'
import { isIdentifier } from 'ydoc-shared/ast'
import { visIdentifierEquals, type VisualizationIdentifier } from 'ydoc-shared/yjsModel'

/** The minimum width must be at least the total width of:
 * - both of toolbars that are always visible (32px + 60px), and
 * - the 4px flex gap between the toolbars. */
const MIN_WIDTH_PX = 200
const MIN_CONTENT_HEIGHT_PX = 32
const DEFAULT_CONTENT_HEIGHT_PX = 150
const TOOLBAR_HEIGHT_PX = 36

// Used for testing.
type RawDataSource = { type: 'raw'; data: any }

const props = defineProps<{
  currentType?: Opt<VisualizationIdentifier>
  isCircularMenuVisible: boolean
  isPreview?: boolean
  nodePosition: Vec2
  nodeSize: Vec2
  width: Opt<number>
  height: Opt<number>
  scale: number
  isFocused: boolean
  isFullscreen: boolean
  typename?: string | undefined
  dataSource: VisualizationDataSource | RawDataSource | undefined
}>()
const emit = defineEmits<{
  'update:rect': [rect: Rect | undefined]
  'update:id': [id: VisualizationIdentifier]
  'update:enabled': [visible: boolean]
  'update:fullscreen': [fullscreen: boolean]
  'update:width': [width: number]
  'update:height': [height: number]
  'update:nodePosition': [pos: Vec2]
  createNodes: [options: NodeCreationOptions[]]
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
    if (!raw?.ok || !raw.value || !raw.value.name) return
    return {
      name: raw.value.name,
      module:
        raw.value.library == null ?
          { kind: 'Builtin' }
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
  else return { name, error: new Error(`${visualizationData.error.payload}`) }
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

const toolbarHeight = computed(() => (isBelowToolbar.value ? TOOLBAR_HEIGHT_PX : 0))

const rect = computed(
  () =>
    new Rect(
      props.nodePosition,
      new Vec2(
        Math.max(props.width ?? MIN_WIDTH_PX, props.nodeSize.x),
        Math.max(props.height ?? DEFAULT_CONTENT_HEIGHT_PX, MIN_CONTENT_HEIGHT_PX) +
          toolbarHeight.value +
          props.nodeSize.y,
      ),
    ),
)

watchEffect(() => emit('update:rect', rect.value))
onUnmounted(() => emit('update:rect', undefined))

const allTypes = computed(() => Array.from(visualizationStore.types(props.typename)))

provideVisualizationConfig({
  get isFocused() {
    return props.isFocused
  },
  get fullscreen() {
    return props.isFullscreen
  },
  set fullscreen(value) {
    emit('update:fullscreen', value)
  },
  get scale() {
    return props.scale
  },
  get width() {
    return rect.value.width
  },
  set width(value) {
    emit('update:width', value)
  },
  get height() {
    return rect.value.height - toolbarHeight.value - props.nodeSize.y
  },
  set height(value) {
    emit('update:height', value)
  },
  get nodePosition() {
    return props.nodePosition
  },
  set nodePosition(value) {
    emit('update:nodePosition', value)
  },
  get isBelowToolbar() {
    return isBelowToolbar.value
  },
  set isBelowToolbar(value) {
    isBelowToolbar.value = value
  },
  get types() {
    return allTypes.value
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
  get nodeType() {
    return props.typename
  },
  get isPreview() {
    return props.isPreview ?? false
  },
  hide: () => emit('update:enabled', false),
  updateType: (id) => emit('update:id', id),
  createNodes: (...options) => emit('createNodes', options),
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

const root = ref<HTMLElement>()

const keydownHandler = visualizationBindings.handler({
  nextType: () => {
    if (props.isFocused || focusIsIn(root.value)) {
      const currentIndex = allTypes.value.findIndex((type) =>
        visIdentifierEquals(type, currentType.value),
      )
      const nextIndex = (currentIndex + 1) % allTypes.value.length
      emit('update:id', allTypes.value[nextIndex]!)
    } else {
      return false
    }
  },
  toggleFullscreen: () => {
    if (props.isFocused || focusIsIn(root.value)) {
      emit('update:fullscreen', !props.isFullscreen)
    } else {
      return false
    }
  },
  exitFullscreen: () => {
    if (props.isFullscreen) {
      emit('update:fullscreen', false)
    } else {
      return false
    }
  },
})

useEvent(window, 'keydown', keydownHandler)

watch(
  () => props.isFullscreen,
  (f) => {
    f && nextTick(() => root.value?.focus())
  },
)
</script>

<template>
  <div ref="root" class="GraphVisualization" tabindex="-1">
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

<style scoped>
.GraphVisualization {
  /** Prevent drawing on top of other UI elements (e.g. dropdown widgets). */
  isolation: isolate;
}
</style>
