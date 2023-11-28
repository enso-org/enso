<script setup lang="ts">
import LoadingErrorVisualization from '@/components/visualizations/LoadingErrorVisualization.vue'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { useProjectStore } from '@/stores/project'
import {
  DEFAULT_VISUALIZATION_CONFIGURATION,
  DEFAULT_VISUALIZATION_IDENTIFIER,
  useVisualizationStore,
  type Visualization,
} from '@/stores/visualization'
import type { URLString } from '@/stores/visualization/compilerMessaging'
import { toError } from '@/util/error'
import type { Icon } from '@/util/iconName'
import type { Opt } from '@/util/opt'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import type { ExprId, VisualizationIdentifier } from 'shared/yjsModel'
import { computed, onErrorCaptured, onUnmounted, ref, shallowRef, watch, watchEffect } from 'vue'

const visPreprocessor = ref(DEFAULT_VISUALIZATION_CONFIGURATION)
const error = ref<Error>()

const TOP_WITHOUT_TOOLBAR_PX = 36
const TOP_WITH_TOOLBAR_PX = 72

const projectStore = useProjectStore()
const visualizationStore = useVisualizationStore()

const props = defineProps<{
  currentType: Opt<VisualizationIdentifier>
  isCircularMenuVisible: boolean
  nodePosition: Vec2
  nodeSize: Vec2
  typename?: string | undefined
  expressionId?: ExprId | undefined
  data?: any | undefined
}>()
const emit = defineEmits<{
  'update:rect': [rect: Rect | undefined]
  setVisualizationId: [id: VisualizationIdentifier]
  setVisualizationVisible: [visible: boolean]
}>()

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
    ? { name: props.currentType?.name, error: error.value }
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
  () => [props.currentType, visualization.value],
  () => (error.value = undefined),
)

watchEffect(async () => {
  if (props.currentType == null) return
  visualization.value = undefined
  icon.value = undefined
  try {
    const module = await visualizationStore.get(props.currentType).value
    if (module) {
      if (module.defaultPreprocessor != null) {
        updatePreprocessor(...module.defaultPreprocessor)
      } else {
        switchToDefaultPreprocessor()
      }
      visualization.value = module.default
      icon.value = module.icon
    } else {
      switch (props.currentType.module.kind) {
        case 'Builtin': {
          error.value = new Error(
            `The builtin visualization '${props.currentType.name}' was not found.`,
          )
          break
        }
        case 'CurrentProject': {
          error.value = new Error(
            `The visualization '${props.currentType.name}' was not found in the current project.`,
          )
          break
        }
        case 'Library': {
          error.value = new Error(
            `The visualization '${props.currentType.name}' was not found in the library '${props.currentType.module.name}'.`,
          )
          break
        }
      }
    }
  } catch (caughtError) {
    error.value = toError(caughtError)
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
    return visualizationStore.types(props.typename)
  },
  get isCircularMenuVisible() {
    return props.isCircularMenuVisible
  },
  get nodeSize() {
    return props.nodeSize
  },
  get currentType() {
    return props.currentType ?? DEFAULT_VISUALIZATION_IDENTIFIER
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
