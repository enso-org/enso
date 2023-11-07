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
import { toError } from '@/util/error'
import type { Opt } from '@/util/opt'
import type { Vec2 } from '@/util/vec2'
import type { ExprId, VisualizationIdentifier } from 'shared/yjsModel'
import { computed, onErrorCaptured, ref, shallowRef, watch, watchEffect } from 'vue'

const visPreprocessor = ref(DEFAULT_VISUALIZATION_CONFIGURATION)
const error = ref<Error>()

const projectStore = useProjectStore()
const visualizationStore = useVisualizationStore()

const props = defineProps<{
  currentType: Opt<VisualizationIdentifier>
  isCircularMenuVisible: boolean
  nodeSize: Vec2
  typename?: string | undefined
  expressionId?: ExprId | undefined
  data?: any | undefined
}>()
const emit = defineEmits<{
  setVisualizationId: [id: VisualizationIdentifier]
  setVisualizationVisible: [visible: boolean]
}>()

const visualization = shallowRef<Visualization>()

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
  try {
    const module = await visualizationStore.get(props.currentType).value
    if (module) {
      if (module.defaultPreprocessor != null) {
        updatePreprocessor(...module.defaultPreprocessor)
      } else {
        switchToDefaultPreprocessor()
      }
      visualization.value = module.default
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
    return props.currentType ?? DEFAULT_VISUALIZATION_IDENTIFIER
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
