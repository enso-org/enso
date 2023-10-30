<script setup lang="ts">
import ErrorLoadingVisualizationVisualization from '@/components/visualizations/ErrorLoadingVisualizationVisualization.vue'
import LoadingVisualization from '@/components/visualizations/LoadingVisualization.vue'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { useProjectStore } from '@/stores/project'
import {
  DEFAULT_VISUALIZATION_CONFIGURATION,
  DEFAULT_VISUALIZATION_IDENTIFIER,
  useVisualizationStore,
  type Visualization,
} from '@/stores/visualization'
import type { Opt } from '@/util/opt'
import type { Vec2 } from '@/util/vec2'
import type { ExprId, VisualizationIdentifier } from 'shared/yjsModel'
import { computed, onErrorCaptured, ref, shallowRef, watch, watchEffect } from 'vue'

const visPreprocessor = ref(DEFAULT_VISUALIZATION_CONFIGURATION)
const errored = ref(false)

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

onErrorCaptured(() => {
  errored.value = true
})

const visualizationData = projectStore.useVisualizationData(() => {
  return props.data == null && props.expressionId != null
    ? {
        ...visPreprocessor.value,
        expressionId: props.expressionId,
      }
    : null
})

const effectiveVisualizationData = computed(() => props.data ?? visualizationData.value)

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
  () => props.currentType,
  () => {
    errored.value = false
  },
)

watchEffect(async () => {
  if (props.currentType == null) {
    return
  }

  visualization.value = undefined
  const module = await visualizationStore.get(props.currentType).value
  if (module) {
    if (module.defaultPreprocessor != null) {
      updatePreprocessor(...module.defaultPreprocessor)
    } else {
      switchToDefaultPreprocessor()
    }
    visualization.value = module.default
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
  if (errored.value) {
    return ErrorLoadingVisualizationVisualization
  }
  if (!visualization.value || effectiveVisualizationData.value == null) {
    return LoadingVisualization
  }
  return visualization.value
})
</script>

<template>
  <div class="GraphVisualization">
    <component
      :is="effectiveVisualization"
      :data="effectiveVisualizationData"
      @update:preprocessor="updatePreprocessor"
    />
  </div>
</template>
