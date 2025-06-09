import type GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import { type RawDataSource } from '@/components/GraphEditor/GraphVisualization/visualizationData'
import { injectKeyboard } from '@/providers/keyboard'
import { type VisualizationDataSource } from '@/stores/visualization'
import { type Opt } from '@/util/data/opt'
import { type Rect } from '@/util/data/rect'
import { type ToValue } from '@/util/reactivity'
import { computed, ref, shallowRef, toValue, watch } from 'vue'
import { type ComponentProps } from 'vue-component-type-helpers'
import { type VisualizationIdentifier, type VisualizationMetadata } from 'ydoc-shared/yjsModel'

interface Emit {
  (event: 'update:visualizationWidth', width: number): void
  (event: 'update:visualizationEnabled', enabled: boolean): void
  (event: 'update:visualizationRect', rect: Rect | undefined): void
  (event: 'update:visualizationId', id: Opt<VisualizationIdentifier>): void
  (event: 'update:visualizationEnabled', enabled: boolean): void
  (event: 'update:visualizationHeight', height: number): void
}

interface NodeVisualizationOptions {
  vis: ToValue<Opt<VisualizationMetadata>>
  nodeHovered: ToValue<boolean>
  isComponentMenuVisible: ToValue<boolean>
  nodeRect: ToValue<Rect>
  scale: ToValue<number>
  isFocused: ToValue<boolean>
  typename: ToValue<Opt<string>>
  dataSource: ToValue<Opt<VisualizationDataSource | RawDataSource>>
  emit: Emit
}

/** Composable managing the state of the visualization for a node. */
export function useNodeVisualization({
  vis,
  nodeHovered,
  isComponentMenuVisible,
  nodeRect,
  scale,
  isFocused,
  typename,
  dataSource,
  emit,
}: NodeVisualizationOptions) {
  const keyboard = injectKeyboard()
  const metadata = computed(() => toValue(vis))
  const visualizationWidth = computed<number | null>({
    get: () => metadata.value?.width ?? null,
    set: (value) => value && emit('update:visualizationWidth', value),
  })
  const visualizationHeight = computed<number | null>(() => metadata.value?.height ?? null)
  const isVisualizationEnabled = computed<boolean>({
    get: () => metadata.value?.visible ?? false,
    set: (value) => emit('update:visualizationEnabled', value),
  })
  const visualizationHovered = ref(false)

  const isVisualizationPreviewed = computed(
    () =>
      !isVisualizationEnabled.value &&
      keyboard.mod &&
      (visualizationHovered.value || toValue(nodeHovered)),
  )
  const isVisualizationVisible = computed(
    () => isVisualizationEnabled.value || isVisualizationPreviewed.value,
  )
  watch(isVisualizationVisible, (val) => {
    // When visualization is being hidden, we don’t receive `pointerleave` event for some reason.
    // So we need to set `visualizationHovered` to `false` manually.
    if (!val) {
      visualizationHovered.value = false
    }
  })

  const visRect = shallowRef<Rect>()
  watch(visRect, (rect) => emit('update:visualizationRect', rect))

  const visualization = computed((): ComponentProps<typeof GraphVisualization> => {
    const { size: nodeSize, pos: nodePosition } = toValue(nodeRect)
    return {
      width: visualizationWidth.value,
      nodeSize,
      scale: toValue(scale),
      nodePosition,
      isComponentMenuVisible: toValue(isComponentMenuVisible),
      currentType: metadata.value?.identifier,
      dataSource: toValue(dataSource) ?? undefined,
      typename: toValue(typename) ?? undefined,
      height: visualizationHeight.value,
      isFocused: toValue(isFocused),
      isPreview: isVisualizationPreviewed.value,
      isFullscreenAllowed: true,
      isResizable: true,
      'onUpdate:hovered': (event) => (visualizationHovered.value = event),
      'onUpdate:rect': (event) => (visRect.value = event),
      'onUpdate:id': (event) => emit('update:visualizationId', event),
      'onUpdate:enabled': (event) => emit('update:visualizationEnabled', event),
      'onUpdate:height': (event) => emit('update:visualizationHeight', event),
      'onUpdate:width': (event) => (visualizationWidth.value = event),
    }
  })

  return {
    visualizationWidth,
    isVisualizationEnabled,
    isVisualizationPreviewed,
    visRect: computed((): Opt<Rect> => (isVisualizationVisible.value ? visRect.value : null)),
    visualization: computed(() => (isVisualizationVisible.value ? visualization.value : null)),
  }
}
