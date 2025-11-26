import type { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import type GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import type { RawDataSource } from '@/components/GraphEditor/GraphVisualization/visualizationData'
import { injectBubblingKeyboard } from '@/providers/keyboard'
import { type VisualizationDataSource } from '@/stores/visualization'
import { type Opt } from '@/util/data/opt'
import { type Rect } from '@/util/data/rect'
import { type ToValue } from '@/util/reactivity'
import { computed, ref, shallowRef, toValue, watch } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'
import type { VisualizationIdentifier, VisualizationMetadata } from 'ydoc-shared/yjsModel'

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
  nodeRect: ToValue<Rect>
  scale: ToValue<number>
  isFocused: ToValue<boolean>
  typeinfo: ToValue<Opt<TypeInfo>>
  dataSource: ToValue<Opt<VisualizationDataSource | RawDataSource>>
  hidden: ToValue<boolean>
  emit: Emit
}

/** Composable managing the state of the visualization for a node. */
export function useNodeVisualization({
  vis,
  nodeHovered,
  nodeRect,
  scale,
  isFocused,
  typeinfo,
  dataSource,
  hidden,
  emit,
}: NodeVisualizationOptions) {
  const keyboard = injectBubblingKeyboard()
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

  function hoverWithLease(baseHover: ToValue<boolean>) {
    const hoverWithLease = ref(toValue(baseHover))
    watch(
      () => toValue(baseHover),
      (immediateHovered) => {
        if (immediateHovered) hoverWithLease.value = true
        else {
          requestAnimationFrame(() => {
            hoverWithLease.value = toValue(baseHover)
          })
        }
      },
      { flush: 'post' },
    )
    return hoverWithLease
  }

  const visualizationHovered = ref(false)
  const visHoveredWithLease = hoverWithLease(visualizationHovered)
  const nodeHoveredWithLease = hoverWithLease(nodeHovered)

  const isVisualizationPreviewed = computed(
    () =>
      !isVisualizationEnabled.value &&
      keyboard.mod &&
      (visHoveredWithLease.value || nodeHoveredWithLease.value),
  )

  const isVisualizationVisible = computed(
    () => isVisualizationEnabled.value || isVisualizationPreviewed.value,
  )

  watch(isVisualizationVisible, (visible) => {
    if (!visible && visualizationHovered.value) visualizationHovered.value = false
  })

  const visRect = shallowRef<Rect>()
  const visibleVisRect = computed(
    (): Opt<Rect> => (isVisualizationVisible.value && !toValue(hidden) ? visRect.value : null),
  )
  watch(visibleVisRect, (rect) => emit('update:visualizationRect', rect ?? undefined))

  const visualization = computed((): ComponentProps<typeof GraphVisualization> => {
    const { size: nodeSize, pos: nodePosition } = toValue(nodeRect)
    return {
      show: isVisualizationVisible.value,
      width: visualizationWidth.value,
      nodeSize,
      scale: toValue(scale),
      nodePosition,
      currentType: metadata.value?.identifier,
      dataSource: toValue(dataSource) ?? undefined,
      typename: toValue(typeinfo)?.primaryType ?? undefined,
      typeinfo: toValue(typeinfo) ?? undefined,
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
    visRect: visibleVisRect,
    visualization,
  }
}
