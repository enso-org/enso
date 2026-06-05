import type { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import { type ToValue } from '$/utils/reactivity'
import type GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import type { RawDataSource } from '@/components/GraphEditor/GraphVisualization/visualizationData'
import { injectBubblingKeyboard } from '@/providers/keyboard'
import { type VisualizationDataSource } from '@/stores/visualization'
import { type Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref, toValue, watch } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'
import type { VisualizationIdentifier, VisualizationMetadata } from 'ydoc-shared/yjsModel'

interface Emit {
  (event: 'update:visualizationWidth', width: number): void
  (event: 'update:visualizationEnabled', enabled: boolean): void
  (event: 'update:visualizationId', id: Opt<VisualizationIdentifier>): void
  (event: 'update:visualizationEnabled', enabled: boolean): void
  (event: 'update:visualizationHeight', height: number): void
}

interface NodeVisualizationOptions {
  vis: ToValue<Opt<VisualizationMetadata>>
  nodeHovered: ToValue<boolean>
  nodeWidgetsSize: ToValue<Vec2>
  nodePos: ToValue<Vec2>
  scale: ToValue<number>
  isFocused: ToValue<boolean>
  typeinfo: ToValue<Opt<TypeInfo>>
  dataSource: ToValue<Opt<VisualizationDataSource | RawDataSource>>
  hidden: ToValue<boolean>
  emit: Emit
  onResize?: (rect0: Rect, rect1: Rect) => void
}

/** Composable managing the state of the visualization for a node. */
export function useNodeVisualization({
  vis,
  nodeHovered,
  nodeWidgetsSize,
  nodePos,
  scale,
  isFocused,
  typeinfo,
  dataSource,
  hidden,
  emit,
  onResize,
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

  const effectiveHeight = ref<number>()
  const visibleVisHeight = computed((): number => {
    if (!isVisualizationVisible.value || toValue(hidden)) return 0
    return effectiveHeight.value ?? 0
  })
  const effectiveWidth = ref<number>()
  const visibleVisWidth = computed((): number => {
    if (!isVisualizationVisible.value || toValue(hidden)) return 0
    return effectiveWidth.value ?? 0
  })
  const visibleSize = computed<Vec2 | undefined>((prev) => {
    if (effectiveHeight.value == null || effectiveWidth.value == null) return
    const size = new Vec2(visibleVisWidth.value, visibleVisHeight.value)
    return prev?.equals(size) ? prev : size
  })
  let resizing = false
  watch(visibleSize, (size1, size0) => {
    if (!resizing || !size1 || !size0 || size1.equals(size0)) return
    const widgets = toValue(nodeWidgetsSize)
    const pos = toValue(nodePos)
    const fullSize = (vizSize: Vec2) =>
      new Vec2(Math.max(widgets.x, vizSize.x), widgets.y + vizSize.y)
    const rect0 = new Rect(pos, fullSize(size0))
    const rect1 = new Rect(pos, fullSize(size1))
    onResize?.(rect0, rect1)
  })

  const visualization = computed((): ComponentProps<typeof GraphVisualization> => {
    return {
      show: isVisualizationVisible.value,
      width: visualizationWidth.value,
      nodeSize: toValue(nodeWidgetsSize),
      scale: toValue(scale),
      nodePosition: toValue(nodePos),
      currentType: metadata.value?.identifier,
      dataSource: toValue(dataSource) ?? undefined,
      typeinfo: toValue(typeinfo) ?? undefined,
      height: visualizationHeight.value,
      isFocused: toValue(isFocused),
      isPreview: isVisualizationPreviewed.value,
      isFullscreenAllowed: true,
      isResizable: true,
      'onUpdate:hovered': (event) => (visualizationHovered.value = event),
      'onUpdate:effectiveHeight': (event) => (effectiveHeight.value = event),
      'onUpdate:effectiveWidth': (event) => (effectiveWidth.value = event),
      'onUpdate:id': (event) => emit('update:visualizationId', event),
      'onUpdate:enabled': (event) => emit('update:visualizationEnabled', event),
      'onUpdate:height': (event) => emit('update:visualizationHeight', event),
      'onUpdate:width': (event) => (visualizationWidth.value = event),
      'onUpdate:resizing': (event) => (resizing = event),
    }
  })

  return {
    visualizationWidth: visibleVisWidth,
    isVisualizationEnabled,
    isVisualizationPreviewed,
    vizHeight: visibleVisHeight,
    visualization,
  }
}
