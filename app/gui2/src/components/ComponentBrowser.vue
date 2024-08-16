<script setup lang="ts">
import { componentBrowserBindings } from '@/bindings'
import { makeComponentList, type Component } from '@/components/ComponentBrowser/component'
import ComponentEditor from '@/components/ComponentBrowser/ComponentEditor.vue'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import { useComponentBrowserInput, type Usage } from '@/components/ComponentBrowser/input'
import { useScrolling } from '@/components/ComponentBrowser/scrolling'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useApproach } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import type { useNavigator } from '@/composables/navigator'
import { groupColorStyle } from '@/composables/nodeColors'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { SuggestionKind, type Typename } from '@/stores/suggestionDatabase/entry'
import type { VisualizationDataSource } from '@/stores/visualization'
import { endOnClick, isNodeOutside } from '@/util/autoBlur'
import { tryGetIndex } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { allRanges } from '@/util/data/range'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { DEFAULT_ICON, suggestionEntryToIcon } from '@/util/getIconName'
import { debouncedGetter } from '@/util/reactivity'
import type { ComponentInstance, Ref } from 'vue'
import { computed, onMounted, onUnmounted, reactive, ref, watch, watchEffect } from 'vue'
import type { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'
import LoadingSpinner from './LoadingSpinner.vue'

const ITEM_SIZE = 32
const TOP_BAR_HEIGHT = 32
// Difference in position between the component browser and a node for the input of the component browser to
// be placed at the same position as the node.
const COMPONENT_BROWSER_TO_NODE_OFFSET = new Vec2(-4, -4)
const WIDTH = 600
const INPUT_AREA_HEIGHT = 40
const PANELS_HEIGHT = 384
// Height of the visualization area, starting from the bottom of the input area.
const VISUALIZATION_HEIGHT = 190
const PAN_MARGINS = {
  top: 48,
  bottom: 40,
  left: 80,
  right: 40,
}
const COMPONENT_EDITOR_PADDING = 12
const ICON_WIDTH = 16

const cssComponentEditorPadding = `${COMPONENT_EDITOR_PADDING}px`

const projectStore = useProjectStore()
const suggestionDbStore = useSuggestionDbStore()
const graphStore = useGraphStore()
const interaction = injectInteractionHandler()

const props = defineProps<{
  nodePosition: Vec2
  navigator: ReturnType<typeof useNavigator>
  usage: Usage
  closeIfClicked: (e: PointerEvent) => boolean
}>()

const emit = defineEmits<{
  accepted: [
    searcherExpression: string,
    requiredImports: RequiredImport[],
    firstAppliedReturnType: Typename | undefined,
  ]
  canceled: [],
  selectedSuggestionId: [id: SuggestionId | null],
  isAiPrompt: [boolean],
}>()

const cbRoot = ref<HTMLElement>()

defineExpose({ cbRoot })

const cbOpen: Interaction = endOnClick(props.closeIfClicked, {
  cancel: () => emit('canceled'),
  end: () => {
    // In AI prompt mode likely the input is not a valid mode.
    if (input.anyChange.value && !input.isAiPrompt.value) {
      acceptInput()
    } else {
      emit('canceled')
    }
  },
})

function scaleValues<T extends Record<any, number>>(
  values: T,
  scale: number,
): { [Key in keyof T]: number } {
  return Object.fromEntries(
    Object.entries(values).map(([key, value]) => [key, value * scale]),
  ) as any
}

const clientToSceneFactor = computed(() => 1 / props.navigator.targetScale)

const originScenePos = computed(() => {
  return props.nodePosition.add(COMPONENT_BROWSER_TO_NODE_OFFSET.scale(clientToSceneFactor.value))
})

function panIntoView() {
  const origin = originScenePos.value
  const inputArea = new Rect(
    origin,
    new Vec2(WIDTH, INPUT_AREA_HEIGHT).scale(clientToSceneFactor.value),
  )
  const panelsAreaDimensions = new Vec2(WIDTH, PANELS_HEIGHT).scale(clientToSceneFactor.value)
  const panelsArea = new Rect(origin.sub(new Vec2(0, panelsAreaDimensions.y)), panelsAreaDimensions)
  const vizHeight = VISUALIZATION_HEIGHT * clientToSceneFactor.value
  const margins = scaleValues(PAN_MARGINS, clientToSceneFactor.value)
  props.navigator.panTo([
    // Always include the bottom-left of the input area.
    { x: inputArea.left, y: inputArea.bottom },
    // Try to reach the top-right corner of the panels.
    { x: inputArea.right, y: panelsArea.top },
    // Extend down to include the visualization.
    { y: inputArea.bottom + vizHeight },
    // Top (and left) margins are more important than bottom (and right) margins because the screen has controls across
    // the top and on the left.
    { x: inputArea.left - margins.left, y: panelsArea.top - margins.top },
    // If the screen is very spacious, even the bottom right gets some breathing room.
    { x: inputArea.right + margins.right, y: inputArea.bottom + vizHeight + margins.bottom },
  ])
}

onMounted(() => {
  interaction.setCurrent(cbOpen)
  input.reset(props.usage)
  inputElement.value?.focus()
  panIntoView()
})

// === Position ===

const transform = computed(() => {
  const nav = props.navigator
  const translate = nav.translate
  const position = props.nodePosition.add(
    COMPONENT_BROWSER_TO_NODE_OFFSET.scale(clientToSceneFactor.value),
  )
  const screenPosition = translate.add(position).scale(nav.scale)
  const x = Math.round(screenPosition.x)
  const y = Math.round(screenPosition.y)

  return `translate(${x}px, ${y}px) translateY(-100%)`
})

// === Input and Filtering ===

const input = useComponentBrowserInput()
const filterFlags = ref({ showUnstable: false, showLocal: false })

const currentFiltering = computed(() => {
  const currentModule = projectStore.modulePath
  return new Filtering(
    {
      ...input.filter.value,
      ...filterFlags.value,
    },
    currentModule?.ok ? currentModule.value : undefined,
  )
})

watch(currentFiltering, () => {
  selected.value = input.autoSelectFirstComponent.value ? 0 : null
  scrolling.targetScroll.value = { type: 'bottom' }

  // Update `highlightPosition` synchronously, so the subsequent animation `skip` have an effect.
  if (selectedPosition.value != null) {
    highlightPosition.value = selectedPosition.value
  }
  animatedHighlightPosition.skip()
  animatedHighlightHeight.skip()
})

onUnmounted(() => {
  graphStore.cbEditedEdge = undefined
})

// Compute edge, except for the color. The color is set in a separate watch, as it changes more often.
watchEffect(() => {
  const sourceIdent = input.selfArgument.value
  const sourceNode =
    sourceIdent != null ? graphStore.db.getIdentDefiningNode(sourceIdent) : undefined
  const source = graphStore.db.getNodeFirstOutputPort(sourceNode)
  if (!source) {
    graphStore.cbEditedEdge = undefined
    return
  }
  const scenePos = originScenePos.value.add(
    new Vec2(COMPONENT_EDITOR_PADDING + ICON_WIDTH / 2, 0).scale(clientToSceneFactor.value),
  )
  graphStore.cbEditedEdge = {
    source,
    target: undefined,
    anchor: { type: 'fixed', scenePos },
  }
})

function handleDefocus(e: FocusEvent) {
  const stillInside = !isNodeOutside(e.relatedTarget, cbRoot.value)
  // We want to focus input even when relatedTarget == null, because sometimes defocus event is
  // caused by focused item being removed, for example an entry in visualization chooser.
  if (stillInside || e.relatedTarget == null) {
    inputElement.value?.focus()
  }
}

const inputElement = ref<ComponentInstance<typeof ComponentEditor>>()
const inputSize = useResizeObserver(inputElement, false)

// === Components List and Positions ===

const components = computed(() =>
  makeComponentList(suggestionDbStore.entries, currentFiltering.value),
)

const visibleComponents = computed(() => {
  if (scroller.value == null) return []
  const scrollPos = scrolling.scrollPosition.value
  const topmostVisible = componentAtY(scrollPos)
  const bottommostVisible = Math.max(0, componentAtY(scrollPos + scrollerSize.value.y))
  return components.value.slice(bottommostVisible, topmostVisible + 1).map((component, i) => {
    return { component, index: i + bottommostVisible }
  })
})

function componentPos(index: number) {
  return listContentHeight.value - (index + 1) * ITEM_SIZE
}

function componentAtY(pos: number) {
  return Math.floor((listContentHeight.value - pos) / ITEM_SIZE)
}

function componentStyle(index: number) {
  return { transform: `translateY(${componentPos(index)}px)` }
}

/**
 * Group colors are populated in `GraphEditor`, and for each group in suggestion database a CSS variable is created.
 */
function componentColor(component: Component): string {
  return groupColorStyle(tryGetIndex(suggestionDbStore.groups, component.group))
}

// === Highlight ===

const selected = ref<number | null>(null)
const highlightPosition = ref(0)
const selectedPosition = computed(() =>
  selected.value != null ? componentPos(selected.value) : null,
)
const highlightHeight = computed(() => (selected.value != null ? ITEM_SIZE : 0))
const animatedHighlightPosition = useApproach(highlightPosition)
const animatedHighlightHeight = useApproach(highlightHeight)

const selectedSuggestionId = computed(() => {
  if (selected.value === null) return null
  return components.value[selected.value]?.suggestionId ?? null
})

const selectedSuggestion = computed(() => {
  const id = selectedSuggestionId.value
  if (id == null) return null
  return suggestionDbStore.entries.get(id) ?? null
})

const { getNodeColor } = injectNodeColors()
const nodeColor = computed(() => {
  if (props.usage.type === 'editNode') {
    const override = graphStore.db.nodeIdToNode.get(props.usage.node)?.colorOverride
    if (override) return override
  }
  if (selectedSuggestion.value?.groupIndex != null)
    return groupColorStyle(
      tryGetIndex(suggestionDbStore.groups, selectedSuggestion.value.groupIndex),
    )
  if (props.usage.type === 'editNode') {
    const color = getNodeColor(props.usage.node)
    if (color) return color
  }
  return 'var(--node-color-no-type)'
})
watchEffect(() => {
  if (!graphStore.cbEditedEdge) return
  graphStore.cbEditedEdge.color = nodeColor.value
})

const selectedSuggestionIcon = computed(() => {
  if (!input.selfArgument.value) return undefined
  return selectedSuggestion.value ? suggestionEntryToIcon(selectedSuggestion.value) : DEFAULT_ICON
})

watch(selectedPosition, (newPos) => {
  if (newPos == null) return
  highlightPosition.value = newPos
})

const highlightClipPath = computed(() => {
  let height = animatedHighlightHeight.value
  let position = animatedHighlightPosition.value
  let top = position + ITEM_SIZE - height
  let bottom = listContentHeight.value - position - ITEM_SIZE
  return `inset(${top}px 0px ${bottom}px 0px round 16px)`
})

function selectWithoutScrolling(index: number) {
  const scrollPos = scrolling.scrollPosition.value
  scrolling.targetScroll.value = { type: 'offset', offset: scrollPos }
  selected.value = index
}

// === Preview ===

type PreviewState = { expression: string; suggestionId?: SuggestionId }
const previewed = debouncedGetter<PreviewState>(() => {
  if (selectedSuggestionId.value == null || selectedSuggestion.value == null) {
    return { expression: input.code.value }
  } else {
    return {
      expression: input.inputAfterApplyingSuggestion(selectedSuggestion.value).newCode,
      suggestionId: selectedSuggestionId.value,
    }
  }
}, 200)

const previewedSuggestionReturnType = computed(() => {
  const id = previewed.value.suggestionId
  if (id == null) return
  return suggestionDbStore.entries.get(id)?.returnType
})

const previewDataSource = computed<VisualizationDataSource | undefined>(() => {
  if (input.isAiPrompt.value) return
  if (!previewed.value.expression.trim()) return
  if (!graphStore.methodAst.ok) return
  const body = graphStore.methodAst.value.body
  if (!body) return

  return {
    type: 'expression',
    expression: previewed.value.expression,
    contextId: body.externalId,
  }
})

const visualizationSelections = reactive(new Map<SuggestionId | null, VisualizationIdentifier>())
const previewedVisualizationId = computed(() => {
  return visualizationSelections.get(previewed.value.suggestionId ?? null)
})

function setVisualization(visualization: VisualizationIdentifier) {
  visualizationSelections.set(previewed.value.suggestionId ?? null, visualization)
}

// === Scrolling ===

const scroller = ref<HTMLElement>()
const scrollerSize = useResizeObserver(scroller)
const listContentHeight = computed(() =>
  // We add a top padding of TOP_BAR_HEIGHT / 2 - otherwise the topmost entry would be covered
  // by top bar.
  Math.max(components.value.length * ITEM_SIZE + TOP_BAR_HEIGHT / 2, scrollerSize.value.y),
)
const scrolling = useScrolling(
  animatedHighlightPosition,
  computed(() => scrollerSize.value.y),
  listContentHeight,
  ITEM_SIZE,
)

const listContentHeightPx = computed(() => `${listContentHeight.value}px`)

function updateScroll() {
  // If the scrollTop value changed significantly, that means the user is scrolling.
  if (scroller.value && Math.abs(scroller.value.scrollTop - scrolling.scrollPosition.value) > 1.0) {
    scrolling.targetScroll.value = { type: 'offset', offset: scroller.value.scrollTop }
  }
}

// === Documentation Panel ===

watch(selectedSuggestionId, (id) => emit('selectedSuggestionId', id))
watch(input.isAiPrompt, (v) => emit('isAiPrompt', v))

// === Accepting Entry ===

function applySuggestion(component: Opt<Component> = null) {
  const suggestionId = component?.suggestionId ?? selectedSuggestionId.value
  if (suggestionId == null) return
  input.applySuggestion(suggestionId)
}

function acceptSuggestion(component: Opt<Component> = null) {
  applySuggestion(component)
  const providedSuggestion =
    component != null ? suggestionDbStore.entries.get(component.suggestionId) : null
  const suggestion = providedSuggestion ?? selectedSuggestion.value
  const shouldFinish =
    component == null || (suggestion != null && suggestion.kind !== SuggestionKind.Module)
  if (shouldFinish) acceptInput()
}

function acceptInput() {
  emit(
    'accepted',
    input.code.value.trim(),
    input.importsToAdd(),
    input.firstAppliedReturnType.value,
  )
  interaction.ended(cbOpen)
}

// === Key Events Handler ===

const handler = componentBrowserBindings.handler({
  applySuggestion() {
    if (input.isAiPrompt.value) return false
    applySuggestion()
  },
  acceptSuggestion() {
    if (input.isAiPrompt.value) return false
    acceptSuggestion()
  },
  acceptInput() {
    if (input.isAiPrompt.value) return false
    acceptInput()
  },
  acceptAIPrompt() {
    if (input.isAiPrompt.value) input.applyAIPrompt()
  },
  moveUp() {
    if (selected.value != null && selected.value < components.value.length - 1) {
      selected.value += 1
    }
    scrolling.scrollWithTransition({ type: 'selected' })
  },
  moveDown() {
    if (selected.value == null) {
      selected.value = components.value.length - 1
    } else if (selected.value > 0) {
      selected.value -= 1
    }
    scrolling.scrollWithTransition({ type: 'selected' })
  },
})
</script>

<template>
  <div
    ref="cbRoot"
    class="ComponentBrowser"
    :style="{ transform, '--list-height': listContentHeightPx }"
    :data-self-argument="input.selfArgument.value"
    tabindex="-1"
    @focusout="handleDefocus"
    @keydown="handler"
    @pointerdown.stop.prevent
    @pointerup.stop.prevent
    @click.stop.prevent
    @keydown.enter.stop
    @keydown.backspace.stop
    @keydown.delete.stop
    @keydown.arrow-left.stop
    @keydown.arrow-right.stop
  >
    <div class="panels">
      <div class="panel components">
        <div class="top-bar">
          <div class="top-bar-inner">
            <ToggleIcon v-model="filterFlags.showLocal" icon="local_scope2" />
            <ToggleIcon icon="command3" />
            <ToggleIcon v-model="filterFlags.showUnstable" icon="unstable2" />
            <ToggleIcon icon="marketplace" />
          </div>
        </div>
        <div v-if="!input.isAiPrompt.value" class="components-content">
          <div
            ref="scroller"
            class="list"
            :scrollTop.prop="scrolling.scrollPosition.value"
            @wheel.stop.passive
            @scroll="updateScroll"
          >
            <div class="list-variant">
              <div
                v-for="item in visibleComponents"
                :key="item.component.suggestionId"
                class="component"
                :style="componentStyle(item.index)"
                @mousemove="selectWithoutScrolling(item.index)"
                @click="acceptSuggestion(item.component)"
              >
                <SvgIcon
                  :name="item.component.icon"
                  :style="{ color: componentColor(item.component) }"
                />
                <span>
                  <span v-if="!item.component.matchedRanges" v-text="item.component.label"></span>
                  <span
                    v-for="range in allRanges(
                      item.component.matchedRanges,
                      item.component.label.length,
                    )"
                    v-else
                    :key="`${range.start},${range.end}`"
                    class="component-label-segment"
                    :class="{ match: range.isMatch }"
                    v-text="item.component.label.slice(range.start, range.end)"
                  ></span>
                </span>
              </div>
            </div>
            <div class="list-variant selected" :style="{ clipPath: highlightClipPath }">
              <div
                v-for="item in visibleComponents"
                :key="item.component.suggestionId"
                class="component"
                :style="{
                  backgroundColor: componentColor(item.component),
                  ...componentStyle(item.index),
                }"
                @click="acceptSuggestion(item.component)"
              >
                <SvgIcon :name="item.component.icon" />
                <span>
                  <span v-if="!item.component.matchedRanges" v-text="item.component.label"></span>
                  <span
                    v-for="range in allRanges(
                      item.component.matchedRanges,
                      item.component.label.length,
                    )"
                    v-else
                    :key="`${range.start},${range.end}`"
                    class="component-label-segment"
                    :class="{ match: range.isMatch }"
                    v-text="item.component.label.slice(range.start, range.end)"
                  ></span>
                </span>
              </div>
            </div>
          </div>
        </div>
        <LoadingSpinner v-if="input.isAiPrompt.value && input.processingAIPrompt" />
      </div>
    </div>
    <div class="bottom-panel">
      <GraphVisualization
        class="visualization-preview"
        :nodeSize="inputSize"
        :nodePosition="nodePosition"
        :scale="1"
        :isCircularMenuVisible="false"
        :isFullscreen="false"
        :isFocused="true"
        :width="null"
        :height="null"
        :dataSource="previewDataSource"
        :typename="previewedSuggestionReturnType"
        :currentType="previewedVisualizationId"
        @update:id="setVisualization($event)"
      />
      <ComponentEditor
        ref="inputElement"
        v-model="input.content.value"
        :navigator="props.navigator"
        :icon="selectedSuggestionIcon"
        :nodeColor="nodeColor"
        class="component-editor"
        :style="{ '--component-editor-padding': cssComponentEditorPadding }"
      />
    </div>
  </div>
</template>

<style scoped>
.ComponentBrowser {
  --list-height: 0px;
  --radius-default: 20px;
  --background-color: #eaeaea;
  --doc-panel-bottom-clip: 4px;
  width: fit-content;
  color: rgba(0, 0, 0, 0.6);
  font-size: 11.5px;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.panels {
  display: flex;
  flex-direction: row;
  gap: 4px;
}

.panel {
  height: 380px;
  border: none;
  border-radius: var(--radius-default);
  background-color: var(--background-color);
}

.components {
  width: 200px;
  position: relative;
}

.components-content {
  position: absolute;
  top: 0px;
  padding: 4px;
  width: 100%;
  height: 100%;
}

.docs {
  width: 406px;
  clip-path: inset(0 0 0 0 round var(--radius-default));
  transition: clip-path 0.2s;
}
.docs.hidden {
  clip-path: inset(0 100% 0 0 round var(--radius-default));
}

.list {
  top: var(--radius-default);
  width: 100%;
  height: calc(100% - var(--radius-default));
  overflow-x: hidden;
  overflow-y: auto;
  position: relative;
}

.list-variant {
  top: 0px;
  width: 100%;
  height: var(--list-height);
  position: absolute;
}

.component {
  width: 100%;
  height: 32px;
  flex-direction: row;
  align-items: center;
  gap: 8px;
  padding: 9px;
  display: flex;
  position: absolute;
  line-height: 1;
  font-family: var(--font-code);
}

.selected {
  color: white;
  & svg {
    color: white;
  }
}

.component-label-segment.match {
  font-weight: bold;
}

.top-bar {
  width: 100%;
  height: 40px;
  padding: 4px;
  background-color: var(--background-color);
  border-radius: var(--radius-default);
  position: absolute;
  top: 0px;
  z-index: 1;
}

.top-bar-inner {
  width: 100%;
  height: 100%;
  border-radius: 16px;
  border: 0.5px solid rgba(0, 0, 0, 0.12);
  display: flex;
  flex-direction: row;
  gap: 12px;
  padding: 7px;

  & * {
    color: rgba(0, 0, 0, 0.18);
    transition: color 0.2s;
  }
  & .first-on-right {
    margin-left: auto;
  }
  & > .toggledOn {
    color: rgba(0, 0, 0, 0.6);
  }
}

.bottom-panel {
  position: relative;
}

.component-editor {
  position: absolute;
}

.visualization-preview {
  position: absolute;
}
</style>
