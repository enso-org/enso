<script setup lang="ts">
import {
  useGraphStore,
  useProjectNames,
  useSuggestionDbStore,
} from '$/components/WithCurrentProject.vue'
import type { RequiredImport } from '$/providers/openedProjects/module/imports'
import { type Typename } from '$/providers/openedProjects/suggestionDatabase/entry'
import { componentBrowserBindings, listBindings } from '@/bindings'
import ActionButton from '@/components/ActionButton.vue'
import type { Component } from '@/components/ComponentBrowser/component'
import ComponentEditor from '@/components/ComponentBrowser/ComponentEditor.vue'
import ComponentList from '@/components/ComponentBrowser/ComponentList.vue'
import { useComponentBrowserInput, type Usage } from '@/components/ComponentBrowser/input'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import { useResizeObserver } from '@/composables/events'
import type { useNavigator } from '@/composables/navigator'
import { groupColorStyle } from '@/composables/nodeColors'
import { registerHandlers, toggledAction, type Action } from '@/providers/action'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { VisualizationDataSource } from '@/stores/visualization'
import { isNodeOutside, targetIsOutside } from '@/util/autoBlur'
import { tryGetIndex } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { parseAbsoluteProjectPathRaw } from '@/util/projectPath'
import { debouncedGetter } from '@/util/reactivity'
import * as objects from 'enso-common/src/utilities/data/object'
import { Ok } from 'enso-common/src/utilities/data/result'
import type { ComponentInstance } from 'vue'
import { computed, onMounted, onUnmounted, ref, toValue, watch, watchEffect } from 'vue'
import type { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import { Range } from 'ydoc-shared/util/data/range'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'
import { NODE_CONTENT_PADDING } from './GraphEditor/GraphNode.vue'

// Difference in position between the component browser and a node for the input of the component browser to
// be placed at the same position as the node.
const COMPONENT_BROWSER_TO_NODE_OFFSET = new Vec2(0, 0)
const PAN_MARGINS = {
  top: 48,
  bottom: 40,
  left: 80,
  right: 40,
}
const COMPONENT_EDITOR_PADDING = NODE_CONTENT_PADDING
const ICON_WIDTH = 24
// Component editor is larger than a typical node, so the edge should touch it a bit higher.
const EDGE_Y_OFFSET = -8
const MIN_WIDTH = 295

const cssComponentEditorPadding = `${COMPONENT_EDITOR_PADDING}px`

const suggestionDbStore = useSuggestionDbStore()
const graphStore = useGraphStore()
const interaction = injectInteractionHandler()
const projectNames = useProjectNames()

const props = defineProps<{
  nodePosition: Vec2
  navigator: ReturnType<typeof useNavigator>
  usage: Usage
  graphEditorRoot: Opt<HTMLElement>
}>()

const emit = defineEmits<{
  accepted: [
    searcherExpression: string,
    requiredImports: RequiredImport[],
    firstAppliedReturnType: Typename | undefined,
  ]
  canceled: []
  selectedSuggestionId: [id: SuggestionId | undefined]
  isAiPrompt: [boolean]
}>()

const cbRoot = ref<HTMLElement>()
const componentList = ref<ComponentInstance<typeof ComponentList>>()

const cbOpen: Interaction = {
  pointerdown: (e: PointerEvent) => {
    if (targetIsOutside(e, cbRoot.value) && !targetIsOutside(e, props.graphEditorRoot)) {
      if (props.usage.type === 'editNode') {
        acceptInput()
      } else {
        emit('canceled')
      }
    }
    return false
  },
  cancel: () => {
    emit('canceled')
  },
  end: () => {
    // In AI prompt mode, the input is likely not a valid expression.
    if (input.mode.mode === 'aiPrompt') {
      emit('canceled')
    } else {
      acceptInput()
    }
  },
}

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
  const screenRect = cbRoot.value?.getBoundingClientRect()
  if (!screenRect) return
  const area = new Rect(
    origin,
    new Vec2(screenRect.width, screenRect.height).scale(clientToSceneFactor.value),
  )
  const margins = scaleValues(PAN_MARGINS, clientToSceneFactor.value)
  props.navigator.panToThenFollow([
    // Always include the top-left of the input area.
    new Vec2(area.left, area.top),
    // Try to reach the bottom-right corner of the panels.
    new Vec2(area.right, area.bottom),
    // Top (and left) margins are more important than bottom (and right) margins because the screen has controls across
    // the top and on the left.
    new Vec2(area.left - margins.left, area.top - margins.top),
    // If the screen is very spacious, even the bottom right gets some breathing room.
    new Vec2(area.right + margins.right, area.bottom + margins.bottom),
  ])
}

onMounted(() => {
  interaction.setCurrent(cbOpen)
  input.reset(props.usage)
  inputElement.value?.delayedFocus()
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

  return `translate(${x}px, ${y}px)`
})

const minWidth = computed(() => {
  if (props.usage.type !== 'editNode') return `${MIN_WIDTH}px`
  const rect = graphStore.nodeRects.get(props.usage.node)
  if (rect == null) return `${MIN_WIDTH}px`
  return `${rect.width * props.navigator.scale}px`
})

// === Selection ===

const selected = ref<Component | null>(null)

const selectedSuggestionId = computed(() => selected.value?.suggestionId)
const selectedSuggestion = computed(() => {
  const id = selectedSuggestionId.value
  if (id == null) return null
  return suggestionDbStore.entries.get(id) ?? null
})

// === Input and Filtering ===

const input = useComponentBrowserInput()

onUnmounted(() => {
  graphStore.cbEditedEdge = undefined
})

// Compute edge, except for the color. The color is set in a separate watch, as it changes more often.
watchEffect(() => {
  const sourceIdent = input.selfArgument
  const sourceNode =
    sourceIdent != null ? graphStore.db.getIdentDefiningNode(sourceIdent) : undefined
  const source = graphStore.db.getNodeFirstOutputPort(sourceNode)
  if (!source) {
    graphStore.cbEditedEdge = undefined
    return
  }
  const scenePos = originScenePos.value.add(
    new Vec2(COMPONENT_EDITOR_PADDING + ICON_WIDTH / 2, 0)
      .scale(clientToSceneFactor.value)
      .add(new Vec2(0, EDGE_Y_OFFSET)),
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

// === Preview ===

const previewedCode = debouncedGetter<string>(() => input.code, 200)

const previewedSuggestionReturnType = computed(() => {
  const appliedEntry = input.mode.mode === 'codeEditing' ? input.mode.appliedSuggestion : undefined
  const entry =
    appliedEntry ? appliedEntry
    : props.usage.type === 'editNode' ? graphStore.db.getNodeMainSuggestion(props.usage.node)
    : undefined
  const returnType = entry?.returnType(projectNames)
  if (returnType == null) return undefined
  const parsed = parseAbsoluteProjectPathRaw(returnType)
  if (parsed.ok) return parsed.value
  return undefined
})

const previewDataSource = computed<VisualizationDataSource | undefined>(() => {
  if (input.mode.mode !== 'codeEditing') return
  if (!previewedCode.value.trim()) return
  if (!graphStore.currentMethod.ast.ok) return
  const body = graphStore.currentMethod.ast.value.body
  if (!body) return
  return {
    type: 'expression',
    expression: previewedCode.value,
    contextId: body.externalId,
  }
})

const visualizationSelection = ref<Opt<VisualizationIdentifier>>(
  props.usage.type === 'editNode' ?
    graphStore.db.nodeIdToNode.get(props.usage.node)?.vis?.identifier
  : undefined,
)

const isVisualizationVisible = ref(true)

// === Documentation Panel ===

watch(selectedSuggestionId, (id) => emit('selectedSuggestionId', id))
watch(
  () => input.mode,
  (mode) => emit('isAiPrompt', mode.mode === 'aiPrompt'),
)

// === Accepting Entry ===

function applyComponent(component: Opt<Component> = null) {
  component ??= selected.value
  if (component == null) {
    input.switchToCodeEditMode()
    return Ok()
  }
  if (component.suggestionId != null) {
    return input.applySuggestion(component.suggestionId, component.macroSuffix)
  } else {
    // Component without suggestion database entry, for example "literal" component.
    input.content = { text: component.label, selection: Range.emptyAt(component.label.length) }
    input.switchToCodeEditMode()
    return Ok()
  }
}

function acceptComponent(component: Opt<Component> = null) {
  const result = applyComponent(component)
  if (result.ok) acceptInput()
  else result.error.log('Cannot apply suggestion')
}

function acceptInput() {
  const appliedReturnType =
    input.mode.mode === 'codeEditing' ?
      input.mode.appliedSuggestion?.returnType(projectNames)
    : undefined
  emit('accepted', input.code.trim(), input.importsToAdd(), appliedReturnType)
  interaction.ended(cbOpen)
}

// === Action Handlers ===

const insideComponentBrowsing = computed(() => input.mode.mode === 'componentBrowsing')
const actions = registerHandlers({
  'componentBrowser.editSuggestion': {
    enabled: insideComponentBrowsing,
    action: () => {
      const result = applyComponent()
      if (!result.ok) result.error.log('Cannot apply component')
    },
  },
  'componentBrowser.acceptSuggestion': {
    enabled: insideComponentBrowsing,
    action: () => acceptComponent(),
  },
  'componentBrowser.acceptInputAsCode': {
    available: () => input.mode.mode === 'codeEditing',
    action: acceptInput,
  },
  'componentBrowser.switchToCodeEditMode': {
    enabled: insideComponentBrowsing,
    action: input.switchToCodeEditMode,
  },
  'component.toggleVisualization': {
    ...toggledAction(isVisualizationVisible),
    available: () => input.mode.mode === 'codeEditing' && !isVisualizationVisible.value,
  },
  'componentBrowser.acceptInput': {
    action: acceptInput,
  },
  'componentBrowser.acceptAIPrompt': {
    available: () => input.mode.mode == 'aiPrompt',
    action: () => input.applyAIPrompt(),
  },
  'componentBrowser.switchPanelFocus': { action: () => componentList.value?.switchPanelFocus() },
  'list.moveUp': { action: () => componentList.value?.moveUp() },
  'list.moveDown': { action: () => componentList.value?.moveDown() },
})

function performActionIfNotDisabled(action: Action & { action: () => void }) {
  if (!toValue(action.available ?? true) || !toValue(action.enabled ?? true)) return false
  else return action.action()
}

const handler = componentBrowserBindings.handler(
  objects.mapEntries(
    componentBrowserBindings.bindings,
    (actionName) => () => performActionIfNotDisabled(actions[actionName]),
  ),
)

const listsHandler = listBindings.handler({
  'list.moveUp': actions['list.moveUp'].action,
  'list.moveDown': actions['list.moveDown'].action,
})
</script>

<template>
  <div
    ref="cbRoot"
    class="ComponentBrowser"
    :style="{ transform, minWidth }"
    :data-self-argument="input.selfArgument"
    tabindex="-1"
    @focusout="handleDefocus"
    @keydown="handler($event) !== false || listsHandler($event)"
    @pointerdown.stop.prevent
    @pointerup.stop.prevent
    @click.stop.prevent
    @keydown.enter.stop
    @keydown.backspace.stop
    @keydown.delete.stop
    @keydown.arrow-left.stop
    @keydown.arrow-right.stop
  >
    <GraphVisualization
      :show="input.mode.mode === 'codeEditing' && isVisualizationVisible"
      class="visualization-preview"
      :nodeSize="inputSize"
      :nodePosition="nodePosition"
      :scale="1"
      :isFullscreen="false"
      :isFullscreenAllowed="false"
      :isResizable="false"
      :isFocused="true"
      :width="null"
      :height="null"
      :dataSource="previewDataSource"
      :typename="previewedSuggestionReturnType"
      :currentType="visualizationSelection"
      @update:id="visualizationSelection = $event"
      @update:enabled="isVisualizationVisible = $event"
    />
    <ComponentEditor
      ref="inputElement"
      v-model="input.content"
      :usage="usage"
      :mode="input.mode"
      :nodeColor="nodeColor"
      :style="{ '--component-editor-padding': cssComponentEditorPadding }"
    />
    <div class="show-visualization">
      <ActionButton action="component.toggleVisualization" />
    </div>
    <ComponentList
      v-if="input.mode.mode === 'componentBrowsing'"
      ref="componentList"
      :filter="input.mode.filter"
      :literal="input.mode.literal"
      @acceptSuggestion="acceptComponent($event)"
      @update:selectedComponent="selected = $event"
    />
  </div>
</template>

<style scoped>
.ComponentBrowser {
  --radius-default: 20px;
  --background-color: #fff;
  --doc-panel-bottom-clip: 4px;
  width: min-content;
  font-size: 11.5px;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.show-visualization {
  position: relative;
  display: flex;
  padding: 8px;
  opacity: 30%;
  &:not(:has(> *)) {
    display: none;
  }
}

.ComponentEditor {
  position: relative;
  z-index: 1;
}

.visualization-preview {
  position: absolute;
}
</style>
