<script setup lang="ts">
import { componentBrowserBindings } from '@/bindings'
import { type Component } from '@/components/ComponentBrowser/component'
import ComponentEditor from '@/components/ComponentBrowser/ComponentEditor.vue'
import ComponentList from '@/components/ComponentBrowser/ComponentList.vue'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import { useComponentBrowserInput, type Usage } from '@/components/ComponentBrowser/input'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import SvgButton from '@/components/SvgButton.vue'
import { useResizeObserver } from '@/composables/events'
import type { useNavigator } from '@/composables/navigator'
import { groupColorStyle } from '@/composables/nodeColors'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { type Typename } from '@/stores/suggestionDatabase/entry'
import type { VisualizationDataSource } from '@/stores/visualization'
import { cancelOnClick, isNodeOutside, targetIsOutside } from '@/util/autoBlur'
import { tryGetIndex } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { DEFAULT_ICON, iconOfNode, suggestionEntryToIcon } from '@/util/getIconName'
import { debouncedGetter } from '@/util/reactivity'
import type { ComponentInstance } from 'vue'
import { computed, onMounted, onUnmounted, ref, watch, watchEffect } from 'vue'
import type { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

// Difference in position between the component browser and a node for the input of the component browser to
// be placed at the same position as the node.
const COMPONENT_BROWSER_TO_NODE_OFFSET = new Vec2(-4, -4)
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
  associatedElements: HTMLElement[]
}>()

const emit = defineEmits<{
  accepted: [
    searcherExpression: string,
    requiredImports: RequiredImport[],
    firstAppliedReturnType: Typename | undefined,
  ]
  canceled: []
  selectedSuggestionId: [id: SuggestionId | null]
  isAiPrompt: [boolean]
}>()

const cbRoot = ref<HTMLElement>()
const componentList = ref<ComponentInstance<typeof ComponentList>>()

defineExpose({ cbRoot })

const clickOutsideAssociatedElements = (e: PointerEvent) => {
  return props.associatedElements.length === 0 ?
      false
    : props.associatedElements.every((element) => targetIsOutside(e, element))
}
const cbOpen: Interaction = cancelOnClick(clickOutsideAssociatedElements, {
  cancel: () => emit('canceled'),
  end: () => {
    // In AI prompt mode likely the input is not a valid mode.
    if (input.mode.mode !== 'aiPrompt') {
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
  const screenRect = cbRoot.value?.getBoundingClientRect()
  if (!screenRect) return
  const area = new Rect(
    origin,
    new Vec2(screenRect.width, screenRect.height).scale(clientToSceneFactor.value),
  )
  const margins = scaleValues(PAN_MARGINS, clientToSceneFactor.value)
  props.navigator.panToThenFollow([
    // Always include the top-left of the input area.
    { x: area.left, y: area.top },
    // Try to reach the bottom-right corner of the panels.
    { x: area.right, y: area.bottom },
    // Top (and left) margins are more important than bottom (and right) margins because the screen has controls across
    // the top and on the left.
    { x: area.left - margins.left, y: area.top - margins.top },
    // If the screen is very spacious, even the bottom right gets some breathing room.
    { x: area.right + margins.right, y: area.bottom + margins.bottom },
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

  return `translate(${x}px, ${y}px)`
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

const currentFiltering = computed(() => {
  if (input.mode.mode === 'componentBrowsing') {
    const currentModule = projectStore.modulePath
    return new Filtering(input.mode.filter, currentModule?.ok ? currentModule.value : undefined)
  } else {
    return undefined
  }
})

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
  return selectedSuggestion.value ? suggestionEntryToIcon(selectedSuggestion.value) : undefined
})

const icon = computed(() => {
  if (!input.selfArgument) return undefined
  if (input.mode.mode === 'componentBrowsing' && selectedSuggestionIcon.value)
    return selectedSuggestionIcon.value
  if (props.usage.type === 'editNode') {
    return iconOfNode(props.usage.node, graphStore.db)
  }
  return DEFAULT_ICON
})

// === Preview ===

const previewedCode = debouncedGetter<string>(() => input.code, 200)

const previewedSuggestionReturnType = computed(() => {
  const id = input.mode.mode === 'codeEditing' ? input.mode.appliedSuggestion : undefined
  const appliedEntry = id != null ? suggestionDbStore.entries.get(id) : undefined
  if (appliedEntry != null) return appliedEntry.returnType
  else if (props.usage.type === 'editNode') {
    return graphStore.db.getNodeMainSuggestion(props.usage.node)?.returnType
  }
  return undefined
})

const previewDataSource = computed<VisualizationDataSource | undefined>(() => {
  if (input.mode.mode !== 'codeEditing') return
  if (!previewedCode.value.trim()) return
  if (!graphStore.methodAst.ok) return
  const body = graphStore.methodAst.value.body
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

watch(selectedSuggestionId, (id) => emit('selectedSuggestionId', id ?? null))
watch(
  () => input.mode,
  (mode) => emit('isAiPrompt', mode.mode === 'aiPrompt'),
)

// === Accepting Entry ===

function acceptSuggestion(component: Opt<Component> = null) {
  const suggestionId = component?.suggestionId ?? selectedSuggestionId.value
  if (suggestionId == null) return acceptInput()
  const result = input.applySuggestion(suggestionId)
  if (result.ok) acceptInput()
  else result.error.log('Cannot apply suggestion')
}

function applySuggestion(component: Opt<Component> = null) {
  const suggestionId = component?.suggestionId ?? selectedSuggestionId.value
  if (suggestionId == null) return input.switchToCodeEditMode()
  const result = input.applySuggestion(suggestionId)
  if (!result.ok) result.error.log('Cannot apply suggestion')
}

function acceptInput() {
  const appliedReturnType =
    input.mode.mode === 'codeEditing' && input.mode.appliedSuggestion != null ?
      suggestionDbStore.entries.get(input.mode.appliedSuggestion)?.returnType
    : undefined
  emit('accepted', input.code.trim(), input.importsToAdd(), appliedReturnType)
  interaction.ended(cbOpen)
}

// === Key Events Handler ===

const handler = componentBrowserBindings.handler({
  applySuggestion() {
    if (input.mode.mode != 'componentBrowsing') return false
    applySuggestion()
  },
  acceptSuggestion() {
    if (input.mode.mode != 'componentBrowsing') return false
    acceptSuggestion()
  },
  acceptCode() {
    if (input.mode.mode != 'codeEditing') return false
    acceptInput()
  },
  acceptInput() {
    if (input.mode.mode != 'componentBrowsing' && input.mode.mode != 'codeEditing') return false
    acceptInput()
  },
  acceptAIPrompt() {
    if (input.mode.mode == 'aiPrompt') input.applyAIPrompt()
    else return false
  },
  moveUp() {
    componentList.value?.moveUp()
  },
  moveDown() {
    componentList.value?.moveDown()
  },
})
</script>

<template>
  <div
    ref="cbRoot"
    class="ComponentBrowser"
    :style="{ transform }"
    :data-self-argument="input.selfArgument"
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
    <GraphVisualization
      v-if="input.mode.mode === 'codeEditing' && isVisualizationVisible"
      class="visualization-preview"
      :nodeSize="inputSize"
      :nodePosition="nodePosition"
      :scale="1"
      :isCircularMenuVisible="false"
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
      class="component-editor"
      :navigator="props.navigator"
      :icon="icon"
      :nodeColor="nodeColor"
      :style="{ '--component-editor-padding': cssComponentEditorPadding }"
    >
      <SvgButton
        name="add_to_graph_editor"
        :title="
          input.mode.mode === 'componentBrowsing' && selected != null ?
            'Accept Suggested Component'
          : 'Accept'
        "
        @click.stop="input.mode.mode === 'componentBrowsing' ? acceptSuggestion() : acceptInput()"
      />
      <SvgButton
        name="edit"
        :disabled="input.mode.mode === 'codeEditing'"
        :title="selected != null ? 'Edit Suggested Component' : 'Code Edit Mode'"
        data-testid="switchToEditMode"
        @click.stop="applySuggestion()"
      />
    </ComponentEditor>
    <div
      v-if="input.mode.mode === 'codeEditing' && !isVisualizationVisible"
      class="show-visualization"
    >
      <SvgButton
        name="eye"
        title="Show visualization"
        @click.stop="isVisualizationVisible = true"
      />
    </div>
    <ComponentList
      v-if="input.mode.mode === 'componentBrowsing' && currentFiltering"
      ref="componentList"
      :filtering="currentFiltering"
      :autoSelectFirstComponent="true"
      @acceptSuggestion="acceptSuggestion($event)"
      @update:selectedComponent="selected = $event"
    />
  </div>
</template>

<style scoped>
.ComponentBrowser {
  --radius-default: 20px;
  --background-color: #eaeaea;
  --doc-panel-bottom-clip: 4px;
  min-width: 295px;
  width: min-content;
  color: rgba(0, 0, 0, 0.6);
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
}

.component-editor {
  position: relative;
}

.visualization-preview {
  position: absolute;
}
</style>
