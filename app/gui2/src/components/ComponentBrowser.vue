<script lang="ts">
/** One of the modes of the component browser:
 * * "component browsing" when user wants to add new component
 * * "code editing" for editing existing, or just added nodes
 * See https://github.com/enso-org/enso/issues/10598 for design details.
 */
export enum ComponentBrowserMode {
  COMPONENT_BROWSING,
  CODE_EDITING,
}
</script>

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
import { SuggestionKind, type Typename } from '@/stores/suggestionDatabase/entry'
import type { VisualizationDataSource } from '@/stores/visualization'
import { cancelOnClickOutside, isNodeOutside } from '@/util/autoBlur'
import { tryGetIndex } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { DEFAULT_ICON, suggestionEntryToIcon } from '@/util/getIconName'
import { iconOfNode } from '@/util/getIconName.ts'
import { debouncedGetter } from '@/util/reactivity'
import type { ComponentInstance, Ref } from 'vue'
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
}>()

const emit = defineEmits<{
  accepted: [
    searcherExpression: string,
    requiredImports: RequiredImport[],
    firstAppliedReturnType: Typename | undefined,
  ]
  canceled: []
}>()

const cbRoot = ref<HTMLElement>()
const componentList = ref<ComponentInstance<typeof ComponentList>>()

const mode = ref<ComponentBrowserMode>(
  props.usage.type === 'newNode' ?
    ComponentBrowserMode.COMPONENT_BROWSING
  : ComponentBrowserMode.CODE_EDITING,
)

const cbOpen: Interaction = cancelOnClickOutside(cbRoot, {
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
  const screenRect = cbRoot.value?.getBoundingClientRect()
  if (!screenRect) return
  const area = new Rect(
    origin,
    screenRect.size.scale(clientToSceneFactor.value),
  )
  const margins = scaleValues(PAN_MARGINS, clientToSceneFactor.value)
  props.navigator.panTo([
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
  const currentModule = projectStore.modulePath
  return new Filtering(input.filter.value, currentModule?.ok ? currentModule.value : undefined)
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
  if (!input.selfArgument.value) return undefined
  if (mode.value === ComponentBrowserMode.COMPONENT_BROWSING && selectedSuggestionIcon.value)
    return selectedSuggestionIcon.value
  if (props.usage.type === 'editNode') {
    return iconOfNode(props.usage.node, graphStore.db)
  }
  return DEFAULT_ICON
})

// === Preview ===

const previewedCode = debouncedGetter<string>(() => input.code.value, 200)

const previewedSuggestionReturnType = computed(() => {
  const id = appliedSuggestion.value
  const appliedEntry = id != null ? suggestionDbStore.entries.get(id) : undefined
  if (appliedEntry != null) return appliedEntry.returnType
  else if (props.usage.type === 'editNode') {
    return graphStore.db.getNodeMainSuggestion(props.usage.node)?.returnType
  }
  return undefined
})

const previewDataSource = computed<VisualizationDataSource | undefined>(() => {
  if (input.isAiPrompt.value) return
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

// === Documentation Panel ===

const docEntry: Ref<Opt<SuggestionId>> = ref(null)

watch(selectedSuggestionId, (id) => {
  docEntry.value = id
})

// === Accepting Entry ===

const appliedSuggestion = ref<SuggestionId>()

function applySuggestion(component: Opt<Component> = null) {
  const suggestionId = component?.suggestionId ?? selectedSuggestionId.value
  if (suggestionId == null) return
  input.applySuggestion(suggestionId)
  appliedSuggestion.value = suggestionId
}

function applySuggestionAndSwitchToEditMode() {
  applySuggestion()
  mode.value = ComponentBrowserMode.CODE_EDITING
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
  applySuggestionAndSwitchToEditMode() {
    if (mode.value != ComponentBrowserMode.COMPONENT_BROWSING || input.isAiPrompt.value)
      return false
    applySuggestionAndSwitchToEditMode()
  },
  acceptSuggestion() {
    if (mode.value != ComponentBrowserMode.COMPONENT_BROWSING || input.isAiPrompt.value)
      return false
    acceptSuggestion()
  },
  acceptCode() {
    if (mode.value != ComponentBrowserMode.CODE_EDITING || input.isAiPrompt.value) return false
    acceptInput()
  },
  acceptInput() {
    if (input.isAiPrompt.value) return false
    acceptInput()
  },
  acceptAIPrompt() {
    if (input.isAiPrompt.value) input.applyAIPrompt()
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
    <GraphVisualization
      v-if="mode === ComponentBrowserMode.CODE_EDITING && !input.isAiPrompt.value"
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
      :currentType="visualizationSelection"
      @update:id="visualizationSelection = $event"
    />
    <ComponentEditor
      ref="inputElement"
      v-model="input.content.value"
      class="component-editor"
      :navigator="props.navigator"
      :icon="icon"
      :nodeColor="nodeColor"
      :style="{ '--component-editor-padding': cssComponentEditorPadding }"
    >
      <SvgButton
        name="add"
        :title="
          mode === ComponentBrowserMode.COMPONENT_BROWSING && selected != null ?
            'Accept Suggested Component'
          : 'Accept'
        "
        @click.stop="
          mode === ComponentBrowserMode.COMPONENT_BROWSING ? acceptSuggestion() : acceptInput()
        "
      />
      <SvgButton
        name="edit"
        :disabled="mode === ComponentBrowserMode.CODE_EDITING"
        :title="selected != null ? 'Edit Suggested Component' : 'Code Edit Mode'"
        data-testid="switchToEditMode"
        @click.stop="applySuggestionAndSwitchToEditMode()"
      />
    </ComponentEditor>
    <ComponentList
      v-if="mode === ComponentBrowserMode.COMPONENT_BROWSING && !input.isAiPrompt.value"
      ref="componentList"
      :filtering="currentFiltering"
      :autoSelectFirstComponent="input.autoSelectFirstComponent.value"
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
  width: 295px;
  color: rgba(0, 0, 0, 0.6);
  font-size: 11.5px;
  display: flex;
  flex-direction: column;
  gap: 4px;
}

.component-editor {
  position: relative;
}

.visualization-preview {
  position: absolute;
}
</style>
