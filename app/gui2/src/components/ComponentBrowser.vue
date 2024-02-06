<script setup lang="ts">
import { componentBrowserBindings } from '@/bindings'
import { makeComponentList, type Component } from '@/components/ComponentBrowser/component'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import { useComponentBrowserInput, type Usage } from '@/components/ComponentBrowser/input'
import { useScrolling } from '@/components/ComponentBrowser/scrolling'
import { default as DocumentationPanel } from '@/components/DocumentationPanel.vue'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useApproach } from '@/composables/animation'
import { useEvent, useResizeObserver } from '@/composables/events'
import type { useNavigator } from '@/composables/navigator'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { groupColorStyle, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import type { VisualizationDataSource } from '@/stores/visualization'
import { tryGetIndex } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { allRanges } from '@/util/data/range'
import { Vec2 } from '@/util/data/vec2'
import { debouncedGetter } from '@/util/reactivity'
import type { SuggestionId } from 'shared/languageServerTypes/suggestions'
import { computed, onMounted, ref, watch, type ComputedRef, type Ref } from 'vue'

const ITEM_SIZE = 32
const TOP_BAR_HEIGHT = 32
// Difference in position between the component browser and a node for the input of the component browser to
// be placed at the same position as the node.
const COMPONENT_BROWSER_TO_NODE_OFFSET = new Vec2(-4, -4)

const projectStore = useProjectStore()
const suggestionDbStore = useSuggestionDbStore()
const graphStore = useGraphStore()

const props = defineProps<{
  nodePosition: Vec2
  navigator: ReturnType<typeof useNavigator>
  usage: Usage
}>()

const emit = defineEmits<{
  accepted: [searcherExpression: string, requiredImports: RequiredImport[]]
  closed: [searcherExpression: string, requiredImports: RequiredImport[]]
  canceled: []
}>()

onMounted(() => {
  input.reset(props.usage)
  if (inputField.value != null) {
    inputField.value.focus({ preventScroll: true })
  } else {
    console.warn(
      'Component Browser input element was not mounted. This is not expected and may break the Component Browser',
    )
  }
})

// === Position ===

const transform = computed(() => {
  const nav = props.navigator
  const translate = nav.translate
  const position = props.nodePosition.add(COMPONENT_BROWSER_TO_NODE_OFFSET)
  const screenPosition = translate.add(position).scale(nav.scale)
  const x = Math.round(screenPosition.x)
  const y = Math.round(screenPosition.y)

  return `translate(${x}px, ${y}px) translateY(-100%)`
})

// === Input and Filtering ===

const cbRoot = ref<HTMLElement>()
const inputField = ref<HTMLInputElement>()
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

function readInputFieldSelection() {
  if (
    inputField.value != null &&
    inputField.value.selectionStart != null &&
    inputField.value.selectionEnd != null
  ) {
    input.selection.value.start = inputField.value.selectionStart
    input.selection.value.end = inputField.value.selectionEnd
  }
}
// HTMLInputElement's same event is not supported in chrome yet. We just react for any
// selectionchange in the document and check if the input selection changed.
// BUT some operations like deleting does not emit 'selectionChange':
// https://bugs.chromium.org/p/chromium/issues/detail?id=725890
// Therefore we must also refresh selection after changing input.
useEvent(document, 'selectionchange', readInputFieldSelection)

watch(
  input.selection,
  (newPos) => {
    if (inputField.value == null) return
    // Do nothing if boundaries didn't change. We don't want to affect selection dir.
    if (
      inputField.value.selectionStart == newPos.start &&
      inputField.value.selectionEnd == newPos.end
    )
      return
    inputField.value.setSelectionRange(newPos.start, newPos.end)
  },
  // This update should be after any possible inputField content update.
  { flush: 'post' },
)

function handleDefocus(e: FocusEvent) {
  const stillInside =
    cbRoot.value != null &&
    e.relatedTarget instanceof Node &&
    cbRoot.value.contains(e.relatedTarget)
  // We want to focus input even when relatedTarget == null, because sometimes defocus event is
  // caused by focused item being removed, for example an entry in visualization chooser.
  if (stillInside || e.relatedTarget == null) {
    inputField.value?.focus({ preventScroll: true })
  }
}

useEvent(
  window,
  'pointerdown',
  (event) => {
    if (event.button !== 0) return
    if (!(event.target instanceof Element)) return
    if (!cbRoot.value?.contains(event.target)) {
      emit('closed', input.code.value, input.importsToAdd())
    }
  },
  { capture: true },
)

const inputElement = ref<HTMLElement>()
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

const previewedExpression = debouncedGetter(() => {
  if (selectedSuggestion.value == null) return input.code.value
  else return input.inputAfterApplyingSuggestion(selectedSuggestion.value).newCode
}, 200)

const previewDataSource: ComputedRef<VisualizationDataSource | undefined> = computed(() => {
  if (!previewedExpression.value.trim()) return
  if (!graphStore.methodAst) return
  const body = graphStore.methodAst.body
  if (!body) return

  return {
    type: 'expression',
    expression: previewedExpression.value,
    contextId: body.externalId,
  }
})

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

const docsVisible = ref(true)

const displayedDocs: Ref<Opt<SuggestionId>> = ref(null)
const docEntry = computed({
  get() {
    return displayedDocs.value
  },
  set(value) {
    displayedDocs.value = value
  },
})

watch(selectedSuggestionId, (id) => {
  docEntry.value = id
})

// === Accepting Entry ===

function applySuggestion(component: Opt<Component> = null): SuggestionEntry | null {
  const providedSuggestion =
    component != null ? suggestionDbStore.entries.get(component.suggestionId) : null
  const suggestion = providedSuggestion ?? selectedSuggestion.value
  if (suggestion == null) return null
  input.applySuggestion(suggestion)
  return suggestion
}

function acceptSuggestion(index: Opt<Component> = null) {
  const applied = applySuggestion(index)
  const shouldFinish = applied != null && applied.kind !== SuggestionKind.Module
  if (shouldFinish) acceptInput()
}

function acceptInput() {
  emit('accepted', input.code.value, input.importsToAdd())
}

// === Key Events Handler ===

const handler = componentBrowserBindings.handler({
  applySuggestion() {
    applySuggestion()
  },
  acceptSuggestion() {
    applySuggestion()
    acceptInput()
  },
  acceptInput() {
    acceptInput()
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
  cancelEditing() {
    emit('canceled')
  },
})
</script>

<template>
  <div
    ref="cbRoot"
    class="ComponentBrowser"
    :style="{ transform, '--list-height': listContentHeightPx }"
    tabindex="-1"
    @focusout="handleDefocus"
    @keydown="handler"
    @pointerdown.stop
    @keydown.enter.stop
    @keydown.backspace.stop
    @keydown.delete.stop
  >
    <div class="panels">
      <div class="panel components">
        <div class="top-bar">
          <div class="top-bar-inner">
            <ToggleIcon v-model="filterFlags.showLocal" icon="local_scope2" />
            <ToggleIcon icon="command3" />
            <ToggleIcon v-model="filterFlags.showUnstable" icon="unstable2" />
            <ToggleIcon icon="marketplace" />
            <ToggleIcon v-model="docsVisible" icon="right_side_panel" class="first-on-right" />
          </div>
        </div>
        <div class="components-content">
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
                  <span
                    v-if="!item.component.matchedRanges || item.component.matchedAlias"
                    v-text="item.component.label"
                  ></span>
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
                  <span
                    v-if="!item.component.matchedRanges || item.component.matchedAlias"
                    v-text="item.component.label"
                  ></span>
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
      </div>
      <div class="panel docs" :class="{ hidden: !docsVisible }">
        <DocumentationPanel v-model:selectedEntry="docEntry" />
      </div>
    </div>
    <div class="bottom-panel">
      <GraphVisualization
        class="visualization-preview"
        :nodeSize="inputSize"
        :nodePosition="nodePosition"
        :scale="1"
        :isCircularMenuVisible="false"
        :dataSource="previewDataSource"
      />
      <div ref="inputElement" class="CBInput">
        <input
          ref="inputField"
          v-model="input.code.value"
          name="cb-input"
          autocomplete="off"
          @input="readInputFieldSelection"
        />
      </div>
    </div>
  </div>
</template>

<style scoped>
.ComponentBrowser {
  --list-height: 0px;
  --radius-default: 20px;
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
  background-color: #eaeaea;
}

.components {
  width: 190px;
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
  background-color: #eaeaea;
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

  & svg {
    color: rgba(0, 0, 0, 0.18);
    transition: color 0.2s;
  }
  & .first-on-right {
    margin-left: auto;
  }
  & > svg.toggledOn {
    color: rgba(0, 0, 0, 0.6);
  }

  & > svg:not(.toggledOn):hover {
    color: rgba(0, 0, 0, 0.3);
  }
}

.bottom-panel {
  position: relative;
}
.CBInput {
  border-radius: var(--radius-default);
  background-color: #eaeaea;
  width: 100%;
  height: 40px;
  padding: 12px;
  display: flex;
  flex-direction: row;
  position: absolute;

  & input {
    border: none;
    outline: none;
    min-width: 0;
    flex-grow: 1;
    background: none;
    font: inherit;
  }
}

.visualization-preview {
  position: absolute;
}
</style>
