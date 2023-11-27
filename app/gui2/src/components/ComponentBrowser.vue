<script setup lang="ts">
import { componentBrowserBindings } from '@/bindings'
import { makeComponentList, type Component } from '@/components/ComponentBrowser/component'
import { Filtering } from '@/components/ComponentBrowser/filtering'
import { default as DocumentationPanel } from '@/components/DocumentationPanel.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { groupColorStyle, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { useApproach } from '@/util/animation'
import { tryGetIndex } from '@/util/array'
import { useEvent, useResizeObserver } from '@/util/events'
import type { useNavigator } from '@/util/navigator'
import type { Opt } from '@/util/opt'
import { allRanges } from '@/util/range'
import { Vec2 } from '@/util/vec2'
import type { SuggestionId } from 'shared/languageServerTypes/suggestions'
import type { ContentRange, ExprId } from 'shared/yjsModel.ts'
import { computed, nextTick, onMounted, ref, watch, type Ref } from 'vue'
import { useComponentBrowserInput } from './ComponentBrowser/input'

const ITEM_SIZE = 32
const TOP_BAR_HEIGHT = 32

const projectStore = useProjectStore()
const suggestionDbStore = useSuggestionDbStore()
const graphStore = useGraphStore()

const props = defineProps<{
  position: Vec2
  navigator: ReturnType<typeof useNavigator>
  initialContent: string
  initialCaretPosition: ContentRange
  sourceNode: Opt<ExprId>
}>()

const emit = defineEmits<{
  accepted: [searcherExpression: string, requiredImports: RequiredImport[]]
  closed: [searcherExpression: string]
  canceled: []
}>()

function getInitialContent(): string {
  if (props.sourceNode == null) return props.initialContent
  const sourceNode = props.sourceNode
  const sourceNodeName = graphStore.db.getNodeMainOutputPortIdentifier(sourceNode)
  const sourceNodeNameWithDot = sourceNodeName ? sourceNodeName + '.' : ''
  return sourceNodeNameWithDot + props.initialContent
}

function getInitialCaret(): ContentRange {
  if (props.sourceNode == null) return props.initialCaretPosition
  const sourceNode = props.sourceNode
  const sourceNodeName = graphStore.db.getNodeMainOutputPortIdentifier(sourceNode)
  const sourceNodeNameWithDot = sourceNodeName ? sourceNodeName + '.' : ''
  return [
    props.initialCaretPosition[0] + sourceNodeNameWithDot.length,
    props.initialCaretPosition[1] + sourceNodeNameWithDot.length,
  ]
}

onMounted(() => {
  nextTick(() => {
    input.code.value = getInitialContent()
    const caret = getInitialCaret()
    if (inputField.value != null) {
      inputField.value.selectionStart = caret[0]
      inputField.value.selectionEnd = caret[1]
      inputField.value.focus({ preventScroll: true })
      selectLastAfterRefresh()
    }
  })
})

// === Position ===

const transform = computed(() => {
  const nav = props.navigator
  const translate = nav.translate
  const position = translate.add(props.position).scale(nav.scale)
  const x = Math.round(position.x)
  const y = Math.round(position.y)

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

watch(currentFiltering, selectLastAfterRefresh)

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
  if (stillInside) {
    if (inputField.value != null) {
      inputField.value.focus({ preventScroll: true })
    }
  } else {
    emit('closed', input.code.value)
  }
}

// === Components List and Positions ===

const components = computed(() => {
  return makeComponentList(suggestionDbStore.entries, currentFiltering.value)
})

const visibleComponents = computed(() => {
  if (scroller.value == null) return []
  const scrollPosition = animatedScrollPosition.value
  const topmostVisible = componentAtY(scrollPosition)
  const bottommostVisible = Math.max(0, componentAtY(scrollPosition + scrollerSize.value.y))
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
  if (animatedHighlightHeight.value <= 1.0) {
    animatedHighlightPosition.skip()
  }
})

const highlightClipPath = computed(() => {
  let height = animatedHighlightHeight.value
  let position = animatedHighlightPosition.value
  let top = position + ITEM_SIZE - height
  let bottom = listContentHeight.value - position - ITEM_SIZE
  return `inset(${top}px 0px ${bottom}px 0px round 16px)`
})

/**
 * Select the last element after updating component list.
 *
 * As the list changes the scroller's content, we need to wait a frame so the scroller
 * recalculates its height and setting scrollTop will work properly.
 */
function selectLastAfterRefresh() {
  selected.value = 0
  nextTick(() => {
    scrollToSelected()
    animatedScrollPosition.skip()
    animatedHighlightPosition.skip()
  })
}

// === Scrolling ===

const scroller = ref<HTMLElement>()
const scrollerSize = useResizeObserver(scroller)
const scrollPosition = ref(0)
const animatedScrollPosition = useApproach(scrollPosition)

const listContentHeight = computed(() =>
  // We add a top padding of TOP_BAR_HEIGHT / 2 - otherwise the topmost entry would be covered
  // by top bar.
  Math.max(components.value.length * ITEM_SIZE + TOP_BAR_HEIGHT / 2, scrollerSize.value.y),
)
const listContentHeightPx = computed(() => `${listContentHeight.value}px`)

function scrollToSelected() {
  if (selectedPosition.value == null) return
  scrollPosition.value = Math.max(selectedPosition.value - scrollerSize.value.y + ITEM_SIZE, 0)
}

function updateScroll() {
  if (scroller.value && Math.abs(scroller.value.scrollTop - animatedScrollPosition.value) > 1.0) {
    scrollPosition.value = scroller.value.scrollTop
    animatedScrollPosition.skip()
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
    scrollToSelected()
  },
  moveDown() {
    if (selected.value == null) {
      selected.value = components.value.length - 1
    } else if (selected.value > 0) {
      selected.value -= 1
    }
    scrollToSelected()
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
  >
    <div class="panels">
      <div class="panel components">
        <div class="top-bar">
          <div class="top-bar-inner">
            <ToggleIcon v-model="filterFlags.showLocal" icon="local_scope2" />
            <ToggleIcon icon="command_key3" />
            <ToggleIcon v-model="filterFlags.showUnstable" icon="unstable2" />
            <ToggleIcon icon="marketplace" />
            <ToggleIcon v-model="docsVisible" icon="right_side_panel" class="first-on-right" />
          </div>
        </div>
        <div class="components-content">
          <div
            ref="scroller"
            class="list"
            :scrollTop.prop="animatedScrollPosition.value"
            @wheel.stop.passive
            @scroll="updateScroll"
          >
            <div class="list-variant">
              <div
                v-for="item in visibleComponents"
                :key="item.component.suggestionId"
                class="component"
                :style="componentStyle(item.index)"
                @mousemove="selected = item.index"
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
    <div class="CBInput">
      <input
        ref="inputField"
        v-model="input.code.value"
        name="cb-input"
        autocomplete="off"
        @keyup="readInputFieldSelection"
      />
    </div>
  </div>
</template>

<style scoped>
.ComponentBrowser {
  --list-height: 0px;
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
  border-radius: 20px;
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
  clip-path: inset(0 0 0 0 round 20px);
  transition: clip-path 0.2s;
}
.docs.hidden {
  clip-path: inset(0 100% 0 0 round 20px);
}

.list {
  top: 20px;
  width: 100%;
  height: calc(100% - 20px);
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
  border-radius: 20px;
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

.CBInput {
  border-radius: 20px;
  background-color: #eaeaea;
  height: 40px;
  padding: 12px;
  display: flex;
  flex-direction: row;

  & input {
    border: none;
    outline: none;
    min-width: 0;
    flex-grow: 1;
    background: none;
    font: inherit;
  }
}
</style>
