<script setup lang="ts" generic="T">
import SizeTransition from '@/components/SizeTransition.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useRaf } from '@/composables/animation'
import { useEvent } from '@/composables/events'
import { useAppClass } from '@/providers/appClass'
import { Vec2 } from '@/util/data/vec2'
import { computed, type Ref, ref, shallowReactive, watchEffect, watchPostEffect } from 'vue'
import { Range } from 'ydoc-shared/util/data/range'

const props = defineProps<{
  items: T[]
  getKey?: (item: T) => string | number | undefined
  /**
   * If present, a {@link DataTransferItem} is added with a MIME type of `text/plain`.
   * This is useful if the drag payload has a representation that can be pasted in terminals,
   * search bars, and/or address bars.
   */
  toPlainText?: (item: T) => string
  /**
   * The MIME type for the payload output added by `toDragPayload`.
   * Unused if `toDragPayload` is not also present.
   * When in doubt, this should be `application/json`.
   * Defaults to `application/octet-stream`, meaning the payload is arbitrary binary data.
   */
  dragMimeType?: string
  /**
   * Convert the list item to a drag payload stored under `dragMimeType`. When in doubt, this
   * should be `JSON.stringify` of data describing the object.
   */
  toDragPayload?: (item: T) => string
  /**
   * Transform a drag position from client space to appropriate list "scene" space. Not necessary
   * when the list is not transformed with scale.
   */
  toDragPosition?: (p: Vec2) => Vec2
  showHandles: boolean
  axis: 'x' | 'y'
  horizontalScroll?: boolean
}>()
const emit = defineEmits<{
  addItem: []
  reorder: [oldIndex: number, newIndex: number]
  remove: [index: number]
  dropInsert: [index: number, payload: string]
}>()

const listUuid = crypto.randomUUID()

const mimeType = computed(() => props.dragMimeType ?? 'application/octet-stream')

const dragMetaMimePrefix = 'application/x-enso-list-item;item='

function stringToHex(str: string) {
  return Array.from(str, (c) =>
    c.charCodeAt(0) < 128 ?
      c.charCodeAt(0).toString(16)
    : encodeURIComponent(c).replace(/%/g, '').toLowerCase(),
  ).join('')
}

function hexToString(hex: string) {
  return decodeURIComponent('%' + (hex.match(/.{1,2}/g) ?? []).join('%'))
}

function encodeMetadataToMime(meta: DropMetadata) {
  return `${dragMetaMimePrefix}${stringToHex(JSON.stringify(meta))}`
}

function decodeMetadataFromMime(mime: string): DropMetadata | undefined {
  if (!mime.startsWith(dragMetaMimePrefix)) return
  const data = hexToString(mime.substring(dragMetaMimePrefix.length))
  return JSON.parse(data)
}

const draggedIndex = ref<number>()

interface BaseItem {
  key: string | number
}

interface NonPlaceholderItem extends BaseItem {
  type: 'item'
  index: number
  item: T
  hintDeletable: Ref<boolean>
}

interface PlaceholderItem extends BaseItem {
  type: 'placeholder'
  size: number
}

type DragItem = NonPlaceholderItem | PlaceholderItem

const defaultPlaceholderKey = '__placeholder_key__'

const mappedItems = computed<DragItem[]>(() => {
  return props.items.map((item, index) => ({
    type: 'item',
    index,
    item,
    key: props.getKey?.(item) ?? index,
    hintDeletable: ref(false),
  }))
})

const dropInfo = ref<DropHoverInfo>()
const dropIndex = ref<number>()

watchEffect(() => {
  const info = dropInfo.value
  if (info == null) {
    dropIndex.value = undefined
  } else {
    const index = getDropIndex(info, itemAxisBounds)
    if (index !== dropIndex.value) dropIndex.value = index
  }
})

const displayedChildren = computed(() => {
  const items = [...mappedItems.value]
  const index = dropIndex.value
  if (index != null && index >= 0 && dropInfo.value != null) {
    const meta = dropInfo.value.meta
    const key = meta.list === listUuid ? meta.key : defaultPlaceholderKey

    items.splice(index, 0, {
      type: 'placeholder',
      size: meta.size,
      key,
    } as const)
  }
  return items.filter((item) => item.type !== 'item' || item.index !== draggedIndex.value)
})

const cssPropsToCopy = [
  '--color-node-primary',
  '--color-edge-from-node',
  '--node-border-radius',
  'font-family',
  'font-size',
]

function transformPosition(p: Vec2): Vec2 {
  return props.toDragPosition?.(p) ?? p
}

function onDragStart(event: DragEvent, index: number) {
  if (!event.dataTransfer) return
  if (!(event.target instanceof HTMLElement)) return
  // The element that will be shown following the mouse.
  const previewElement = event.target.parentElement
  if (!(previewElement instanceof HTMLElement)) return
  // The element being replaced with a placeholder during the operation.
  const sizeElement = previewElement.parentElement
  if (!(sizeElement instanceof HTMLElement)) return

  const xAxis = props.axis === 'x'

  // Create a fake offscreen DOM element to use as the drag "ghost" image. It will hold a visual
  // clone of the widget being dragged. The ghost style is modified to add a background color
  // and additional border, as well as apply appropriate element scaling in cross-browser way.
  const elementOffsetSize = xAxis ? sizeElement.offsetWidth : sizeElement.offsetHeight
  const elementRect = originalBoundingClientRect.call(sizeElement)
  const rectAxisSize = xAxis ? elementRect.width : elementRect.height
  const elementScale = rectAxisSize / elementOffsetSize
  // Drag ghost need two layers, root and actual ghost element. Othwerwise some styles
  // (such as transform) are not respected for drag images when applied directly to root.
  const dragGhostRoot = document.createElement('div')
  const dragGhost = document.createElement('div')
  dragGhost.classList.add('draggableList-drag-ghost')
  const previewElementStyle = getComputedStyle(previewElement)

  const elementTopLeft = transformPosition(new Vec2(elementRect.left, elementRect.top))
  const currentMousePos = transformPosition(new Vec2(event.clientX, event.clientY))
  const elementRelativeOffset = currentMousePos.sub(elementTopLeft).scale(elementScale)
  // To maintain appropriate styling, we have to copy over a set of node tree CSS variables from
  // the preview element to the ghost element.
  cssPropsToCopy.forEach((prop) => {
    dragGhost.style.setProperty(prop, previewElementStyle.getPropertyValue(prop))
  })
  dragGhost.style.setProperty('transform', `scale(${elementScale})`)
  dragGhost.appendChild(previewElement.cloneNode(true))
  dragGhostRoot.appendChild(dragGhost)
  document.body.appendChild(dragGhostRoot)
  event.dataTransfer.setDragImage(dragGhostRoot, elementRelativeOffset.x, elementRelativeOffset.y)
  // Remove the ghost element after a short delay, giving the browser time to render it and set
  // the drag image.
  setTimeout(() => dragGhostRoot.remove(), 0)

  event.dataTransfer.effectAllowed = 'move'
  // `dropEffect: none` does not work for removing an element - it disables drop completely.
  event.dataTransfer.dropEffect = 'move'
  const dragItem = props.items[index]!

  const meta: DropMetadata = {
    list: listUuid,
    key: props.getKey?.(dragItem) ?? index,
    size: elementOffsetSize,
  }

  const payload = props.toDragPayload?.(dragItem) ?? ''
  event.dataTransfer.setData(mimeType.value, payload)

  if (props.toPlainText) {
    event.dataTransfer.setData('text/plain', props.toPlainText(dragItem))
  }

  const metaMime = encodeMetadataToMime(meta)
  event.dataTransfer.setData(metaMime, '')
  // The code below will remove the item from list; because doing it in the same frame ends drag
  // immediately, we need to put it in setTimeout (nextTick is not enough).
  setTimeout(() => {
    updateItemBounds()
    draggedIndex.value = index
    dropInfo.value = { meta, position: currentMousePos }
  }, 0)
}

interface DropMetadata {
  list: string
  key: string | number
  size: number
}

function metaEquals(a: DropMetadata, b: DropMetadata) {
  return a.list === b.list && a.key === b.key && a.size === b.size
}

interface DropHoverInfo {
  position: Vec2
  meta: DropMetadata
}

function areaDragOver(e: DragEvent) {
  const metaMime = e.dataTransfer?.types.find((ty) => ty.startsWith(dragMetaMimePrefix))
  const typesMatch = e.dataTransfer?.types.includes(mimeType.value)
  if (!metaMime || (!typesMatch && draggedIndex.value == null)) return
  e.preventDefault()
  const meta = decodeMetadataFromMime(metaMime)
  if (meta == null) return

  const clientPos = new Vec2(e.clientX, e.clientY)
  const position = transformPosition(clientPos)
  const info = dropInfo.value
  if (info != null) {
    if (!metaEquals(info.meta, meta)) info.meta = meta
    if (!info.position.equals(position)) info.position = position
  } else {
    dropInfo.value = { meta, position }
  }
}

const itemAxisBounds = shallowReactive<(Range | undefined)[]>([])
useRaf(() => dropInfo.value != null, updateItemBounds)
function updateItemBounds() {
  itemAxisBounds.length = itemRefs.length
  for (let i = 0; i < itemRefs.length; i++) {
    const item = itemRefs[i]
    const currentRange = itemAxisBounds[i]
    if (item == null) {
      itemAxisBounds[i] = undefined
      continue
    }
    const rect = originalBoundingClientRect.call(item)
    const from = transformPosition(new Vec2(rect.left, rect.top))[props.axis]
    const to = transformPosition(new Vec2(rect.right, rect.bottom))[props.axis]
    if (currentRange?.from !== from || currentRange?.to !== to) {
      itemAxisBounds[i] = Range.tryFromBounds(from, to)
    }
  }
}

function getDropIndex(info: DropHoverInfo, bounds: (Range | undefined)[]): number {
  const pos = info.position
  const insertIndex = bounds.findIndex(
    (range, i) =>
      i !== draggedIndex.value && range != null && (range.from + range.to) / 2 > pos[props.axis],
  )
  return insertIndex >= 0 ? insertIndex : bounds.length
}

function areaDragLeave(_event: DragEvent) {
  dropInfo.value = undefined
}

function areaOnDrop(e: DragEvent) {
  const index = dropIndex.value
  if (index == null || index < 0) return
  e.preventDefault()
  e.stopImmediatePropagation()

  if (draggedIndex.value != null) {
    // draggedIndex works as if the dragged element was still part of the collection.
    // We have to offset it when the element is dragged past its original position.
    const newIndex =
      draggedIndex.value != null && index >= draggedIndex.value + 1 ? index - 1 : index
    if (draggedIndex.value != newIndex) {
      emit('reorder', draggedIndex.value, newIndex)
    }
  } else {
    const payload = e.dataTransfer?.getData(mimeType.value)
    if (payload) emit('dropInsert', index, payload)
  }

  draggedIndex.value = undefined
  dropInfo.value = undefined
}

function onDragEnd(event: DragEvent) {
  const effect = event.dataTransfer?.dropEffect
  if (effect !== 'none' && draggedIndex.value != null) {
    deleteItem(draggedIndex.value)
  }
  draggedIndex.value = undefined
  dropInfo.value = undefined
}

const dragDetected = ref(0)
useEvent(
  window,
  'dragenter',
  (e) => {
    if (e.dataTransfer?.types.includes(mimeType.value)) dragDetected.value += 1
  },
  { capture: true },
)
useEvent(
  window,
  'dragleave',
  (e) => {
    if (e.dataTransfer?.types.includes(mimeType.value)) dragDetected.value -= 1
  },
  { capture: true },
)
useEvent(
  window,
  'drop',
  (e) => {
    if (e.dataTransfer?.types.includes(mimeType.value)) dragDetected.value -= 1
  },
  { capture: true },
)

useAppClass(() => {
  return {
    'list-widget-dragging': dragDetected.value > 0,
  }
})

// FIXME: This is a workaround for a bug in Vue, where the animations are not taking into account
// the CSS transform scale applied to the element. Remove once this is fixed. Reported upstream:
// https://github.com/vuejs/core/issues/9665
const originalBoundingClientRect = Element.prototype.getBoundingClientRect
function patchBoundingClientRectScaling(elem: unknown) {
  if (!(elem instanceof HTMLElement)) return
  if (elem.getBoundingClientRect === originalBoundingClientRect) {
    elem.getBoundingClientRect = function () {
      const rect = originalBoundingClientRect.call(this)
      const scale = rect.width / this.offsetWidth
      if (!Number.isFinite(scale)) return rect
      return new DOMRect(
        rect.left / scale,
        rect.top / scale,
        rect.width / scale,
        rect.height / scale,
      )
    }
  }
}

const itemRefs = shallowReactive<(HTMLElement | null)[]>([])
function setItemRef(el: unknown, index: number) {
  if (el instanceof HTMLElement) {
    patchBoundingClientRectScaling(el)
    itemRefs[index] = el
  }
}

watchPostEffect(() => {
  itemRefs.length = props.items.length
})

function addItem() {
  emit('addItem')
}

function deleteItem(index: number) {
  emit('remove', index)
}

const placeholderSizeProp = computed(() => `--placeholder-${props.axis}` as const)
</script>

<template>
  <TransitionGroup
    tag="ul"
    name="list"
    class="DraggableList"
    :class="{
      animate: dropInfo != null || draggedIndex != null,
      [`axis-${axis}`]: true,
      horizontalScroll,
    }"
    :css="dropInfo != null || draggedIndex != null"
    @pointerdown="
      !$event.shiftKey && !$event.altKey && !$event.metaKey && $event.stopImmediatePropagation()
    "
  >
    <template v-for="entry in displayedChildren" :key="entry.key">
      <template v-if="entry.type === 'item'">
        <li :ref="patchBoundingClientRectScaling" class="item">
          <div :ref="(el) => setItemRef(el, entry.index)" class="draggableContent">
            <SizeTransition width>
              <!-- This wrapper is needed because an SVG element cannot directly be draggable. -->
              <div
                v-if="props.showHandles"
                class="deletable"
                :class="{ hintDeletable: entry.hintDeletable.value }"
                draggable="true"
                @dragstart="onDragStart($event, entry.index)"
                @dragend="onDragEnd"
              >
                <SvgIcon name="grab" class="handle" />
              </div>
            </SizeTransition>
            <div
              class="deletable"
              :class="{ hintDeletable: entry.hintDeletable.value }"
              data-testid="list-item-content"
            >
              <slot :item="entry.item" :index="entry.index"></slot>
            </div>
            <SizeTransition width>
              <!-- This wrapper is needed to animate an `SvgButton` because it ultimately contains a `TooltipTrigger`, which has a fragment root. -->
              <div v-if="props.showHandles" class="iconWrapper">
                <SvgButton
                  class="item-button"
                  name="close"
                  title="Remove item"
                  @activate="deleteItem(entry.index)"
                  @pointerenter="entry.hintDeletable.value = true"
                  @pointerleave="entry.hintDeletable.value = false"
                />
              </div>
            </SizeTransition>
          </div>
          <slot v-if="entry.index != props.items.length - 1" name="separator" />
        </li>
      </template>
      <template v-else>
        <li
          :ref="patchBoundingClientRectScaling"
          data-testid="dragPlaceholder"
          class="placeholder"
          :style="{ [placeholderSizeProp]: entry.size + 'px' }"
        ></li>
      </template>
    </template>
    <SizeTransition key="add-icon" :width="axis === 'x'" :height="axis === 'y'">
      <!-- This wrapper is a workaround: If the `v-if` is applied to the `SvgIcon`, once the button is shown it will
             never go back to hidden. This might be a Vue bug? -->
      <div v-if="props.showHandles" class="iconWrapper axisAligned">
        <SvgButton
          class="item-button after-last-item"
          name="vector_add"
          title="Add a new item"
          @activate="addItem"
        />
      </div>
    </SizeTransition>
    <div
      key="drop-area"
      class="drop-area widgetOutOfLayout"
      @dragleave="areaDragLeave"
      @dragover="areaDragOver"
      @drop="areaOnDrop"
    ></div>
  </TransitionGroup>
</template>

<style scoped>
.DraggableList {
  --base-height: var(--node-port-height);
  display: flex;
  list-style: none;
  --placeholder-x: 0;
  --placeholder-y: 0;

  &.axis-x {
    align-items: stretch;
    flex-direction: row;
  }
  &.axis-y {
    align-items: flex-start;
    flex-direction: column;
  }
}

.DraggableList.animate {
  .placeholder {
    display: flex;
    width: var(--placeholder-x);
    height: var(--placeholder-y);
  }

  .item.list-leave-active {
    position: absolute;
    visibility: hidden;
  }

  .list-move,
  .list-enter-active,
  .list-leave-active {
    transition:
      margin 0.2s ease-in-out,
      width 0.2s ease-in-out,
      transform 0.2s ease-in-out,
      opacity 0.2s ease-in-out;
  }

  .list-enter-from,
  .list-leave-to {
    opacity: 0;
  }
}

.DraggableList.horizontalScroll {
  overflow-x: auto;
}

.App.list-widget-dragging {
  .placeholder.list-enter-from,
  .placeholder.list-leave-to {
    width: 0;
  }
}

div {
  display: inline-block;
}

.item {
  display: flex;
  flex-direction: row;
  align-items: stretch;
}

.draggableContent {
  display: flex;
  flex-direction: row;
  align-items: stretch;
}

.drop-area {
  position: absolute;
  z-index: 10;
}

.App.list-widget-dragging .drop-area {
  inset: -10px 0px;
}

.item {
  position: relative;
}

.handle {
  transition: color 0.2s ease;
  cursor: grab;
  --icon-size: 16px;
  margin: calc((var(--base-height) - var(--icon-size)) / 2) 0;

  &:hover {
    opacity: 0.5;
  }
}

.item:hover {
  z-index: 0;
}

.item-button {
  transition-property: opacity;
  transition-duration: 150ms;
  transition-timing-function: ease-in-out;
  opacity: 0.5;
  transition: margin 0.2s ease-in-out;
  .items:empty + & {
    margin: 0 2px;
  }
  &:hover,
  &:focus,
  &:active {
    opacity: 1;
  }
}

.DraggableList.axis-x .item + .iconWrapper > .after-last-item {
  margin-left: 4px;
}

:global(.draggableList-drag-ghost) {
  position: absolute;
  left: -5000px;
  background-color: var(--color-node-primary);
  border-radius: var(--node-border-radius);
  padding: 4px;
  color: white;
}

.deletable {
  opacity: 1;
  transition: opacity 0.2s ease-in-out;
  &.hintDeletable {
    opacity: 0.3;
  }
}

.iconWrapper {
  flex-direction: row;
  overflow: clip;
  display: flex;
  align-items: center;
  height: var(--base-height);
  justify-content: flex-end;
  .axis-y &.axisAligned {
    flex-direction: column;
  }
}

.SvgButton {
  --color-menu-entry-hover-bg: transparent;
}
</style>
