<script lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { useAppClass } from '@/providers/appClass'
import { useRaf } from '@/util/animation'
import { useEvent } from '@/util/events'
import { Range } from '@/util/range'
import { Vec2 } from '@/util/vec2'
import { uuidv4 } from 'lib0/random'
import { nextTick } from 'process'
import { computed, ref, shallowReactive, watchEffect, watchPostEffect } from 'vue'
</script>

<script setup lang="ts" generic="T">
const props = defineProps<{
  modelValue: T[]
  default: () => T
  getKey?: (item: T) => string | number | undefined
  /** If present, a {@link DataTransferItem} is added with a MIME type of `text/plain`.
   * This is useful if the drag payload has a representation that can be pasted in terminals,
   * search bars, and/or address bars. */
  toPlainText?: (item: T) => string
  /** The MIME type for the payload output added by `toDragPayload`.
   * Unused if `toDragPayload` is not also present.
   * When in doubt, this should be `application/json`.
   * Defaults to `application/octet-stream`, meaning the payload is arbitrary binary data. */
  dragMimeType?: string
  /** Convert the list item to a drag payload stored under `dragMimeType`. When in doubt, this
   * should be `JSON.stringify` of data describing the object. */
  toDragPayload: (item: T) => string
  /** Convert payload created by `toDragPayload` back to the list item. This function can be called
   * on the payload received from a different application instance (e.g. another browser), so it
   * should not rely on any local state.
   */
  fromDragPayload: (payload: string) => T | undefined
  toDragPosition: (p: Vec2) => Vec2
}>()
const emit = defineEmits<{
  'update:modelValue': [modelValue: T[]]
}>()

const listUuid = uuidv4()

const mimeType = computed(() => props.dragMimeType ?? 'application/octet-stream')

const dragMetaMimePrefix = 'application/x-enso-list-meta'

function stringToHex(str: string) {
  return Array.from(str)
    .map((c) =>
      c.charCodeAt(0) < 128
        ? c.charCodeAt(0).toString(16)
        : encodeURIComponent(c).replace(/%/g, '').toLowerCase(),
    )
    .join('')
}

function hexToString(hex: string) {
  return decodeURIComponent('%' + (hex.match(/.{1,2}/g) ?? []).join('%'))
}

function encodeMetadataToMime(meta: DropMetadata) {
  return `${dragMetaMimePrefix}${stringToHex(JSON.stringify(meta))}`
}

function decodeMetadataFromMime(mime: string): DropMetadata | undefined {
  if (!mime.startsWith(dragMetaMimePrefix)) return undefined
  const rawMeta = hexToString(mime.substring(dragMetaMimePrefix.length))
  return JSON.parse(rawMeta) as DropMetadata
}

const draggedIndex = ref<number>()

type DragItem =
  | {
      type: 'item'
      index: number
      item: T
      key: string | number
    }
  | {
      type: 'placeholder'
      width: number
      key: string | number
    }

const defaultPlaceholderKey = '__placeholder_key__'

const mappedItems = computed(() => {
  return props.modelValue.map(
    (item, index): DragItem => ({
      type: 'item',
      index,
      item,
      key: props.getKey?.(item) ?? index,
    }),
  )
})

const dropInfo = ref<DropHoverInfo>()
const dropIndex = ref<number>()

watchEffect(() => {
  const info = dropInfo.value
  if (info == null) {
    dropIndex.value = undefined
  } else {
    const index = getDropIndex(info, itemHorizontalBounds)
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
      width: meta.width,
      key,
    } as const)
  }
  const x = items.filter((item) => item.type !== 'item' || item.index !== draggedIndex.value)
  return x
})

const rootNode = ref<HTMLElement>()

const cssPropsToCopy = ['--node-color-primary', '--node-color-port', '--node-border-radius']

function onDragStart(event: DragEvent, index: number) {
  if (
    !(event.target instanceof HTMLElement && event.target.nextElementSibling instanceof HTMLElement)
  )
    return

  if (event.dataTransfer) {
    // The dragged widget contents is the next sibling of the drag handle.
    const previewElement = event.target.nextElementSibling

    // Create a fake offscreen DOM element to use as the drag "ghost" image. It will hold a visual
    // clone of the widget being dragged. The ghost style is modified to add a background color
    // and additional border, as well as apply appropriate element scaling in cross-browser way.
    const elementOffsetWidth = previewElement.offsetWidth
    const elementRect = originalBoundingClientRect.call(previewElement)
    const elementScale = elementRect.width / elementOffsetWidth
    const dragGhost = document.createElement('div')
    dragGhost.classList.add('ListWidget-drag-ghost')
    const dragGhostInner = document.createElement('div')
    dragGhost.appendChild(dragGhostInner)
    const previewElementStyle = getComputedStyle(previewElement)
    const elementTopLeft = props.toDragPosition(new Vec2(elementRect.left, elementRect.top))
    const currentMousePos = props.toDragPosition(new Vec2(event.clientX, event.clientY))
    const elementRelativeOffset = currentMousePos.sub(elementTopLeft)
    // To maintain appropriate styling, we have to copy over a set of node tree CSS variables from
    // the original preview element to the ghost element.
    cssPropsToCopy.forEach((prop) => {
      dragGhostInner.style.setProperty(prop, previewElementStyle.getPropertyValue(prop))
    })
    dragGhostInner.style.setProperty('transform', `scale(${elementScale})`)
    dragGhostInner.appendChild(previewElement.cloneNode(true))
    document.body.appendChild(dragGhost)
    event.dataTransfer.setDragImage(dragGhost, elementRelativeOffset.x, elementRelativeOffset.y)
    // Remove the ghost element after a short delay, giving the browser time to render it and set
    // the drag image.
    setTimeout(() => dragGhost.remove(), 0)

    event.dataTransfer.effectAllowed = 'move'
    // `dropEffect: none` does not work for removing an element - it disables drop completely.
    event.dataTransfer.dropEffect = 'move'
    const dragItem = props.modelValue[index]!

    const meta: DropMetadata = {
      list: listUuid,
      key: props.getKey?.(dragItem) ?? index,
      width: elementOffsetWidth,
    }
    const payload = props.toDragPayload(dragItem)
    event.dataTransfer.setData(mimeType.value, payload)

    if (props.toPlainText) {
      event.dataTransfer.setData('text/plain', props.toPlainText(dragItem))
    }

    const metaMime = encodeMetadataToMime(meta)
    event.dataTransfer.setData(metaMime, '')
    nextTick(() => {
      updateItemBounds()
      draggedIndex.value = index
      dropInfo.value = { meta, position: currentMousePos }
    })
  }
}

interface DropMetadata {
  list: string
  key: string | number
  width: number
}

function metaEquals(a: DropMetadata, b: DropMetadata) {
  return a.list === b.list && a.key === b.key && a.width === b.width
}

interface DropHoverInfo {
  position: Vec2
  meta: DropMetadata
}

function areaDragOver(e: DragEvent) {
  const metaMime = e.dataTransfer?.types.find((ty) => ty.startsWith(dragMetaMimePrefix))
  const typesMatch = e.dataTransfer?.types.includes(mimeType.value)
  if (!metaMime || !typesMatch) return
  const meta = decodeMetadataFromMime(metaMime)
  if (meta == null) return

  const clientPos = new Vec2(e.clientX, e.clientY)
  const position = props.toDragPosition(clientPos)
  const info = dropInfo.value
  if (info != null) {
    if (!metaEquals(info.meta, meta)) info.meta = meta
    if (!info.position.equals(position)) info.position = position
  } else {
    dropInfo.value = { meta, position }
  }
}

const itemHorizontalBounds = shallowReactive<(Range | undefined)[]>([])
useRaf(() => dropInfo.value != null, updateItemBounds)
function updateItemBounds() {
  itemHorizontalBounds.length = itemRefs.length
  for (let i = 0; i < itemRefs.length; i++) {
    const item = itemRefs[i]
    const currentRange = itemHorizontalBounds[i]
    if (item == null) {
      itemHorizontalBounds[i] = undefined
      continue
    }
    const rect = originalBoundingClientRect.call(item)
    const start = props.toDragPosition(new Vec2(rect.left, rect.top)).x
    const end = props.toDragPosition(new Vec2(rect.right, rect.bottom)).x
    if (currentRange?.start !== start || currentRange?.end !== end) {
      itemHorizontalBounds[i] = new Range(start, end)
    }
  }
}

function getDropIndex(info: DropHoverInfo, bounds: (Range | undefined)[]): number {
  const pos = info.position
  const insertIndex = bounds.findIndex(
    (range) => range != null && (range.start + range.end) / 2 > pos.x,
  )
  return insertIndex >= 0 ? insertIndex : bounds.length
}

function areaDragLeave(_event: DragEvent) {
  dropInfo.value = undefined
}

function areaOnDrop(e: DragEvent) {
  const payload = e.dataTransfer?.getData(mimeType.value)
  const index = dropIndex.value
  if (index == null || index < 0 || payload == null) return
  e.preventDefault()
  e.stopImmediatePropagation()

  const item = props.fromDragPayload(payload)
  if (item != null) {
    let modelValue = [...props.modelValue]
    let insertIndex = index
    if (draggedIndex.value != null) {
      if (draggedIndex.value <= insertIndex) insertIndex -= 1
      modelValue = modelValue.filter((_, i) => i !== draggedIndex.value)
    }
    modelValue.splice(insertIndex, 0, item)
    emit('update:modelValue', modelValue)
  }

  draggedIndex.value = undefined
  dropInfo.value = undefined
}

function onDragEnd(event: DragEvent) {
  const effect = event.dataTransfer?.dropEffect
  if (effect !== 'none' && draggedIndex.value != null) {
    let modelValue = props.modelValue.filter((_, i) => i !== draggedIndex.value)
    emit('update:modelValue', modelValue)
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
  itemRefs.length = props.modelValue.length
})
</script>

<template>
  <div
    ref="rootNode"
    class="VectorWidget"
    :class="{ animate: dropInfo != null || draggedIndex != null }"
    @pointerdown="
      !$event.shiftKey && !$event.altKey && !$event.metaKey && $event.stopImmediatePropagation()
    "
  >
    <div class="vector-literal literal">
      <span class="token">[</span>
      <TransitionGroup
        tag="ul"
        name="list"
        class="items"
        :css="dropInfo != null || draggedIndex != null"
      >
        <template v-for="entry in displayedChildren" :key="entry.key">
          <template v-if="entry.type === 'item'">
            <li :ref="(el) => setItemRef(el, entry.index)" class="item">
              <div
                class="handle"
                draggable="true"
                @dragstart="onDragStart($event, entry.index)"
                @dragend="onDragEnd"
              ></div>
              <slot :item="entry.item"></slot>
            </li>
            <li
              v-show="entry.index != props.modelValue.length - 1"
              :ref="patchBoundingClientRectScaling"
              class="token"
            >
              ,&nbsp;
            </li>
          </template>
          <template v-else>
            <li
              :ref="patchBoundingClientRectScaling"
              class="placeholder"
              :style="{ '--placeholder-width': entry.width + 'px' }"
            ></li>
          </template>
        </template>
      </TransitionGroup>
      <SvgIcon
        class="add-item"
        name="vector_add"
        @pointerdown="
          !$event.ctrlKey &&
            !$event.shiftKey &&
            !$event.altKey &&
            !$event.metaKey &&
            $event.stopImmediatePropagation()
        "
        @click="emit('update:modelValue', [...props.modelValue, props.default()])"
      />
      <span class="token">]</span>
    </div>
    <div
      class="drop-area"
      @dragleave="areaDragLeave"
      @dragover="areaDragOver"
      @drop="areaOnDrop"
    ></div>
  </div>
</template>

<style scoped>
.VectorWidget.animate {
  .placeholder {
    display: flex;
    width: var(--placeholder-width);
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

.App.list-widget-dragging {
  .placeholder.list-enter-from,
  .placeholder.list-leave-to {
    width: 0;
  }
}

.vector-literal {
  display: flex;
  align-items: center;
}

.items {
  display: flex;
}

.item {
  display: flex;
  align-items: center;
}

.item .preview {
  background: var(--node-color-primary);
  padding: 4px;
  border-radius: var(--node-border-radius);
}

.token {
  color: rgb(255 255 255 / 0.33);
  vertical-align: middle;
  align-items: center;
  display: inline-flex;
  user-select: none;
  height: 24px;
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
  position: absolute;
  display: block;
  left: -6px;
  height: calc(100% - 12px);
  width: 2px;
  box-shadow:
    2px 0 0 transparent,
    -2px 0 0 transparent;
  transition: box-shadow 0.2s ease;
  pointer-events: none;

  &:before {
    content: '';
    opacity: 0;
    transition: opacity 0.2s ease;
    position: absolute;
    display: block;
    left: -8px;
    right: -16px;
    top: -3px;
    bottom: -3px;
    border-radius: var(--node-border-radius) 0 0 var(--node-border-radius);
    background-color: var(--node-color-primary);
    z-index: -1;
  }
}

.item:hover {
  z-index: 0;
}

.item:hover .handle {
  box-shadow:
    2px 0 0 rgb(255 255 255 / 0.5),
    -2px 0 0 rgb(255 255 255 / 0.5);

  &:hover {
    box-shadow:
      2px 0 0 rgb(255 255 255 / 0.8),
      -2px 0 0 rgb(255 255 255 / 0.8);
  }

  background: var(--node-color-primary);
  pointer-events: all;

  &:before {
    opacity: 0.5;
  }

  &:after {
    content: '';
    position: absolute;
    display: block;
    left: -1px;
    right: -4px;
    top: -3px;
    bottom: -3px;
    z-index: 1;
  }
}

.item:hover .handle:hover::before {
  opacity: 1;
}

.GraphEditor.draggingEdge .handle {
  display: none;
}

.add-item {
  transition-property: opacity;
  transition-duration: 150ms;
  transition-timing-function: ease-in-out;
  opacity: 0.5;
  margin-left: 4px;
  transition: margin 0.2s ease-in-out;
  .items:empty + & {
    margin: 0 2px;
  }
}

.add-item:hover {
  opacity: 1;
}

:global(.ListWidget-drag-ghost) {
  position: absolute;
  left: -5000px;
}
:global(.ListWidget-drag-ghost > div) {
  background-color: var(--node-color-primary);
  border-radius: var(--node-border-radius);
  padding: 4px;
  color: white;
}
</style>
