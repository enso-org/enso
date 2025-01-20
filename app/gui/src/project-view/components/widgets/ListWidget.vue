<script setup lang="ts" generic="T">
import SizeTransition from '@/components/SizeTransition.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useRaf } from '@/composables/animation'
import { useEvent } from '@/composables/events'
import { useAppClass } from '@/providers/appClass'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Range } from '@/util/data/range'
import { Vec2 } from '@/util/data/vec2'
import { uuidv4 } from 'lib0/random'
import { computed, type Ref, ref, shallowReactive, watchEffect, watchPostEffect } from 'vue'

const props = defineProps<{
  modelValue: T[]
  newItem: () => T | undefined
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
  toDragPayload: (item: T) => string
  /**
   * Convert payload created by `toDragPayload` back to the list item. This function can be called
   * on the payload received from a different application instance (e.g. another browser), so it
   * should not rely on any local state.
   */
  fromDragPayload: (payload: string) => T | undefined
  toDragPosition: (p: Vec2) => Vec2
}>()
const emit = defineEmits<{
  'update:modelValue': [modelValue: T[]]
}>()

const tree = injectWidgetTree()

const listUuid = uuidv4()

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
  width: number
}

type DragItem = NonPlaceholderItem | PlaceholderItem

const defaultPlaceholderKey = '__placeholder_key__'

const mappedItems = computed<DragItem[]>(() => {
  return props.modelValue.map((item, index) => ({
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
  return items.filter((item) => item.type !== 'item' || item.index !== draggedIndex.value)
})

const rootNode = ref<HTMLElement>()

const cssPropsToCopy = ['--color-node-primary', '--color-node-port', '--node-border-radius']

function onDragStart(event: DragEvent, index: number) {
  if (!event.dataTransfer) return
  if (!(event.target instanceof HTMLElement)) return
  // The element that will be shown following the mouse.
  const previewElement = event.target.parentElement
  if (!(previewElement instanceof HTMLElement)) return
  // The element being replaced with a placeholder during the operation.
  const sizeElement = previewElement.parentElement
  if (!(sizeElement instanceof HTMLElement)) return

  // Create a fake offscreen DOM element to use as the drag "ghost" image. It will hold a visual
  // clone of the widget being dragged. The ghost style is modified to add a background color
  // and additional border, as well as apply appropriate element scaling in cross-browser way.
  const elementOffsetWidth = sizeElement.offsetWidth
  const elementRect = originalBoundingClientRect.call(sizeElement)
  const elementScale = elementRect.width / elementOffsetWidth
  const dragGhost = document.createElement('div')
  dragGhost.classList.add('ListWidget-drag-ghost')
  const previewElementStyle = getComputedStyle(previewElement)
  const elementTopLeft = props.toDragPosition(new Vec2(elementRect.left, elementRect.top))
  const currentMousePos = props.toDragPosition(new Vec2(event.clientX, event.clientY))
  const elementRelativeOffset = currentMousePos.sub(elementTopLeft)
  // To maintain appropriate styling, we have to copy over a set of node tree CSS variables from
  // the preview element to the ghost element.
  cssPropsToCopy.forEach((prop) => {
    dragGhost.style.setProperty(prop, previewElementStyle.getPropertyValue(prop))
  })
  dragGhost.style.setProperty('transform', `scale(${elementScale})`)
  dragGhost.appendChild(previewElement.cloneNode(true))
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
  e.preventDefault()
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
    const modelValue = props.modelValue.filter((_, i) => i !== draggedIndex.value)
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

function addItem() {
  const item = props.newItem()
  if (item) emit('update:modelValue', [...props.modelValue, item])
}

function deleteItem(index: number) {
  const modelValue = props.modelValue.filter((_, i) => i !== index)
  emit('update:modelValue', modelValue)
}
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
    <div class="vector-literal">
      <span class="token widgetApplyPadding">[</span>
      <TransitionGroup
        tag="ul"
        name="list"
        class="items"
        :css="dropInfo != null || draggedIndex != null"
      >
        <template v-for="entry in displayedChildren" :key="entry.key">
          <template v-if="entry.type === 'item'">
            <li :ref="patchBoundingClientRectScaling" class="item">
              <div :ref="(el) => setItemRef(el, entry.index)" class="draggableContent">
                <SizeTransition width>
                  <!-- This wrapper is needed because an SVG element cannot directly be draggable. -->
                  <div
                    v-if="tree.extended"
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
                  <slot :item="entry.item"></slot>
                </div>
                <SizeTransition width>
                  <!-- This wrapper is needed to animate an `SvgButton` because it ultimately contains a `TooltipTrigger`,
                       which has a fragment root. -->
                  <div v-if="tree.extended" class="displayContents">
                    <SvgButton
                      class="item-button"
                      name="close"
                      title="Remove item"
                      @click.stop="deleteItem(entry.index)"
                      @pointerenter="entry.hintDeletable.value = true"
                      @pointerleave="entry.hintDeletable.value = false"
                    />
                  </div>
                </SizeTransition>
              </div>
              <div
                v-if="entry.index != props.modelValue.length - 1"
                class="token widgetApplyPadding"
              >
                ,&nbsp;
              </div>
            </li>
          </template>
          <template v-else>
            <li
              :ref="patchBoundingClientRectScaling"
              data-testid="dragPlaceholder"
              class="placeholder"
              :style="{ '--placeholder-width': entry.width + 'px' }"
            ></li>
          </template>
        </template>
      </TransitionGroup>
      <SizeTransition width>
        <!-- This wrapper is a workaround: If the `v-if` is applied to the `SvgIcon`, once the button is shown it will
             never go back to hidden. This might be a Vue bug? -->
        <div v-if="tree.extended" class="displayContents">
          <SvgButton
            class="item-button after-last-item"
            name="vector_add"
            title="Add a new item"
            @click.stop="addItem"
          />
        </div>
      </SizeTransition>
      <span class="token widgetApplyPadding">]</span>
    </div>
    <div
      class="drop-area widgetOutOfLayout"
      @dragleave="areaDragLeave"
      @dragover="areaDragOver"
      @drop="areaOnDrop"
    ></div>
  </div>
</template>

<style scoped>
.VectorWidget {
  display: flex;
  flex-direction: row;
  align-items: center;
}

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

div {
  display: inline-block;
}

.vector-literal {
  display: flex;
  align-items: center;
}

.items {
  display: flex;
  flex-direction: row;
  align-items: center;
  list-style: none;
}

.item {
  display: flex;
  flex-direction: row;
  align-items: center;
}

.draggableContent {
  display: flex;
  flex-direction: row;
  align-items: center;
}

.token {
  opacity: 0.33;
  user-select: none;
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

  color: var(--color-widget-unfocus);

  &:hover {
    color: var(--color-widget-focus);
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
  &:hover {
    opacity: 1;
  }
}

.after-last-item {
  margin-left: 4px;
}

:global(.ListWidget-drag-ghost) {
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

.displayContents {
  display: contents;
}

.SvgButton {
  --color-menu-entry-hover-bg: transparent;
}
</style>
