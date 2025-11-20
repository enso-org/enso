<script setup lang="ts" generic="T extends DropdownEntry | SubmenuEntry<T>">
import ConditionalTeleport from '@/components/ConditionalTeleport.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import DropdownWidget, { type DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { unrefElement } from '@/composables/events'
import { usePopoverRoot } from '@/providers/popoverRoot'
import { targetIsOutside } from '@/util/autoBlur'
import type { Opt } from '@/util/data/opt'
import { computed, nextTick, ref, toRef, useTemplateRef, watch, type ComputedRef } from 'vue'
import { submenuDropdownStyles } from './styles'
import { isSubmenuEntry, type SubmenuEntry } from './submenuEntry'

const props = defineProps<{
  floatReference: Opt<HTMLElement>
  show: boolean
  entries: T[]
  topLevel?: boolean
  color?: string | undefined
  backgroundColor?: string | undefined
}>()
const floatReference = toRef(props, 'floatReference')
const rootElement = usePopoverRoot(true)

const emit = defineEmits<{
  clickedEntry: [T, boolean]
}>()

interface Submenu {
  entries: ComputedRef<T[]>
  relativeTo: HTMLElement
}

function isSubmenuComponent(component: unknown): component is SubmenuComponent {
  return (
    component != null &&
    typeof component === 'object' &&
    'isTargetOutside' in component &&
    typeof component.isTargetOutside === 'function'
  )
}

const submenu = ref<Submenu | null>(null)
const submenuEntries = computed(() => submenu.value?.entries ?? [])
const submenuRef = useTemplateRef('submenuRef')

const dropdownElement = useTemplateRef('dropdownElement')

const { floatingStyles } = submenuDropdownStyles(
  floatReference,
  dropdownElement,
  props.topLevel,
  rootElement,
)

const nestedEntriesPresent = computed(() =>
  props.entries.some((entry) => 'isNested' in entry && entry.isNested),
)

function resetSubmenu() {
  submenu.value = null
}
watch([() => props.show, () => props.entries], resetSubmenu)

function nestedEntryToSubmenu(entry: SubmenuEntry<T>, target: HTMLElement): Submenu {
  return {
    entries: computed(() => entry.nestedValues),
    relativeTo: target,
  }
}

function onClick(entry: T, keepOpen: boolean, htmlElement: HTMLElement) {
  if (isSubmenuEntry(entry) && entry.isNested) {
    // Change submenu in two steps to trigger the size transition.
    submenu.value = null
    nextTick(() => (submenu.value = nestedEntryToSubmenu(entry, htmlElement)))
  } else {
    emit('clickedEntry', entry as T, keepOpen)
  }
}

function onScroll() {
  submenu.value = null
}

/** Check if the event target is outside the current submenu and any of its descendants. */
function isTargetOutside(event: Event): boolean {
  const isOutsideCurrent = targetIsOutside(event, unrefElement(dropdownElement))
  const isOutsideSubmenu =
    isSubmenuComponent(submenuRef.value) ? submenuRef.value.isTargetOutside(event) : true
  return isOutsideCurrent && isOutsideSubmenu
}

defineExpose({
  isTargetOutside,
})

defineOptions({
  inheritAttrs: false,
})
</script>

<script lang="ts">
/** Referring to the type of the component in the current file is hard, so we define a helper type. */
export interface SubmenuComponent {
  isTargetOutside: (event: Event) => boolean
}
</script>

<template>
  <ConditionalTeleport :target="rootElement">
    <div
      ref="dropdownElement"
      :style="floatingStyles"
      class="SelectionSubmenu widgetOutOfLayout"
      v-bind="$attrs"
    >
      <SizeTransition height :duration="100">
        <DropdownWidget
          v-if="props.show"
          class="outlined"
          :entries="entries"
          @clickEntry="onClick"
          @scroll="onScroll"
        />
      </SizeTransition>
    </div>
  </ConditionalTeleport>
  <SelectionSubmenu
    v-if="nestedEntriesPresent"
    ref="submenuRef"
    :floatReference="submenu?.relativeTo"
    :show="props.show && submenu != null"
    :entries="submenuEntries"
    @clickedEntry="(entry, keepOpen) => emit('clickedEntry', entry, keepOpen)"
  />
</template>

<style scoped>
.SelectionSubmenu {
  z-index: var(--z-index-selection-submenu);
}

.outlined {
  border: 1px solid color-mix(in oklab, var(--dropdown-bg), black 5%);
}
</style>
