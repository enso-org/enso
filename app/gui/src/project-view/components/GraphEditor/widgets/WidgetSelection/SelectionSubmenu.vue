<script setup lang="ts" generic="T extends DropdownEntry | SubmenuEntry<T>">
import ConditionalTeleport from '@/components/ConditionalTeleport.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import DropdownWidget, { DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { unrefElement } from '@/composables/events'
import { targetIsOutside } from '@/util/autoBlur'
import { Opt } from '@/util/data/opt'
import { computed, ComputedRef, ref, toRef, useTemplateRef, watch } from 'vue'
import { submenuDropdownStyles } from './styles'
import { isSubmenuEntry, type SubmenuEntry } from './submenuEntry'

const { extendUpwards = true, ...props } = defineProps<{
  rootElement: Opt<HTMLElement>
  floatReference: Opt<HTMLElement>
  show: boolean
  entries: T[]
  topLevel?: boolean
  extendUpwards?: boolean
  color?: string | undefined
  backgroundColor?: string | undefined
}>()
const floatReference = toRef(props, 'floatReference')
const rootElement = toRef(props, 'rootElement')

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
    submenu.value = nestedEntryToSubmenu(entry, htmlElement)
  } else {
    emit('clickedEntry', entry as T, keepOpen)
  }
}

function onScroll() {
  submenu.value = null
}

/** Check if the event target is outside the current submenu and any of its descendants. */
function isTargetOutside(event: Event) {
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
  <ConditionalTeleport :target="props.rootElement">
    <div
      ref="dropdownElement"
      :style="floatingStyles"
      class="SelectionSubmenu widgetOutOfLayout"
      v-bind="$attrs"
    >
      <SizeTransition height :duration="100">
        <DropdownWidget
          v-if="props.show"
          :class="{ ExtendUpwards: props.topLevel && extendUpwards }"
          :color="props.color ?? 'var(--color-node-text)'"
          :backgroundColor="props.backgroundColor ?? 'var(--color-node-background)'"
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
    :rootElement="props.rootElement"
    :floatReference="submenu?.relativeTo"
    :show="props.show && submenu != null"
    :entries="submenuEntries"
    :color="props.color ?? 'var(--color-node-text)'"
    :backgroundColor="props.backgroundColor ?? 'var(--color-node-background)'"
    @clickedEntry="(entry, keepOpen) => emit('clickedEntry', entry, keepOpen)"
  />
</template>

<style scoped>
.SelectionSubmenu {
  z-index: 21;
}
</style>
