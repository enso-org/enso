<script setup lang="ts">
import SizeTransition from '@/components/SizeTransition.vue'
import DropdownWidget, { DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { unrefElement } from '@/composables/events'
import { targetIsOutside } from '@/util/autoBlur'
import { computed, ComputedRef, ref, useTemplateRef, watch } from 'vue'
import { submenuDropdownStyles } from './styles'
import { Entry, ExpressionTag, isEntry, NestedChoiceTag } from './tags'

const props = defineProps<{
  rootElement: HTMLElement | undefined
  floatReference: HTMLElement | undefined
  show: boolean
  entries: Entry[]
  selectedExpressions: Set<string>
  topLevel?: boolean
}>()

const emit = defineEmits<{
  clickedEntry: [Entry, boolean]
}>()

export interface Submenu {
  entries: ComputedRef<Entry[]>
  relativeTo: HTMLElement
}

/** Referring to the type of the component in the current file is hard, so we define a helper type. */
interface SubmenuComponent {
  isTargetOutside: (event: Event) => boolean
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
const floatReference = computed(() => props.floatReference)
const rootElement = computed(() => props.rootElement)

const { floatingStyles } = submenuDropdownStyles(
  floatReference,
  dropdownElement,
  props.topLevel,
  rootElement,
)

const nestedEntriesPresent = computed(() =>
  props.entries.some((entry) => isEntry(entry) && entry.tag instanceof NestedChoiceTag),
)

function resetSubmenu() {
  submenu.value = null
}
watch([() => props.show, () => props.entries], resetSubmenu)

function nestedChoiceTagToSubmenu(tag: NestedChoiceTag, target: HTMLElement): Submenu {
  const isSelected = (tag: ExpressionTag | NestedChoiceTag) =>
    tag instanceof ExpressionTag && props.selectedExpressions.has(tag.expression)
  const choiceToEntry = (choice: ExpressionTag | NestedChoiceTag): Entry => ({
    value: choice.label,
    selected: isSelected(choice),
    tag: choice,
  })

  return {
    entries: computed(() => tag.choices.map(choiceToEntry) satisfies Entry[]),
    relativeTo: target,
  }
}

function onClick(entry: DropdownEntry, keepOpen: boolean, htmlElement: HTMLElement) {
  if (!isEntry(entry)) return
  const tag = entry.tag
  if (tag instanceof NestedChoiceTag) {
    submenu.value = nestedChoiceTagToSubmenu(tag, htmlElement)
  } else {
    emit('clickedEntry', entry, keepOpen)
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
</script>

<template>
  <Teleport v-if="props.rootElement" :to="props.rootElement">
    <div ref="dropdownElement" :style="floatingStyles" class="SelectionSubmenu widgetOutOfLayout">
      <SizeTransition height :duration="100">
        <DropdownWidget
          v-if="props.show"
          :class="{ ExtendUpwards: props.topLevel }"
          color="var(--color-node-text)"
          backgroundColor="var(--color-node-background)"
          :entries="entries"
          @clickEntry="onClick"
          @scroll="onScroll"
        />
      </SizeTransition>
    </div>
  </Teleport>
  <SelectionSubmenu
    v-if="nestedEntriesPresent"
    ref="submenuRef"
    :rootElement="props.rootElement"
    :floatReference="submenu?.relativeTo"
    :show="props.show && submenu != null"
    :entries="submenuEntries"
    :selectedExpressions="props.selectedExpressions"
    @clickedEntry="(entry, keepOpen) => emit('clickedEntry', entry, keepOpen)"
  />
</template>

<style scoped>
.SelectionSubmenu {
  z-index: 21;
}
</style>
