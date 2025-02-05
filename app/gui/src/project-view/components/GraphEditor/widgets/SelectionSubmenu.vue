<script setup lang="ts">
import SizeTransition from '@/components/SizeTransition.vue'
import DropdownWidget, { DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { unrefElement } from '@/composables/events'
import { targetIsOutside } from '@/util/autoBlur'
import { computed, ComputedRef, ref, useTemplateRef, watch } from 'vue'
import { submenuDropdownStyles } from './dropdownStyles'
import { Entry, ExpressionTag, isEntry, NestedChoiceTag } from './dropdownTags'

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

watch(
  () => props.show,
  () => {
    submenu.value = null
  },
)

const dropdownElement = useTemplateRef('dropdownElement')
const floatReference = computed(() => props.floatReference)

const element = computed(() => (dropdownElement.value ? dropdownElement.value : undefined))
const rootElement = computed(() => props.rootElement)
const { floatingStyles } = submenuDropdownStyles(
  floatReference,
  element,
  props.topLevel,
  rootElement,
)

function isTargetOutside(event: Event) {
  return (
    targetIsOutside(event, unrefElement(dropdownElement)) &&
    (submenuRef.value != null ?
      (
        'isTargetOutside' in submenuRef.value &&
        typeof submenuRef.value.isTargetOutside === 'function'
      ) ?
        submenuRef.value.isTargetOutside(event)
      : true
    : true)
  )
}

defineExpose({
  dropdownElement,
  isTargetOutside,
})

export interface Submenu {
  entries: ComputedRef<Entry[]>
  relativeTo: HTMLElement
}

const submenu = ref<Submenu | null>(null)
const submenuEntries = computed(() => submenu.value?.entries ?? [])
const submenuRef = useTemplateRef('submenuRef')

const nestedEntriesPresent = computed(() =>
  props.entries.some((entry) => isEntry(entry) && entry.tag instanceof NestedChoiceTag),
)

function onClick(entry: DropdownEntry, keepOpen: boolean, target: HTMLElement) {
  if (!isEntry(entry)) return
  const tag = entry.tag
  if (tag instanceof NestedChoiceTag) {
    submenu.value = {
      entries: computed(
        () =>
          tag.choices.map((choice) => ({
            value: choice.label,
            selected:
              choice instanceof ExpressionTag && props.selectedExpressions.has(choice.expression),
            tag: choice,
          })) satisfies Entry[],
      ),
      relativeTo: target,
    }
  } else {
    emit('clickedEntry', entry, keepOpen)
  }
}

function onScroll() {
  submenu.value = null
}
</script>

<template>
  <Teleport v-if="props.rootElement" :to="props.rootElement">
    <div ref="dropdownElement" :style="floatingStyles" class="SelectionSubmenu widgetOutOfLayout">
      <SizeTransition height :duration="100">
        <DropdownWidget
          v-if="props.show"
          :class="{ TopLevelDropdown: props.topLevel }"
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
