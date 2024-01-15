<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  functionCallConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { ArgumentInfoKey } from '@/util/callTree'
import { qnJoin, qnSegments, tryQualifiedName } from '@/util/qualifiedName'
import { computed, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

interface Tag {
  label: string
  /** If not set, the value is same as label */
  value?: string
  parameters?: ArgumentWidgetConfiguration[]
}

const staticTags = computed<Tag[]>(() => {
  const tags = props.input[ArgumentInfoKey]?.info?.tagValues
  if (tags == null) return []
  return tags.map((tag) => {
    const qualifiedName = tryQualifiedName(tag)
    if (!qualifiedName.ok) return { kind: 'Static', label: tag }
    const segments = qnSegments(qualifiedName.value).slice(-2)
    if (segments[0] == undefined) return { kind: 'Static', label: tag }
    if (segments[1] == undefined) return { kind: 'Static', label: segments[0] }
    return { label: qnJoin(segments[0], segments[1]) }
  })
})

const dynamicTags = computed<Tag[]>(() => {
  const config = props.input.dynamicConfig
  if (config?.kind !== 'Single_Choice') return []
  return config.values.map((value) => ({
    label: value.label || value.value,
    value: value.value,
    parameters: value.parameters,
  }))
})

const tags = computed(() => (dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value))
const tagLabels = computed(() => tags.value.map((tag) => tag.label))

const selectedIndex = ref<number>()
const selectedTag = computed(() =>
  selectedIndex.value != null ? tags.value[selectedIndex.value] : undefined,
)
const selectedValue = computed(() => {
  if (selectedTag.value == null) return WidgetInput.valueRepr(props.input)
  return selectedTag.value.value ?? selectedTag.value.label
})
const innerWidgetInput = computed(() => {
  if (selectedTag.value == null) return props.input
  const parameters = selectedTag.value.parameters
  if (!parameters) return props.input
  const config = functionCallConfiguration(parameters)
  return { ...props.input, dynamicConfig: config }
})
const showDropdownWidget = ref(false)

function toggleDropdownWidget() {
  showDropdownWidget.value = !showDropdownWidget.value
}

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  props.onUpdate({ type: 'set', value: selectedValue.value, origin: props.input.portId })
  showDropdownWidget.value = false
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 999,
  score: (props) => {
    if (props.input.dynamicConfig?.kind === 'Single_Choice') return Score.Perfect
    if (props.input[ArgumentInfoKey]?.info?.tagValues != null) return Score.Perfect
    return Score.Mismatch
  },
})
</script>

<template>
  <div class="WidgetSelection" @pointerdown.stop="toggleDropdownWidget">
    <NodeWidget :input="innerWidgetInput" />
    <DropdownWidget
      v-if="showDropdownWidget"
      class="dropdownContainer"
      :color="'var(--node-color-primary)'"
      :values="tagLabels"
      :selectedValue="selectedValue"
      @pointerdown.stop
      @click="selectedIndex = $event"
    />
  </div>
</template>

<style scoped>
.WidgetSelection {
  display: flex;
  flex-direction: row;
}
</style>
