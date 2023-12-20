<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  functionCallConfiguration,
  type ArgumentWidgetConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { SoCalledExpression } from '@/util/callTree'
import { qnJoin, qnSegments, tryQualifiedName } from '@/util/qualifiedName'
import { computed, nextTick, onMounted, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

interface Tag {
  label: string
  /** If not set, the value is same as label */
  value?: string
  parameters?: ArgumentWidgetConfiguration[]
}

const staticTags = computed<Tag[]>(() => {
  const tags = props.input.arg?.info?.tagValues
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
  const config = props.input.widgetConfig
  if (config?.kind !== 'Single_Choice') return []
  return config.values.map((value) => ({
    label: value.label || value.value,
    value: value.value,
    parameters: value.parameters,
  }))
})

const tags = computed(() => (dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value))
const tagLabels = computed(() => tags.value.map((tag) => tag.label))

const rootElement = ref<HTMLElement>()
const parentColor = ref<string>()

onMounted(async () => {
  await nextTick()
  if (rootElement.value != null) {
    parentColor.value = getComputedStyle(rootElement.value).getPropertyValue('--node-color-primary')
  }
})

const selectedIndex = ref<number>()
const selectedTag = computed(() =>
  selectedIndex.value != null ? tags.value[selectedIndex.value] : undefined,
)
const selectedValue = computed(() => {
  if (selectedTag.value == null) return props.input.arg?.info?.defaultValue ?? ''
  return selectedTag.value.value ?? selectedTag.value.label
})
const selectedLabel = computed(() => {
  if (selectedTag.value == null) return props.input.arg?.info?.defaultValue ?? ''
  return selectedTag.value.label
})
const innerWidgetInput = computed(() => {
  if (selectedTag.value == null) return props.input
  const parameters = selectedTag.value.parameters
  if (!parameters) return props.input
  return new SoCalledExpression(
    props.input.ast,
    functionCallConfiguration(parameters),
    // Not sure if we should pass this to inner widget: should it be aware it's an argument?
    props.input.arg,
  )
})
const showDropdownWidget = ref(false)

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  // TODO: Handle the case for ArgumentPlaceholder once the AST has been updated,
  const id = props.input.ast?.astId
  const expression = selectedValue.value
  if (id) graph.setExpressionContent(id, expression)
  showDropdownWidget.value = false
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(SoCalledExpression, {
  priority: 999,
  score: (props) => {
    if (props.input.widgetConfig?.kind === 'Single_Choice') return Score.Perfect
    if (props.input.arg?.info.tagValues != null) return Score.Perfect
    return Score.Mismatch
  },
})
</script>

<template>
  <div ref="rootElement" class="WidgetRoot">
    <span
      class="SelectionWidgetArgumentValue"
      @pointerdown="showDropdownWidget = !showDropdownWidget"
    >
      <NodeWidget :input="innerWidgetInput" />
      <template v-if="props.input.isPlaceholder()">
        <span class="SelectionWidgetArgumentValue"> {{ selectedValue }} </span>
      </template>
    </span>
    <div class="SelectionWidgetSingleChoice">
      <DropdownWidget
        v-if="showDropdownWidget"
        :color="parentColor ?? 'white'"
        :values="tagLabels"
        :selectedValue="selectedLabel"
        @click="selectedIndex = $event"
      />
    </div>
  </div>
</template>
<style scoped>
.SelectionWidgetArgumentValue {
  margin-left: 8px;
}
.SelectionWidgetSingleChoice {
  position: absolute;
  top: 100%;
  margin-top: 4px;
}
</style>
