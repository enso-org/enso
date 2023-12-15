<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { ArgumentAst, ArgumentPlaceholder } from '@/util/callTree'
import { qnJoin, qnSegments, tryQualifiedName } from '@/util/qualifiedName'
import { computed, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

/** Static selection entry, label and value are the same. */
interface StaticTag {
  kind: 'Static'
  label: string
}

/** Dynamic selection entry, label and value can be different. */
interface DynamicTag {
  kind: 'Dynamic'
  label: string
  value: string
}
type Tag = StaticTag | DynamicTag

const staticTags = computed<Tag[]>(() => {
  const tags = props.input.info?.tagValues
  if (tags == null) return []
  return tags.map((tag) => {
    const qualifiedName = tryQualifiedName(tag)
    if (!qualifiedName.ok) return { kind: 'Static', label: tag }
    const segments = qnSegments(qualifiedName.value).slice(-2)
    if (segments[0] == undefined) return { kind: 'Static', label: tag }
    if (segments[1] == undefined) return { kind: 'Static', label: segments[0] }
    return { kind: 'Static', label: qnJoin(segments[0], segments[1]) }
  })
})

const dynamicTags = computed<Tag[]>(() => {
  const config = props.config
  if (config == null) return []
  const [_, widgetConfig] = config.find(([name]) => name === props.input.info?.name) ?? []
  if (widgetConfig && widgetConfig.kind == 'Single_Choice') {
    return widgetConfig.values.map((value) => ({
      kind: 'Dynamic',
      label: value.label || value.value,
      value: value.value,
    }))
  } else {
    return []
  }
})

const tags = computed(() => (dynamicTags.value.length > 0 ? dynamicTags.value : staticTags.value))
const tagLabels = computed(() => tags.value.map((tag) => tag.label))
const tagValues = computed(() => {
  return tags.value.map((tag) => (tag.kind == 'Static' ? tag.label : tag.value))
})

const selectedIndex = ref<number>()
const selectedValue = computed(() => {
  if (selectedIndex.value == null) return props.input.info?.defaultValue ?? ''
  return tagValues.value[selectedIndex.value] ?? ''
})
const showDropdownWidget = ref(false)

function toggleDropdownWidget() {
  showDropdownWidget.value = !showDropdownWidget.value
}

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  // TODO: Handle the case for ArgumentPlaceholder once the AST has been updated,
  const id = props.input instanceof ArgumentAst ? props.input.ast.exprId : undefined
  const expression = selectedValue.value ?? ''
  if (id) props.onUpdate(expression, id)
  showDropdownWidget.value = false
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget([ArgumentPlaceholder, ArgumentAst], {
  priority: 999,
  score: (props) => {
    const tags = props.input.info?.tagValues
    const [_, dynamicConfig] = props.config?.find(([name]) => name === props.input.info?.name) ?? []
    const isSuitableDynamicConfig = dynamicConfig && dynamicConfig.kind === 'Single_Choice'
    if (tags == null && !isSuitableDynamicConfig) return Score.Mismatch
    return Score.Perfect
  },
})
</script>

<template>
  <div class="WidgetSelection" @pointerdown="toggleDropdownWidget">
    <NodeWidget :input="props.input" />
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
