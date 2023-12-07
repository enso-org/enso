<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { qnJoin, qnSegments, tryQualifiedName } from '@/util/qualifiedName.ts'
import { computed, nextTick, onMounted, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

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

const rootElement = ref<HTMLElement>()
const parentColor = ref<string>()

onMounted(async () => {
  await nextTick()
  if (rootElement.value != null) {
    parentColor.value = getComputedStyle(rootElement.value).getPropertyValue('--node-color-primary')
  }
})

const selectedIndex = ref<number>()
const selectedValue = computed(() => {
  if (selectedIndex.value == null) return props.input.info?.defaultValue ?? ''
  return tagValues.value[selectedIndex.value] ?? ''
})
const selectedLabel = computed(() => {
  if (selectedIndex.value == null) return props.input.info?.defaultValue ?? ''
  return tagLabels.value[selectedIndex.value] ?? ''
})
const showDropdownWidget = ref(false)
const showArgumentValue = ref(true)

// When the selected index changes, we update the expression content.
watch(selectedIndex, (_index) => {
  // TODO: Handle the case for ArgumentPlaceholder once the AST has been updated,
  const id = props.input instanceof ArgumentAst ? props.input.ast.astId : undefined
  const expression = selectedValue.value ?? ''
  if (id) graph.setExpressionContent(id, expression)
  showDropdownWidget.value = false
})
</script>

<script lang="ts">
import { defineWidget, Score } from '@/providers/widgetRegistry.ts'
import { ArgumentAst, ArgumentPlaceholder } from '@/util/callTree.ts'

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
  <div ref="rootElement" class="WidgetRoot">
    <span class="WidgetArgumentName" @pointerdown="showDropdownWidget = !showDropdownWidget">
      <template v-if="showArgumentValue">
        <NodeWidget :input="props.input" /><span class="value"> {{ selectedValue }} </span>
      </template>
      <template v-else><NodeWidget :input="props.input" /></template>
    </span>
    <div class="WidgetSingleChoice">
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
.value {
  margin-left: 8px;
}
.WidgetSingleChoice {
  position: absolute;
  top: 100%;
  margin-top: 4px;
}
</style>
