<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { qnJoin, qnSegments, tryQualifiedName } from '@/util/qualifiedName.ts'
import { computed, nextTick, onMounted, ref, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const tagLabels = computed(() => {
  const tags = props.input.info?.tagValues
  if (tags == null) return []
  return tags.map((tag) => {
    const qualifiedName = tryQualifiedName(tag)
    if (!qualifiedName.ok) return tag
    const segments = qnSegments(qualifiedName.value).slice(-2)
    if (segments[0] == undefined) return tag
    if (segments[1] == undefined) return segments[0]
    return qnJoin(segments[0], segments[1])
  })
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
  return tagLabels.value[selectedIndex.value]
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
    if (tags == null) return Score.Mismatch
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
        :selectedValue="selectedValue"
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
