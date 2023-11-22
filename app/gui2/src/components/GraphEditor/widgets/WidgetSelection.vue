<script setup lang="ts">
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import { widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { qnJoin, qnSegments, tryQualifiedName } from '@/util/qualifiedName.ts'
import { computed, ref, watch } from 'vue'

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

const parentColor = computed(() => {
  // TODO: Get the correct ID of the expression once the AST is updated.
  // Then use `graph.db.getNodeColorStyle` to get the color.
  console.warn('TODO: Get the correct ID of the expression once the AST is updated.')
  return 'oklch(0.525859375 0.11519042968750001 181.7578125)'
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
  console.warn('TODO: Update the expression content once the AST is updated.')
  const id = '' // TODO: Get the id of the expression once the AST is updated.
  const expression = selectedValue.value ?? ''
  if (id) graph.setExpressionContent(id, expression)
  showDropdownWidget.value = false
})

const argName = computed(() => {
  const name = props.input.info?.name
  if (name == null) return ''
  return name
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
  <div>
    <span class="WidgetArgumentName" @pointerdown="showDropdownWidget = !showDropdownWidget">
      <template v-if="showArgumentValue">
        <span class="name">{{ argName }}</span
        ><span> {{ selectedValue }} </span>
      </template>
      <template v-else>{{ argName }}</template>
    </span>
    <div class="WidgetDrodown">
      <DropdownWidget
        v-if="showDropdownWidget"
        :color="parentColor"
        :values="tagLabels ?? []"
        :selectedValue="selectedValue"
        @click="selectedIndex = $event"
      />
    </div>
  </div>
</template>
<style scoped>
.name {
  color: rgb(255 255 255 / 0.5);
}
.name {
  margin-right: 8px;
}
.WidgetDrodown {
  position: absolute;
  top: 100%;
  margin-top: 4px;
}
</style>
