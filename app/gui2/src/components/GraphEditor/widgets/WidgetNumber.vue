<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { useGraphStore } from '@/stores/graph'
import { type AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps<{
  nodeSpanStart: number
  ast: AstExtended
}>()
const graph = useGraphStore()
const value = computed({
  get() {
    return parseFloat(props.ast.repr())
  },
  set(value) {
    graph.setExpressionContent(props.ast.astId, value.toString())
  },
})
</script>

<template>
  <SliderWidget
    v-model="value"
    class="WidgetNumber"
    :min="-1000"
    :max="1000"
    contenteditable="false"
    @beforeinput.stop
  />
</template>

<style scoped>
.WidgetNumber {
  display: inline-block;
  vertical-align: middle;
}
</style>
