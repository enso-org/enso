<script setup lang="ts">
import type { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import { computed } from 'vue'

const props = defineProps<{
  edge: { source: Vec2; target: Vec2 }
  editing: boolean
  navigator: ReturnType<typeof useNavigator>
}>()

const emit = defineEmits<{
  disconnectSource: []
  disconnectTarget: []
}>()

const edgePath = computed(() => {
  const source = props.edge.source
  const target = props.edge.target

  const LINE_OUT = 20
  const QUAD_OUT = 50

  const midpointX = (source.x + target.x) / 2
  const midpointY = (source.y + target.y) / 2

  return `
    M ${source.x} ${source.y}
    L ${source.x} ${source.y + LINE_OUT}
    Q ${source.x} ${source.y + QUAD_OUT} ${midpointX} ${midpointY}
    Q ${target.x} ${target.y - QUAD_OUT} ${target.x} ${target.y - LINE_OUT}
    L ${target.x} ${target.y}
  `
})

function click(e: PointerEvent) {
  if (props.editing) return
  const pos = props.navigator.eventToScenePos(e)
  const sqDistanceFromSource = Vec2.DistanceSquared(pos, props.edge.source)
  const sqDistanceFromTarget = Vec2.DistanceSquared(pos, props.edge.target)
  if (sqDistanceFromSource > sqDistanceFromTarget) {
    emit('disconnectSource')
  } else {
    emit('disconnectTarget')
  }
}
</script>

<template>
  <path
    :d="edgePath"
    stroke="black"
    stroke-width="4"
    fill="none"
    class="edge"
    :class="{ editing }"
    @pointerdown="click"
  />
</template>

<style scoped>
.edge {
  stroke: tan;
}
.edge:hover {
  stroke: red;
}
.edge.editing {
  stroke: red;
}
</style>
