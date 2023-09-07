<script setup lang="ts">
import { useWindowEvent } from '@/util/events'
import type { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import { computed, ref } from 'vue'

const props = defineProps<{
  navigator: ReturnType<typeof useNavigator>
}>()

const shown = ref(false)
const scenePosition = ref(Vec2.Zero())

function positionAtMouse(): boolean {
  const nav = props.navigator
  const mousePos = nav.sceneMousePos
  if (mousePos == null) return false
  scenePosition.value = mousePos
  return true
}

const transform = computed(() => {
  const nav = props.navigator
  const pos = scenePosition.value
  return `${nav.transform} translate(${pos.x}px, ${pos.y}px) scale(${
    1 / nav.scale
  }) translateY(-100%)`
})

useWindowEvent('keypress', (e) => {
  switch (e.key) {
    case 'Enter':
      if (!shown.value && positionAtMouse()) {
        shown.value = true
      } else {
        shown.value = false
      }
      break
  }
})
</script>

<template>
  <div v-if="shown" class="ComponentBrowser" :style="{ transform }">
    <div class="panel components">COMPONENTS</div>
    <div class="panel docs">DOCS</div>
  </div>
</template>

<style scoped>
.ComponentBrowser {
  color: red;
  display: flex;
  flex-direction: row;
}

.panel {
  border: 1px solid black;
  padding: 10px;
  background: #222;
}

.components {
  width: 200px;
}

.docs {
  width: 400px;
}
</style>
