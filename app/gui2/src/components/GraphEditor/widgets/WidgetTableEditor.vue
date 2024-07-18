<script setup lang="ts">
import ResizeHandles from '@/components/ResizeHandles.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref } from 'vue'
import { WidgetInputIsSpecificMethodCall } from './WidgetFunction.vue'

const size = ref(new Vec2(200, 50))
const graphNav = injectGraphNavigator()

const clientBounds = computed({
  get() {
    return new Rect(Vec2.Zero, size.value.scale(graphNav.scale))
  },
  set(value) {
    size.value = new Vec2(value.width / graphNav.scale, value.height / graphNav.scale)
  },
})

const widgetStyle = computed(() => {
  return {
    width: `${size.value.x}px`,
    height: `${size.value.y}px`,
  }
})

const _props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInputIsSpecificMethodCall({
    module: 'Standard.Table.Table',
    definedOnType: 'Standard.Table.Table.Table',
    name: 'new',
  }),
  {
    priority: 999,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTableEditor" :style="widgetStyle">
    <div>WidgetTableEditor</div>
    <ResizeHandles v-model="clientBounds" bottom right />
  </div>
</template>

<style scoped>
.WidgetTableEditor {
  color: yellow;
  display: flex;
  align-items: center;
  justify-content: center;
  background: #00ff0055;
  border-radius: var(--node-port-border-radius);
}
</style>
