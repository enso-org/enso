<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { type URLString } from '@/util/data/urlString'
import { type Icon } from '@/util/iconMetadata/iconName'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const icon = computed(() => props.input[DisplayIcon].icon)
</script>

<script lang="ts">
export const DisplayIcon: unique symbol = Symbol.for('WidgetInput:DisplayIcon')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [DisplayIcon]?: {
      icon: Icon | URLString | '$evaluating'
      allowChoice?: boolean
      showContents?: boolean
    }
  }
}

export const widgetDefinition = defineWidget(
  DisplayIcon,
  {
    priority: 1,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetIcon">
    <div class="iconContainer">
      <Transition>
        <LoadingSpinner
          v-if="icon === '$evaluating'"
          class="nodeCategoryIcon grab-handle"
          :size="16"
        />
        <SvgIcon v-else class="nodeCategoryIcon grab-handle" :name="icon" />
      </Transition>
    </div>
    <NodeWidget v-if="props.input[DisplayIcon].showContents === true" :input="props.input" />
  </div>
</template>

<style scoped>
.WidgetIcon {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: var(--widget-token-pad-unit);
}
.iconContainer {
  position: relative;
  height: 16px;
  width: 16px;
  margin: 0 calc((var(--node-port-height) - 16px) / 2);
}
.nodeCategoryIcon {
  position: absolute;
}
.LoadingSpinner {
  border: 4px solid;
  border-radius: 100%;
  border-color: rgba(255, 255, 255, 90%) #0000;
  animation: s1 0.8s infinite;
}
@keyframes s1 {
  to {
    transform: rotate(0.5turn);
  }
}
.v-enter-active,
.v-leave-active {
  transition: opacity 0.1s ease;
}
.v-enter-from,
.v-leave-to {
  opacity: 0;
}
</style>
