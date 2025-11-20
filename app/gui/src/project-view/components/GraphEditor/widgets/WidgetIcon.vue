<script setup lang="ts">
import { Score, defineWidget, widgetProps } from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { type AnyWidgetIcon } from '@/util/icons'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const icon = computed(() => props.input[DisplayIcon].icon)
</script>

<script lang="ts">
export const DisplayIcon: unique symbol = Symbol.for('WidgetInput:DisplayIcon')
declare module '$/providers/openedProjects/widgetRegistry' {
  export interface WidgetInput {
    [DisplayIcon]?: {
      icon: AnyWidgetIcon
      allowChoice?: boolean
      showContents?: boolean
      noGap?: boolean
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
  <div
    :class="{
      WidgetIcon: true,
      widgetParent: true,
      noGap: props.input[DisplayIcon].noGap === true,
    }"
  >
    <div class="iconContainer widgetSingleLine">
      <Transition>
        <GrowingSpinner
          v-if="icon === '$evaluating'"
          class="nodeCategoryIcon grab-handle"
          :size="16"
          phase="loading-medium"
        />
        <SvgIcon v-else class="nodeCategoryIcon grab-handle" :name="icon" />
      </Transition>
    </div>
    <NodeWidget v-if="props.input[DisplayIcon].showContents === true" :input="props.input" />
  </div>
</template>

<style scoped>
.WidgetIcon {
  gap: var(--widget-token-pad-unit);
  &.noGap {
    gap: 0;
  }
}
.iconContainer {
  width: var(--node-port-height);
}
.nodeCategoryIcon {
  margin: auto;
}
.LoadingSpinner {
  border-radius: 100%;
  border-color: rgba(255, 255, 255, 90%) #0000;
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
