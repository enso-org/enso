<script setup lang="ts">
import { computed, shallowRef } from 'vue'

/* Animate the width of a panel, while holding the interior width constant.
 *
 * Rendering the content at the target width and clipping prevents distracting and expensive reflow.
 */

// Target size is passed via CSS variable --slide-in-panel-size.
const props = defineProps<{
  show: boolean
  // Default: from right.
  from?: 'right' | 'left'
}>()

const contentStyle = computed(() => (props.from === 'left' ? { right: 0 } : { left: 0 }))

const rootElement = shallowRef<HTMLElement>()

defineExpose({ rootElement })
</script>

<template>
  <Transition>
    <div v-if="show" ref="rootElement" class="SlideIn">
      <div class="content" :style="contentStyle">
        <slot />
      </div>
    </div>
  </Transition>
</template>

<style scoped>
.SlideIn {
  box-sizing: content-box;
  width: var(--slide-in-panel-size);
  position: relative;
  overflow-x: clip;
  &.v-enter-active,
  &.v-leave-active {
    transition: width 0.1s ease;
  }
  &.v-enter-from,
  &.v-leave-to {
    width: 0;
  }
}

.content {
  position: absolute;
  width: var(--slide-in-panel-size);
  height: 100%;
}
</style>
