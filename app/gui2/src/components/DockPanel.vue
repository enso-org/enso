<script setup lang="ts">
import ResizeHandles from '@/components/ResizeHandles.vue'
import SvgButton from '@/components/SvgButton.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref } from 'vue'

const rootElement = ref<HTMLElement>()

const show = defineModel<boolean>('show', { required: true })
const size = defineModel<number | undefined>('size')

const computedSize = useResizeObserver(rootElement)
const computedBounds = computed(() => new Rect(Vec2.Zero, computedSize.value))

const style = computed(() => {
  return {
    width: size.value != null ? `${size.value}px` : undefined,
  }
})
</script>

<template>
  <Transition name="rightDock">
    <div v-if="show" ref="rootElement" class="DockPanel rightDock" :style="style">
      <div class="scrollArea">
        <slot />
      </div>
      <SvgButton name="close" class="closeButton button" @click.stop="show = false" />
      <ResizeHandles left :modelValue="computedBounds" @update:modelValue="size = $event.width" />
    </div>
  </Transition>
</template>

<style scoped>
.DockPanel {
  position: absolute;
  top: 46px;
  bottom: 0;
  width: var(--right-dock-default-width);
  right: 0;
  border-radius: 7px 0 0;
  background-color: rgba(255, 255, 255, 0.35);
  backdrop-filter: var(--blur-app-bg);
  padding: 4px 12px 0 0;
}
.rightDock-enter-active,
.rightDock-leave-active {
  transition: left 0.25s ease;
  /* Prevent absolutely-positioned children (such as the close button) from bypassing the show/hide animation. */
  overflow-x: clip;
}
.rightDock-enter-from,
.rightDock-leave-to {
  width: 0;
}
.rightDock .scrollArea {
  width: 100%;
  height: 100%;
  overflow-y: auto;
  padding-left: 6px;
}

.rightDock .closeButton {
  position: absolute;
  top: 4px;
  right: 28px;
  color: red;
  opacity: 0.3;

  &:hover {
    opacity: 0.6;
  }
}
</style>
