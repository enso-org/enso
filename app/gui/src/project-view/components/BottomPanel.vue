<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ResizeHandles from '@/components/ResizeHandles.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { useLocalStorage } from '@vueuse/core'
import { computed, ref, watch } from 'vue'

const MIN_DOCK_SIZE_PX = 20

const rootElement = ref<HTMLElement>()

const show = defineModel<boolean>('show', { required: true })

const savedSize = useLocalStorage<{ height: number | null }>('code-editor-size', { height: null })

const computedSize = useResizeObserver(rootElement)
const computedBounds = computed(() => new Rect(Vec2.Zero, computedSize.value))

function clampSize(size: number) {
  return Math.max(size, MIN_DOCK_SIZE_PX)
}

const fullscreen = ref(false)
const fullscreenAnimating = ref(false)

watch(show, (show) => {
  if (!show) fullscreen.value = false
})

const style = computed(() =>
  savedSize.value?.height != null ?
    { '--panel-size': `${clampSize(savedSize.value.height)}px` }
  : undefined,
)
</script>

<template>
  <ActionButton
    action="graph.toggleCodeEditor"
    class="gutterButton bottomOfGutter"
    :class="{ aboveFullscreen: fullscreen || fullscreenAnimating }"
  />
  <Transition>
    <div
      v-if="show"
      ref="rootElement"
      class="BottomPanel dock"
      :style="style"
      data-testid="bottomDock"
      v-bind="$attrs"
    >
      <WithFullscreenMode v-model="fullscreen" @update:animating="fullscreenAnimating = $event">
        <ActionButton
          action="panel.fullscreen"
          class="gutterButton topOfGutter"
          :class="{ aboveFullscreen: fullscreen || fullscreenAnimating }"
        />
        <slot />
      </WithFullscreenMode>
      <ResizeHandles
        top
        :modelValue="computedBounds"
        @update:modelValue="savedSize = { height: $event.height }"
      />
    </div>
  </Transition>
</template>

<style scoped>
.BottomPanel {
  --panel-size: var(--code-editor-default-height);
  position: relative;
  height: var(--panel-size);
  /*
   * Limit the size of the panel. Because size is persisted, allowing the panel to fill the screen
   * may be confusing, especially if the user sets the size on one screen and then reopens the code
   * editor on a screen with a lower resolution.
   */
  max-height: 85svh;
  margin-right: 1px;
  /*
   * Ensure that the content height doesn't exceed the panel's height while animating, which can
   * cause the native scrollbar to appear momentarily, "shaking" the viewport.
   */
  contain: layout;
}
.v-enter-active,
.v-leave-active {
  transition: height 0.1s ease;
}
.v-enter-from,
.v-leave-to {
  height: 0;
}

.gutterButton {
  position: absolute;
  z-index: 1;
  left: 12px;
  &.aboveFullscreen {
    z-index: 2;
  }
}

.bottomOfGutter {
  bottom: 12px;
}

.topOfGutter {
  top: 12px;
}
</style>
