<script setup lang="ts">
import { useResizeObserver } from '@/composables/events'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { endOnClickOutside } from '@/util/autoBlur'
import { autoUpdate, flip, shift, useFloating } from '@floating-ui/vue'
import { computed, onMounted, ref, watch } from 'vue'
import { Action, Actions } from '../providers/action'
import { injectInteractionHandler, Interaction } from '../providers/interactionHandler'
import ActionMenu from './ActionMenu.vue'

const menu = ref<HTMLElement>()
const { actions, point } = defineProps<{
  actions: (Action | keyof Actions)[]
  /** Location to display the menu near, in client coordinates. */
  point: { x: number; y: number }
}>()
const emit = defineEmits<{ close: [] }>()

const interaction = injectInteractionHandler()

const virtualEl = computed(() => {
  const { x, y } = point
  return {
    getBoundingClientRect() {
      return {
        width: 0,
        height: 0,
        x,
        y,
        top: y,
        left: x,
        right: x,
        bottom: y,
      }
    },
  }
})
const { floatingStyles, update } = useFloating(virtualEl, menu, {
  placement: 'bottom-start',
  middleware: [flip(), shift({ crossAxis: true })],
  whileElementsMounted: autoUpdate,
})

const menuSize = useResizeObserver(menu)
watch(menuSize, update)

onMounted(() => {
  // The widget interactions are a special case: in some widgets (e.g. dropdowns) there are context
  // menus while widget editing is "active" (like in File Browser inside WidgetSelection)
  if (!(interaction.getCurrent() instanceof WidgetEditHandler)) {
    interaction.setCurrent(
      endOnClickOutside(menu, {
        cancel: () => emit('close'),
        end: () => emit('close'),
      }),
    )
  }
})
</script>

<template>
  <Teleport to="#floatingLayer">
    <ActionMenu
      ref="menu"
      :actions="actions"
      :style="floatingStyles"
      @contextmenu.stop.prevent="emit('close')"
      @close="emit('close')"
    >
      <slot />
    </ActionMenu>
  </Teleport>
</template>

<style scoped>
.MenuPanel {
  margin-top: 2px;
  padding: 4px;
  background: var(--dropdown-opened-background, var(--color-app-bg));
  backdrop-filter: var(--dropdown-opened-backdrop-filter, var(--blur-app-bg));
  position: absolute;
  top: 0;
  left: 0;
  height: fit-content;
  width: fit-content;
}
</style>
