<script setup lang="ts">
import { WidgetEditHandlerRoot } from '$/providers/openedProjects/widgetRegistry/editHandler'
import ActionMenu from '@/components/ActionMenu.vue'
import { unrefElement, useEvent, useResizeObserver } from '@/composables/events'
import type { DisplayableActionName } from '@/providers/action'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { endOnClickOutside, targetIsOutside } from '@/util/autoBlur'
import { autoUpdate, flip, shift, useFloating } from '@floating-ui/vue'
import { computed, onMounted, ref, watch } from 'vue'

const menu = ref<HTMLElement>()
const { actions, point } = defineProps<{
  actions: DisplayableActionName[]
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
  //
  // TODO[ao]: this should be handled better, but I'm out of ideas for easy fixes.
  // probably the drop-down widget should _not_ be an interaction, actually (this would also allow
  // simplifying WidgetEditHandler, probably)
  if (!(interaction.getCurrent() instanceof WidgetEditHandlerRoot)) {
    interaction.setCurrent(
      endOnClickOutside(menu, {
        cancel: () => emit('close'),
        end: () => emit('close'),
      }),
    )
  } else {
    useEvent(
      window,
      'pointerdown',
      (e) => {
        if (targetIsOutside(e, unrefElement(menu))) {
          emit('close')
        }
      },
      { capture: true },
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
      data-testid="contextMenu"
      @contextmenu.stop.prevent="emit('close')"
      @close="emit('close')"
    >
      <slot />
    </ActionMenu>
  </Teleport>
</template>

<style scoped>
.ActionMenu {
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
