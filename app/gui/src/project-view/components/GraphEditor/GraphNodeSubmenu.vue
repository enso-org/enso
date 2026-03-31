<script setup lang="ts">
import ActionMenu from '@/components/ActionMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { DisplayableActionName } from '@/providers/action'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { targetIsOutside } from '@/util/autoBlur'
import type { AnyIcon } from '@/util/icons'
import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/vue'
import { nextTick, ref, watch } from 'vue'

const { actions, icon, label } = defineProps<{
  actions: DisplayableActionName[]
  icon: AnyIcon
  label: string
}>()

const interaction = injectInteractionHandler()
const open = ref(false)
const rootElement = ref<HTMLElement>()
const triggerElement = ref<HTMLElement>()
const panelElement = ref<HTMLElement>()

const { floatingStyles, update } = useFloating(triggerElement, panelElement, {
  placement: 'right-start',
  strategy: 'fixed',
  middleware: [offset(4), flip(), shift({ padding: 8 })],
  whileElementsMounted: autoUpdate,
})

watch(open, (isOpen) => {
  if (isOpen) nextTick(update)
})

interaction.setWhenWithParent(open, (parentInteraction) => {
  const nestedInteraction: Interaction = {
    parentInteraction,
    cancel: () => (open.value = false),
    end: () => (open.value = false),
    pointerdown: (event) => {
      if (!targetIsOutside(event, rootElement.value)) return false
      interaction.end(nestedInteraction)
      return parentInteraction?.pointerdown?.(event)
    },
  }
  return nestedInteraction
})
</script>

<template>
  <div ref="rootElement" class="submenuRoot">
    <div ref="triggerElement" class="submenuTrigger">
      <MenuButton v-model="open" class="submenuEntry">
        <SvgIcon :name="icon" class="rowIcon" />
        <span>{{ label }}</span>
        <SvgIcon name="arrow_right_head_only" class="submenuArrow" />
      </MenuButton>
    </div>
    <div v-if="open" ref="panelElement" class="submenuPanel" :style="floatingStyles">
      <ActionMenu :actions="actions" @close="interaction.cancelAll()" />
    </div>
  </div>
</template>

<style scoped>
.submenuRoot,
.submenuTrigger {
  width: 100%;
}

.submenuEntry {
  display: flex;
  align-items: center;
  width: 100%;
  gap: 8px;
  justify-content: left;
  padding-left: 8px;
  padding-right: 8px;
  background: transparent;
  backdrop-filter: none;
  color: inherit;
}

.submenuPanel {
  z-index: var(--z-index-selection-submenu);
  margin-top: 2px;
  padding: 4px;
  background: var(--dropdown-opened-background, var(--color-app-bg));
  backdrop-filter: var(--dropdown-opened-backdrop-filter, var(--blur-app-bg));
  border-radius: 13px;
  overflow: hidden;
  position: absolute;
  top: 0;
  left: 0;
  height: fit-content;
  width: fit-content;
}

.submenuArrow {
  margin-left: auto;
  opacity: 0.7;
}
</style>
