<script setup lang="ts">
import ActionMenu from '@/components/ActionMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { AnyIcon } from '@/util/icons'
import { injectActionContext } from '@/providers/actionContext'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { usePopoverRoot } from '@/providers/popoverRoot'
import type { DisplayableActionName } from '@/providers/action'
import { targetIsOutside } from '@/util/autoBlur'
import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/vue'
import { nextTick, ref, watch } from 'vue'

const { actions, icon, label } = defineProps<{
  actions: DisplayableActionName[]
  icon: AnyIcon
  label: string
}>()

const actionContext = injectActionContext()
const interaction = injectInteractionHandler()
const popoverRoot = usePopoverRoot(true)
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

      const parentRoot = popoverRoot?.value
      if (parentRoot == null || targetIsOutside(event, parentRoot)) {
        actionContext.endInteraction()
      } else {
        interaction.end(nestedInteraction)
      }
      return false
    },
  }
  return nestedInteraction
})

function closeAll() {
  open.value = false
  actionContext.endInteraction()
}
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
      <ActionMenu class="alignmentMenu" :actions="actions" @close="closeAll" />
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
}

.alignmentMenu {
  padding: 4px;
  backdrop-filter: none;
}

.submenuArrow {
  margin-left: auto;
  opacity: 0.7;
}
</style>
