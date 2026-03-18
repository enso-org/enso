<script setup lang="ts">
import ActionMenu from '@/components/ActionMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { usePopoverRoot } from '@/providers/popoverRoot'
import type { DisplayableActionName } from '@/providers/action'
import { targetIsOutside } from '@/util/autoBlur'
import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/vue'
import { nextTick, ref, watch } from 'vue'

const { actions } = defineProps<{
  actions: DisplayableActionName[]
}>()

const emit = defineEmits<{
  closeAll: []
}>()

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
      if (parentInteraction && (parentRoot == null || targetIsOutside(event, parentRoot))) {
        interaction.end(parentInteraction)
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
  emit('closeAll')
}
</script>

<template>
  <div ref="rootElement" class="alignmentSubmenuRoot">
    <div ref="triggerElement" class="alignmentSubmenuTrigger">
      <MenuButton v-model="open" class="alignmentSubmenuEntry">
        <SvgIcon name="align_left" class="rowIcon" />
        <span>Align</span>
        <SvgIcon name="arrow_right_head_only" class="submenuArrow" />
      </MenuButton>
    </div>
    <div
      v-if="open"
      ref="panelElement"
      class="alignmentSubmenuPanel"
      :style="floatingStyles"
    >
      <ActionMenu class="alignmentMenu" :actions="actions" @close="closeAll" />
    </div>
  </div>
</template>

<style scoped>
.alignmentSubmenuRoot,
.alignmentSubmenuTrigger {
  width: 100%;
}

.alignmentSubmenuEntry {
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

.alignmentSubmenuPanel {
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
