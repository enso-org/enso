<script setup lang="ts">
import ConditionalTeleport from '@/components/ConditionalTeleport.vue'
import MenuButton from '@/components/MenuButton.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { usePopoverRoot } from '@/providers/popoverRoot'
import { endOnClickOutside } from '@/util/autoBlur'
import { shift, useFloating, type Placement } from '@floating-ui/vue'
import { shallowRef } from 'vue'

const open = defineModel<boolean>('open', { default: false })
const {
  title,
  placement = 'bottom-start',
  alwaysShowArrow = false,
} = defineProps<{
  title?: string | undefined
  placement?: Placement
  alwaysShowArrow?: boolean | undefined
}>()

const rootElement = shallowRef<HTMLElement>()
const floatElement = shallowRef<HTMLElement>()
const popoverRoot = usePopoverRoot(true)

const end = () => (open.value = false)
const interaction = endOnClickOutside(floatElement, {
  cancel: end,
  end,
  parentInteraction: undefined,
})
injectInteractionHandler().setWhenWithParent(open, (parentInteraction) => {
  interaction.parentInteraction = parentInteraction
  return interaction
})

const { floatingStyles } = useFloating(rootElement, floatElement, {
  placement: () => placement,
  middleware: [shift()],
})
</script>

<template>
  <div ref="rootElement" class="DropdownMenu" @pointerdown.prevent>
    <MenuButton v-model="open" :title="title">
      <slot name="button" />
    </MenuButton>
    <SvgIcon
      v-show="!open"
      name="arrow_right_head_only"
      class="arrow"
      :class="{ visible: alwaysShowArrow }"
    />
    <ConditionalTeleport :target="popoverRoot">
      <SizeTransition height :duration="100">
        <div
          v-if="open"
          ref="floatElement"
          class="DropDownPanel"
          :style="floatingStyles"
          @pointerdown.prevent
        >
          <slot name="menu" />
        </div>
      </SizeTransition>
    </ConditionalTeleport>
  </div>
</template>

<style scoped>
.DropdownMenu {
  position: relative;
  outline: 0;
  margin: -4px;
}

.MenuButton {
  backdrop-filter: var(--blur-app-bg);
}

.arrow {
  position: absolute;
  bottom: calc(-8px - var(--arrow-offset, 0px));
  left: 50%;
  opacity: 0;
  /* Prevent the button from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
  visibility: hidden;
  --icon-transform: translateX(-50%) rotate(90deg) scale(0.7);
  --icon-transform-origin: center;
  transition: opacity 100ms ease-in-out;
  .DropdownMenu:has(.MenuButton:hover) &,
  &.visible {
    opacity: 0.8;
    visibility: inherit;
  }
}

.DropDownPanel {
  /*noinspection CssUnresolvedCustomProperty*/
  z-index: var(--drop-down-panel-z-index, 20);
}
</style>
