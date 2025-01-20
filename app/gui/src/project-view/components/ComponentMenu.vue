<script setup lang="ts">
import ColorRing from '@/components/ColorRing.vue'
import ComponentContextMenu from '@/components/ComponentContextMenu.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectComponentButtons } from '@/providers/componentButtons'
import { ref } from 'vue'

const componentButtons = injectComponentButtons()
const isDropdownOpened = ref(false)
</script>

<template>
  <div
    class="ComponentMenu"
    :class="{
      menu: !componentButtons.pickColor.state,
      openedDropdown: isDropdownOpened,
    }"
  >
    <template v-if="!componentButtons.pickColor.state">
      <SvgButton
        name="eye"
        class="slotS"
        title="Visualization"
        @click.stop="
          componentButtons.toggleVisualization.state = !componentButtons.toggleVisualization.state
        "
      />
      <SvgButton
        name="help"
        class="slotSW"
        title="Help"
        @click.stop="componentButtons.toggleDocPanel.action"
      />
      <DropdownMenu
        v-model:open="isDropdownOpened"
        placement="bottom-start"
        title="More"
        data-testid="more-button"
        class="slotW More"
      >
        <template #button><SvgIcon name="3_dot_menu" class="moreIcon" /></template>
        <template #menu>
          <ComponentContextMenu @close="isDropdownOpened = false" />
        </template>
      </DropdownMenu>
    </template>
    <ColorRing
      v-else
      v-model="componentButtons.pickColor.actionData.currentColor"
      :matchableColors="componentButtons.pickColor.actionData.matchableColors"
      :initialColorAngle="90"
      @close="componentButtons.pickColor.state = false"
    />
  </div>
</template>

<style scoped>
.ComponentMenu {
  position: absolute;
  left: -36px;
  bottom: -36px;
  width: var(--outer-diameter);
  height: var(--outer-diameter);
  user-select: none;
  pointer-events: none;
  /* This is a variable so that it can be referenced in computations,
     but currently it can't be changed due to many hard-coded values below. */
  --outer-diameter: 104px;
  /* It would be preferred to use var(--color-app-bg) and var(--blur-app-bg) here, 
     but for some reason the dropdown is ignoring backdrop-filter, 
     and does not match circular menu in color.*/
  --dropdown-opened-background: white;
  --dropdown-opened-backdrop-filter: none;
}

.menu {
  > * {
    pointer-events: all;
  }

  &:before {
    content: '';
    position: absolute;
    backdrop-filter: var(--blur-app-bg);
    background: var(--color-app-bg);
    width: 100%;
    height: 100%;
    pointer-events: all;
    top: 36px;
    transition: all ease 0.1s;
    clip-path: path(
      'M0,16 V16 A52,52,0,0,0,52,68 A16,16,0,0,0,52,36 A20,20,0,0,1,32,16 A16,16,0,0,0,0,16'
    );
  }
  &.openedDropdown:before {
    background: var(--dropdown-opened-background);
    backdrop-filter: var(--dropdown-opened-backdrop-filter);
    clip-path: path(
      'M0,16 V68 A52,52,0,0,0,52,68 A16,16,0,0,0,52,36 A20,20,0,0,1,32,16 A16,16,0,0,0,0,16'
    );
  }
}

/**
  * Styles to position icons in a circular pattern. Slots are named `slot<SIDE>` and positioned using absolute positioning.
  * The slots form a quarter circle with `slotS` at the bottom, `slotSW` to the left of `slotS`, and `slotW` above `slotSW`.
  * ```
  * slotW
  *      slotSW
  *           slotS
  * ```
 */
.slotS {
  position: absolute;
  left: 44px;
  top: 80px;
}

.slotSW {
  position: absolute;
  top: 69.46px;
  left: 18.54px;
}

.slotW {
  position: absolute;
  top: 44px;
  left: 8px;
}
</style>
