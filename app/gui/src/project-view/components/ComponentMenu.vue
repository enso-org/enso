<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ActionMenu from '@/components/ActionMenu.vue'
import ColorRing from '@/components/ColorRing.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { DisplayableActionName } from '@/providers/action'
import { ref } from 'vue'

const _props = defineProps<{
  colorPickerOpened: boolean
  currentNodeColor: string | undefined
  matchableColors: ReadonlySet<string>
  actions: DisplayableActionName[]
}>()
const emit = defineEmits<{
  closeColorPicker: []
  setNodeColor: [color: string | undefined]
  'update:hovered': [hovered: boolean]
}>()

const isDropdownOpened = ref(false)
</script>

<template>
  <div
    class="ComponentMenu"
    :class="{
      menu: !colorPickerOpened,
      openedDropdown: isDropdownOpened,
    }"
    @pointerenter="emit('update:hovered', true)"
    @pointerleave="emit('update:hovered', false)"
  >
    <template v-if="!colorPickerOpened">
      <DropdownMenu
        v-model:open="isDropdownOpened"
        placement="bottom-start"
        title="More"
        data-testid="more-button"
        class="slotW More"
      >
        <template #button><SvgIcon name="3_dot_menu" class="moreIcon" /></template>
        <template #menu>
          <ActionMenu
            data-testid="component-menu-more-entries"
            :actions="actions"
            @close="isDropdownOpened = false"
          />
        </template>
      </DropdownMenu>
      <ActionButton action="component.toggleDocPanel" class="slotSW" />
      <ActionButton action="component.toggleVisualization" class="slotS" />
    </template>
    <ColorRing
      v-else
      :modelValue="currentNodeColor"
      :matchableColors="matchableColors"
      :initialColorAngle="90"
      @update:modelValue="emit('setNodeColor', $event)"
      @close="emit('closeColorPicker')"
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

.ColorRing {
  /* Cut a hole inside color ring. First we draw a rectangle containing entire ColorRing (with the
   arrow), and then define circle inside. */
  clip-path: path(
    evenodd,
    'M -52,52 L -52,-52 L 154,-52 L 154,154 L -52,154 z M52,32 A20,20 0,1,1 52,72 20,20 0,1,1 52,32'
  );
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
