<script setup lang="ts">
/**
 * A toggleable `SvgButton`.
 *
 * Clicking the icon will switch between `toggledOn` and `toggledOff` css classes on its root `svg`
 * element.
 */

import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconName'

const toggledOn = defineModel<boolean>({ default: false })
const props = defineProps<{ icon: Icon; label?: string; disabled?: boolean }>()
</script>

<template>
  <MenuButton v-model="toggledOn" class="ToggleIcon" :disabled="props.disabled">
    <SvgIcon :name="icon" />
    <div v-if="props.label" v-text="props.label" />
  </MenuButton>
</template>

<style scoped>
.ToggleIcon {
  margin: -4px;
  gap: 4px;
}

.toggledOff svg {
  opacity: 0.4;
}

:is(.toggledOff, .toggledOn):active svg {
  opacity: 0.7;
}

.record {
  &.toggledOn {
    color: red;
  }
  &.toggledOff svg {
    opacity: unset;
  }
}
</style>
