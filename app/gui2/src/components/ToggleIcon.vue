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
const _props = defineProps<{ icon: Icon; label?: string }>()
</script>

<template>
  <MenuButton v-model="toggledOn" class="ToggleIcon">
    <SvgIcon :name="icon" />
    <div v-if="label" v-text="label" />
  </MenuButton>
</template>

<style scoped>
.ToggleIcon {
  margin: -4px;
  gap: 4px;
}

.toggledOff {
  opacity: 0.4;
}
</style>
