<script setup lang="ts">
/**
 * A toggleable `SvgButton`.
 *
 * Clicking the icon will switch between `toggledOn` and `toggledOff` css classes on its root `svg`
 * element.
 */

import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconName'

const toggledOn = defineModel<boolean>({ default: false })
const _props = defineProps<{
  icon: Icon | URLString
  title?: string | undefined
  label?: string | undefined
  disabled?: boolean | undefined
}>()
</script>

<template>
  <MenuButton v-model="toggledOn" class="ToggleIcon" :disabled="disabled" :title="title">
    <SvgIcon :name="icon" />
    <div v-if="label" v-text="label" />
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
