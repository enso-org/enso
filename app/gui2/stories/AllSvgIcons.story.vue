<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'

import iconList from '@/util/iconList.json'
import type { Icon } from '@/util/iconName'

const iconListAsIcons = iconList as Icon[]
</script>

<template>
  <Story
    title="All SVG Icons"
    group="misc"
    :layout="{ type: 'grid', width: 100 }"
    autoPropsDisabled
  >
    <Variant v-for="icon in iconListAsIcons" :key="icon" :title="icon">
      <SvgIcon :name="icon" />
    </Variant>
  </Story>
</template>

<style scoped>
.toggle-icon {
  opacity: 30%;
}

.toggle-icon.toggledOn {
  opacity: unset;
}
</style>
