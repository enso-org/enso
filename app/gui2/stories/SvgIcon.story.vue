<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { ref } from 'vue'

import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'

import iconList from '@/util/iconList.json'
import type { Icon } from '@/util/iconName'

const name = ref<Icon>('enso_logo')

const toggledOn = ref(false)
const icon = ref<Icon>('enso_logo')
</script>

<template>
  <Story title="SVG Icon" group="misc" :layout="{ type: 'grid', width: 100 }" autoPropsDisabled>
    <Variant title="icon">
      <SvgIcon :name="name" @click="logEvent('click', [])" />

      <template #controls>
        <HstSelect v-model="name" title="name" :options="iconList" />
      </template>
    </Variant>
    <Variant title="toggle-icon">
      <ToggleIcon
        v-model="toggledOn"
        :icon="icon"
        class="toggle-icon"
        @click="logEvent('click', [])"
      />

      <template #controls>
        <HstCheckbox v-model="toggledOn" title="v-model" :options="iconList" />
        <HstSelect v-model="icon" title="icon" :options="iconList" />
      </template>
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
