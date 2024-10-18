<script setup lang="ts">
/** @file A dropdown menu supporting the pattern of selecting a single entry from a list. */

import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { SelectionMenuOption } from '@/components/visualizations/toolbar'
import { ref } from 'vue'

type Key = number | string | symbol
const selected = defineModel<Key>({ required: true })
const _props = defineProps<{
  options: Record<Key, SelectionMenuOption>
  title?: string | undefined
  labelButton?: boolean
  alwaysShowArrow?: boolean
}>()

const open = ref(false)
</script>

<template>
  <DropdownMenu v-model:open="open" :title="title" :alwaysShowArrow="alwaysShowArrow">
    <template #button>
      <template v-if="options[selected]">
        <SvgIcon :name="options[selected]!.icon" :style="options[selected]!.iconStyle" />
        <div
          v-if="labelButton && options[selected]!.label"
          class="iconLabel"
          v-text="options[selected]!.label"
        />
      </template>
    </template>
    <template #entries>
      <MenuButton
        v-for="[key, option] in Object.entries(options)"
        :key="key"
        :title="option.title"
        :modelValue="selected === key"
        @update:modelValue="$event && (selected = key)"
        @click="open = false"
      >
        <SvgIcon :name="option.icon" :style="option.iconStyle" :data-testid="option.dataTestid" />
        <div v-if="option.label" class="iconLabel" v-text="option.label" />
      </MenuButton>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.MenuButton {
  margin: -4px;
  justify-content: unset;
}

.iconLabel {
  margin-left: 4px;
  padding-right: 4px;
}
</style>
