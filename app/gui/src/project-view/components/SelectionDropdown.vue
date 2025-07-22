<script setup lang="ts">
/** @file A dropdown menu supporting the pattern of selecting a single entry from a list. */

import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { SelectionMenuOption } from '@/components/visualizations/toolbar'
import { ref, toValue } from 'vue'

type Key = string

const selected = defineModel<Key>({ required: true })
defineProps<{
  options: Record<Key, SelectionMenuOption>
  title?: string | undefined
  labelButton?: boolean | undefined
  alwaysShowArrow?: boolean | undefined
  entriesTestId?: string | undefined
}>()

const open = ref(false)

function onClick(option: SelectionMenuOption) {
  if (!(option.disabled && toValue(option.disabled))) open.value = false
}
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
    <template #menu>
      <MenuPanel :data-testid="entriesTestId">
        <MenuButton
          v-for="[key, option] in Object.entries(options).filter(
            ([_key, { hidden }]) => !hidden || !toValue(hidden),
          )"
          :key="key"
          :title="option.title"
          :modelValue="selected === key"
          :disabled="option.disabled && toValue(option.disabled)"
          @update:modelValue="$event && (selected = key)"
          @click="() => onClick(option)"
        >
          <SvgIcon :name="option.icon" :style="option.iconStyle" :data-testid="option.dataTestid" />
          <div v-if="option.label" class="iconLabel" v-text="option.label" />
          <div
            v-if="option.labelExtension"
            class="iconLabel labelExtension"
            v-text="option.labelExtension"
          />
        </MenuButton>
      </MenuPanel>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.MenuButton {
  margin: -4px;
  justify-content: unset;
  &:has(.iconLabel) {
    padding-right: 4px;
  }
}

.iconLabel {
  padding-left: 4px;
}

.labelExtension {
  font-size: smaller;
}
</style>
