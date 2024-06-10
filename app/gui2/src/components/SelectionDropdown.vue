<script setup lang="ts" generic="T extends string | number | symbol">
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import { ref } from 'vue'

const selected = defineModel<T>({ required: true })
const _props = defineProps<{ values: T[] }>()

const open = ref(false)
</script>

<template>
  <DropdownMenu v-model:open="open">
    <template #button>
      <slot :value="selected" />
    </template>
    <template #entries>
      <MenuButton
        v-for="value in values"
        :key="value"
        :modelValue="selected === value"
        @update:modelValue="$event && (selected = value)"
        @click="open = false"
      >
        <slot :value="value" />
      </MenuButton>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.MenuButton {
  margin: -4px;
}
</style>
