<script setup lang="ts">
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { AnyIcon } from '@/util/icons'

const toggledOn = defineModel<boolean | undefined>()
defineProps<{
  name?: AnyIcon | undefined
  label?: string | undefined
  disabled?: boolean | undefined
  title?: string | undefined
  extraClickZone?: number | undefined
}>()
const emit = defineEmits<{ activate: [] }>()
</script>

<template>
  <MenuButton
    v-model="toggledOn"
    :disabled="disabled"
    class="SvgButton"
    :title="title"
    :extraClickZone="extraClickZone"
    @activate="emit('activate')"
  >
    <SvgIcon v-if="name" :name="name" />
    <div v-if="label">{{ label }}</div>
  </MenuButton>
</template>

<style scoped>
.SvgButton {
  margin: -4px;
  gap: 4px;

  &.disabled {
    opacity: 0.2;
  }
}
</style>
