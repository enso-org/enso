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
  variant?: 'submit' | undefined
}>()
const emit = defineEmits<{ activate: [] }>()
</script>

<template>
  <MenuButton
    v-model="toggledOn"
    :disabled="disabled"
    class="SvgButton"
    :class="[variant && `variant-${variant}`, label !== undefined ? 'with-label' : undefined]"
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

.with-label {
  padding: var(--button-padding, 4px) 1em;
}

.variant-submit {
  background-color: var(--color-submit);
  color: var(--color-text-light);
}
</style>
