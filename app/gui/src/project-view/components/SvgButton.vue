<script setup lang="ts">
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconMetadata/iconName'

const toggledOn = defineModel<boolean | undefined>()
defineProps<{
  name?: Icon | URLString | undefined
  label?: string | undefined
  disabled?: boolean | undefined
  title?: string | undefined
  extraClickZone?: number | undefined
  variant?: 'accent' | 'submit' | undefined
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

.variant-accent {
  background-color: var(--color-accent);
  color: var(--color-text-light);
}
</style>
