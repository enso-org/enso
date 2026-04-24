<script setup lang="ts">
import type { AnyIcon } from '@/util/icons'
import SvgButton from './SvgButton.vue'

const props = defineProps<{
  icon?: AnyIcon | undefined
  label?: string | undefined
  disabled?: boolean
  title?: string | undefined
  variant?: 'submit' | undefined
}>()
const emit = defineEmits<{ activate: [] }>()
</script>

<template>
  <div class="StandaloneButton" @click.stop="emit('activate')">
    <SvgButton v-bind="{ ...$attrs, ...props }" :name="icon" @activate="emit('activate')" />
  </div>
</template>

<style scoped>
.StandaloneButton {
  background-color: var(--color-frame-bg);
  padding: 4px;
  border-radius: var(--radius-full);
}
</style>
