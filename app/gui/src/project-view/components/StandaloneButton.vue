<script setup lang="ts">
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconMetadata/iconName'
import SvgButton from './SvgButton.vue'

const props = defineProps<{
  icon?: Icon | URLString | undefined
  label?: string | undefined
  disabled?: boolean
  title?: string | undefined
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
