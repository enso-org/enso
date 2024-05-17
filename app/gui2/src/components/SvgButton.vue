<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconName'

const _props = defineProps<{
  name: Icon | URLString
}>()
</script>

<template>
  <button class="SvgButton">
    <SvgIcon :name="name" />
  </button>
</template>

<style scoped>
.SvgButton {
  border: none;
  display: flex;
  justify-items: center;
  align-items: center;
  padding: 4px;
  cursor: pointer;
  /* width: calc(var(--scale) * 16px + 8px);
  height: calc(var(--scale) * 16px + 8px); */
  margin: -4px -4px;
  border-radius: var(--radius-full);
  transition: background-color 0.3s;

  &:hover {
    background-color: var(--color-menu-entry-hover-bg);
  }

  & > svg {
    transform: var(--icon-transform);
    transform-origin: var(--icon-transform-origin);
  }
}
</style>
