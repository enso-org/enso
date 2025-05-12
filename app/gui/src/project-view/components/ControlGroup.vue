<script setup lang="ts">
import { DIALOG_BACKGROUND } from '#/components/AriaComponents'
import VNodes from '@/components/VNodes.vue'

const slots = defineSlots<{ default(): any }>()
const controlStyles = DIALOG_BACKGROUND({
  className: 'control',
})
</script>

<template>
  <div class="ControlGroup">
    <div v-for="(item, i) in slots.default()" :key="i" :class="controlStyles">
      <VNodes :vnodes="item" />
    </div>
  </div>
</template>

<style scoped>
.ControlGroup {
  user-select: none;
  display: flex;
  place-items: center;
  gap: 1px;

  & > .control {
    background: var(--color-frame-bg);
    backdrop-filter: var(--blur-app-bg);
    padding: 4px 4px;
    > * {
      margin: auto;
      line-height: 1;
    }
    &:first-child {
      border-bottom-left-radius: var(--radius-full);
      border-top-left-radius: var(--radius-full);
    }

    &:last-child {
      border-bottom-right-radius: var(--radius-full);
      border-top-right-radius: var(--radius-full);
    }
  }
}
</style>
