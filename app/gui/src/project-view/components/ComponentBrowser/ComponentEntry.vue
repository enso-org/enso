<script setup lang="ts">
import type { Component } from '@/components/ComponentBrowser/component'
import SvgIcon from '@/components/SvgIcon.vue'
import { allRanges } from '@/util/data/range'
import { sourceRangeKey } from 'ydoc-shared/util/data/text'

const { component, color } = defineProps<{ component: Component; color: string }>()
</script>

<template>
  <div class="ComponentEntry" :style="{ '--component-color': color }">
    <SvgIcon class="icon" :name="component.icon" />
    <span>
      <span v-if="!component.matchedRanges" v-text="component.label"></span>
      <span
        v-for="range in allRanges(component.matchedRanges, component.label.length)"
        v-else
        :key="sourceRangeKey(range)"
        class="component-label-segment"
        :class="{ match: range.isMatch }"
        v-text="range.slice(component.label)"
      ></span>
    </span>
  </div>
</template>

<style scoped>
.ComponentEntry {
  width: 100%;
  height: 24px;
  border-radius: 12px;
  flex-direction: row;
  align-items: center;
  gap: 8px;
  padding: 5px;
  display: flex;
  line-height: 1;
  font-family: var(--font-code);

  &.selected {
    color: white;
    background-color: var(--component-color);
    & svg {
      color: white;
    }
  }
  &:not(.selected) .icon {
    color: var(--component-color);
  }
}

.component-label-segment.match {
  font-weight: bold;
}
</style>
