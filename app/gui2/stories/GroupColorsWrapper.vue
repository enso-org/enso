<script setup lang="ts">
import { groupColorVar, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { computed } from 'vue'

const suggestionDb = useSuggestionDbStore()

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  for (let group of suggestionDb.groups) {
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name)
  }
  styles[`--group-color-fallback`] = '#006b8a'
  return styles
})
</script>

<template>
  <div :style="groupColors"><slot></slot></div>
</template>
