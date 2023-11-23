<script setup lang="ts">
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { computed } from 'vue'

const suggestionDb = useSuggestionDbStore()

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  for (let group of suggestionDb.groups) {
    const name = group.name.replace(/\s/g, '-')
    let color = group.color ?? colorFromString(name)
    styles[`--group-color-${name}`] = color
  }
  return styles
})
</script>

<template>
  <div :style="groupColors"><slot></slot></div>
</template>
