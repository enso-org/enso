<script lang="ts" setup>
import type { GraphNavigator } from '@/providers/graphNavigator'

const props = defineProps<{
  navigator: GraphNavigator
}>()
</script>

<template>
  <div class="GraphNodeSelections" :style="{ transform: props.navigator.transform }" />
</template>

<style scoped>
.GraphNodeSelections {
  position: absolute;
  top: 0;
  left: 0;
  width: 0;
  height: 0;
  z-index: -1;
}
</style>
