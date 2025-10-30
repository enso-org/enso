<script setup lang="ts">
/** @file A LoadingSpinner, which starts from 'initial' state and then animates to the passed state. */
import LoadingSpinner, { type SpinnerProps } from '@/components/shared/LoadingSpinner.vue'
import { computed, onMounted, onScopeDispose, ref } from 'vue'

const props = defineProps<SpinnerProps>()
const synchronized = ref(false)
const phase = computed(() => (synchronized.value ? props.phase : 'initial'))
onMounted(() => {
  const frame = requestAnimationFrame(() => {
    synchronized.value = true
  })
  onScopeDispose(() => cancelAnimationFrame(frame))
})
</script>

<template>
  <LoadingSpinner v-bind="props" :phase="phase" />
</template>
