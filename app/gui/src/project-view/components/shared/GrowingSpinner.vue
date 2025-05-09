<script setup lang="ts">
/**
 * @file A LoadingSpinner, which starts from 'initial' state and then animates to the passed state.
 */
import LoadingSpinner, { SpinnerProps } from '@/components/shared/LoadingSpinner.vue'
import { computed, onMounted, ref } from 'vue'

const props = defineProps<SpinnerProps>()
const synchronized = ref(false)
const state = computed(() => (synchronized.value ? props.state : 'initial'))
onMounted(() => {
  requestAnimationFrame(() => {
    synchronized.value = true
  })
})
</script>

<template>
  <LoadingSpinner v-bind="props" :state="state" />
</template>
