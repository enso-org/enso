<script setup lang="ts">
/**
 *
 */

import SvgIcon from '@/components/SvgIcon.vue'
import { ref } from 'vue'

const props = withDefaults(defineProps<{ icon: string; modelValue?: boolean }>(), {
  modelValue: false,
})
const emit = defineEmits<{
  (e: 'update:modelValue', toggledOn: boolean): void
}>()
</script>

<template>
  <SvgIcon
    :variant="props.icon"
    :class="{ toggledOn: modelValue }"
    @click="emit('update:modelValue', !modelValue)"
  />
</template>
