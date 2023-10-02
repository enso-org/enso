<script setup lang="ts">
/**
 * A toggleable `SvgIcon`.
 *
 * Clicking the icon will add `toggledOn` css class on its root `svg` element. The component
 * does not have any style by itself, so the `toggledOn` style should be specified in the usage
 * place.
 */

import SvgIcon from '@/components/SvgIcon.vue'

const props = withDefaults(defineProps<{ icon: string; modelValue?: boolean }>(), {
  modelValue: false,
})
const emit = defineEmits<{
  (e: 'update:modelValue', toggledOn: boolean): void
}>()
</script>

<template>
  <SvgIcon
    :name="props.icon"
    :class="{ toggledOn: modelValue }"
    @pointerdown.stop="emit('update:modelValue', !modelValue)"
  />
</template>
