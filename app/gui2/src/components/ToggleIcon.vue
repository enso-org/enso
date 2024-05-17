<script setup lang="ts">
/**
 * A toggleable `SvgButton`.
 *
 * Clicking the icon will switch between `toggledOn` and `toggledOff` css classes on its root `svg`
 * element. The component does not have any style by itself, so the `toggledOn` style should be
 * specified in the usage place.
 */

import SvgButton from '@/components/SvgButton.vue'
import type { Icon } from '@/util/iconName'

const props = withDefaults(defineProps<{ icon: Icon; modelValue?: boolean }>(), {
  modelValue: false,
})
const emit = defineEmits<{
  'update:modelValue': [toggledOn: boolean]
}>()
</script>

<template>
  <SvgButton
    :name="icon"
    :class="{ toggledOn: modelValue, toggledOff: !modelValue }"
    @click.stop="emit('update:modelValue', !modelValue)"
  />
</template>
