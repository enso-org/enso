<script setup lang="ts">
/**
 * A toggleable `SvgButton`.
 *
 * Clicking the icon will switch between `toggledOn` and `toggledOff` css classes on its root `svg`
 * element.
 */

import SvgButton from '@/components/SvgButton.vue'
import type { Icon } from '@/util/iconName'

const _props = withDefaults(defineProps<{ icon: Icon; modelValue?: boolean }>(), {
  modelValue: false,
})
const emit = defineEmits<{
  'update:modelValue': [toggledOn: boolean]
}>()
</script>

<template>
  <SvgButton
    :name="icon"
    class="ToggleIcon"
    :class="{ toggledOn: modelValue, toggledOff: !modelValue }"
    @click.stop="emit('update:modelValue', !modelValue)"
  />
</template>

<style scoped>
.toggledOff {
  opacity: 0.4;
}
</style>
