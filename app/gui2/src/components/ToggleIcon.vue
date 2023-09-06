<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { ref } from 'vue'

const props = defineProps<{ icon: String; modelValue?: Boolean }>()
const emit = defineEmits<{
  (e: 'update:modelValue', toggledOn: Boolean): void
}>()

const toggledOn = ref(props.modelValue ?? false)

function toggle() {
  toggledOn.value = !toggledOn.value
  emit('update:modelValue', toggledOn.value)
}
</script>

<template>
  <SvgIcon :variant="props.icon" :class="{ toggled_on: toggledOn }" @click="toggle" />
</template>
