<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { ref } from 'vue'

const props = defineProps<{ icon_name: String; modelValue?: Boolean }>()
const emit = defineEmits<{
  (e: 'update:modelValue', toggled_on: Boolean): void
}>()

const toggled_on = ref(props.modelValue ?? false)

function toggle() {
  toggled_on.value = !toggled_on.value
  emit('update:modelValue', toggled_on.value)
}
</script>

<template>
  <SvgIcon :name="props.icon_name" :class="{ toggled_on: toggled_on }" @click="toggle" />
</template>
