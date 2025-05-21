<script setup lang="ts">
import { computed, ref, watch } from 'vue'

const props = defineProps<{
  label: string
  value: string
  style?: Record<string, string | number | undefined>
  parentOffset: number
  forceShow: boolean
}>()
const emit = defineEmits<{ hidden: [] }>()

const tagRef = ref<HTMLDivElement>()
const shouldBeHidden = computed(() => {
  if (tagRef.value == null) return false
  return tagRef.value.offsetTop > props.parentOffset
})

watch(shouldBeHidden, (m) => m && emit('hidden'))
</script>

<template>
  <div
    ref="tagRef"
    :class="{ hide: shouldBeHidden && !props.forceShow }"
    :style="props.style || {}"
  >
    {{ props.label + (props.value.length > 0 ? `: ${props.value}` : '') }}
  </div>
</template>

<style scoped>
.hide {
  display: none;
}
</style>
