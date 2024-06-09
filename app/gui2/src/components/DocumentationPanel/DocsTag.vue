<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import type { View } from './DocsTags.vue'

const props = defineProps<{ view: View; parentOffset: number; forceShow: boolean }>()
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
    :style="props.view.style || {}"
  >
    {{ props.view.label }}
  </div>
</template>

<style scoped>
.hide {
  display: none;
}
</style>
