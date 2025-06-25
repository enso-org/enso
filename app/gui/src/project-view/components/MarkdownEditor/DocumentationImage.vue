<script setup lang="ts">
import { useAsyncResources } from '@/providers/asyncResources'
import { computed, toRef } from 'vue'

const DEFAULT_ALT_TEXT = 'Image'

const res = useAsyncResources()

const props = defineProps<{
  src: string
  alt: string
}>()

const data = res.useResourceFromUrl(toRef(props, 'src'))

const title = computed(() =>
  data.value == null ? 'Loading'
  : !data.value.ok ? data.value.error.message()
  : props.alt !== DEFAULT_ALT_TEXT ? props.alt
  : '',
)
</script>

<template>
  <img
    :src="data?.ok ? (data.value.url ?? '') : ''"
    :alt="alt"
    :title="title"
    :class="{ uploading: data?.ok && data.value.status === 'loading' }"
  />
</template>

<style scoped>
.uploading {
  opacity: 0.5;
}
</style>
