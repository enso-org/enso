<script setup lang="ts">
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useAsyncResources } from '@/providers/asyncResources'
import { computed, toRef } from 'vue'

const DEFAULT_ALT_TEXT = 'Image'

const res = useAsyncResources()

const props = defineProps<{
  src: string
  alt: string
}>()

const data = res.useResourceFromUrl(toRef(props, 'src'))

const errorMessage = computed(() => {
  const result = data.value
  return result.ok ? (result.value.error ?? '') : result.error.message('')
})
</script>

<template>
  <div v-if="!data.ok || data.value.status === 'error'" class="error" :title="errorMessage">
    <SvgIcon name="error" /> {{ alt }}
  </div>
  <template v-else>
    <img
      v-if="data.value.url"
      :src="data.value.url"
      :alt="alt"
      :title="alt !== DEFAULT_ALT_TEXT ? alt : ''"
      :class="{ uploading: data.value.status === 'uploading' }"
      data-testid="doc-img"
    />
    <LoadingSpinner v-if="data.value.status == 'loading'" phase="initial" :size="20" />
  </template>
</template>

<style scoped>
.uploading {
  opacity: 0.5;
}

.error {
  padding: 3px;
  border: 1px solid #ffaaaa;
  display: flex;
  flex-direction: row;
}
</style>
