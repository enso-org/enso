<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { useAsyncResources } from '@/providers/asyncResources'
import { computed, toRef } from 'vue'
import LoadingSpinner from '../shared/LoadingSpinner.vue'

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

const errorMessage = computed(() => {
  const result = data.value
  return result.ok ? result.value.error : result.error.message('')
})
</script>

<template>
  <template v-if="!data.ok || data.value.status === 'error'">
    <div class="error" :title="errorMessage || ''">
      <SvgIcon name="error" /> Failed to load resource.
    </div>
  </template>
  <template v-else-if="data.value.status === 'loading'">
    <LoadingSpinner phase="initial" :size="20" />
  </template>
  <template v-else>
    <img
      :src="data.value.url!"
      :alt="alt"
      :title="title"
      :class="{ uploading: data.value.status === 'uploading' }"
    />
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
