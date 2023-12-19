<script setup lang="ts">
import { ref } from 'vue'
import HstWrapper from './HstWrapper.vue'

const props = defineProps<{
  title?: string | undefined
  mode?: FileSystemPermissionMode | undefined
  startIn?: WellKnownDirectory | FileSystemHandle | undefined
}>()
const emit = defineEmits<{ 'update:directory': [directory: FileSystemDirectoryHandle] }>()

const directory = ref<FileSystemDirectoryHandle>()

async function onClick() {
  const handle = await showDirectoryPicker({ mode: props.mode ?? 'read', startIn: props.startIn })
  directory.value = handle
  emit('update:directory', handle)
}
</script>

<template>
  <HstWrapper
    :title="props.title!"
    class="histoire-file htw-cursor-pointer htw-items-center"
    :class="$attrs.class"
    :style="$attrs.style"
    @click="onClick"
  >
    {{ directory?.name ?? 'No folder chosen' }}
  </HstWrapper>
</template>
