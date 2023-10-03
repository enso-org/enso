<script setup lang="ts">
import { ref } from 'vue'

const props = defineProps<{ title?: string; accept?: string }>()
const emit = defineEmits<{ 'update:file': [file: File] }>()

const fileNode = ref<HTMLInputElement>()
const file = ref<File>()

function onFileChange(newFile: File | undefined) {
  if (!newFile) return
  emit('update:file', newFile)
  file.value = newFile
}
</script>

<template>
  <label
    class="histoire-wrapper htw-p-2 hover:htw-bg-primary-100 dark:hover:htw-bg-primary-800 htw-flex htw-gap-2 htw-flex-wrap histoire-json htw-cursor-pointer"
  >
    <span
      class="htw-w-28 htw-whitespace-nowrap htw-text-ellipsis htw-overflow-hidden htw-shrink-0"
      :class="{ 'v-popper--has-tooltip': props.title }"
      v-text="props.title"
    ></span>
    <span class="htw-grow htw-max-w-full htw-flex htw-items-center htw-gap-1">
      <span class="htw-block htw-grow htw-max-w-full">
        {{ file?.name ?? 'No file chosen' }}
        <input
          ref="fileNode"
          type="file"
          class="htw-hidden"
          v-bind="$props as never"
          @input="onFileChange(fileNode?.files?.[0])"
        />
      </span>
    </span>
  </label>
</template>
