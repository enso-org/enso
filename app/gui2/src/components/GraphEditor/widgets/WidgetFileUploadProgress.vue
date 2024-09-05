<script setup lang="ts">
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { UploadingFile } from '@/stores/awareness'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const upload = computed(() => props.input[FileUploadKey])
const style = computed(() => {
  return {
    'background-position': `${200 - upload.value.file.sizePercentage}% 0`,
  }
})
</script>

<script lang="ts">
export const FileUploadKey: unique symbol = Symbol.for('WidgetInput:FileUploadProgress')

export interface FileUploadInfo {
  name: string
  file: UploadingFile
}

function hasFileUpload(input: WidgetInput): input is WidgetInput & {
  [FileUploadKey]: FileUploadInfo
} {
  return input[FileUploadKey] != null
}

export const widgetDefinition = defineWidget(
  hasFileUpload,
  {
    priority: 101,
    score: Score.Perfect,
  },
  import.meta.hot,
)

declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [FileUploadKey]?: FileUploadInfo
  }
}
</script>

<template>
  <div class="WidgetFileUploadProgress" :style="style">
    <span>{{ `Uploading ${upload.name} (${upload.file.sizePercentage}%)` }}</span>
  </div>
</template>

<style scoped>
.WidgetFileUploadProgress {
  border-radius: 16px;
  display: flex;
  flex-direction: row;
  align-items: center;
  white-space: nowrap;
  padding: 2px 8px;
  outline: 0px solid transparent;

  --progress-color: color-mix(in oklab, var(--node-color-port) 85%, white 15%);
  background: linear-gradient(
    to right,
    var(--node-color-port) 0%,
    var(--node-color-port) 50%,
    var(--progress-color) 50%,
    var(--progress-color) 100%
  );
  background-size: 200% 100%;
}
</style>
