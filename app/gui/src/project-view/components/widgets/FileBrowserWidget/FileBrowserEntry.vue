<script setup lang="ts">
import { DatalinkAsset, DirectoryAsset, FileAsset } from '#/services/Backend'

export interface RenamedEntry {
  asset: DirectoryAsset | symbol
  name: string
  state: 'editing' | 'pending'
}

const model = defineModel<string>()
const props = defineProps<{
  entry: DirectoryAsset | FileAsset | DatalinkAsset | symbol
  renamedEntry: RenamedEntry | null
}>()

const emit =
  defineEmits <
  {
    click: [],
    newNameAccepted: [],
  }
</script>

<template>
  <SvgButton name="folder" class="FileBrowserEntry" @click="enterDir(entry)">
    <input
      v-if="renamedEntry?.asset === entry && renamedEntry.state === 'editing'"
      v-model="renamedEntry.name"
      @blur="acceptName('update directory')"
      @keydown.enter.stop="($event.currentTarget as HTMLInputElement)?.blur()"
    />
    <div>{{ entry.title }}</div>
  </SvgButton>
</template>

<style scoped>
.FileBrowserEntry {
  width: 100%;
  justify-content: start;
}
</style>
