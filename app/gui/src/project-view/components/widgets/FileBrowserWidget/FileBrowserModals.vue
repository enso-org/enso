<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'

const overwriteFilename = defineModel<string | null>('overwriteFilename', { required: true })
const warningText = defineModel<string | null>('warningText', { required: true })

const emit = defineEmits<{
  overwriteConfirmed: []
}>()

function overwriteConfirmed() {
  overwriteFilename.value = null
  emit('overwriteConfirmed')
}

function overwriteCancelled() {
  overwriteFilename.value = null
}

function warningDismissed() {
  warningText.value = null
}
</script>

<template>
  <div v-if="overwriteFilename" class="FileBrowserModal">
    <div class="confirmationText">
      {{ `File '${overwriteFilename ?? ''}' already exists. Overwrite?` }}
    </div>
    <div class="confirmationButtons">
      <SvgButton class="FileBrowserButton" label="No" @click.stop="overwriteCancelled" />
      <SvgButton class="FileBrowserButton" label="Yes" @click.stop="overwriteConfirmed" />
    </div>
  </div>
  <div v-if="warningText" class="FileBrowserModal">
    <div class="confirmationText">{{ 'Warning: ' + warningText }}</div>
    <SvgButton class="FileBrowserButton" label="Dismiss" @click.stop="warningDismissed" />
  </div>
</template>

<style scoped>
.FileBrowserModal {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  padding: 32px;
  background-color: rgba(0, 0, 0, 0.8);
  border-radius: 0 0 var(--radius-default) var(--radius-default);
  display: flex;
  flex-direction: column;
  gap: 8px;
  align-items: center;
  justify-content: center;
  z-index: 5;
}

.confirmationText {
  color: white;
  text-align: center;
  font-size: 1.2em;
  display: flex;
  overflow: hidden;
}

.confirmationButtons {
  display: flex;
  width: 40%;
  flex-direction: row;
  justify-content: space-between;
}
</style>
