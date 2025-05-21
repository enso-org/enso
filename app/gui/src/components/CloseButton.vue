<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
</script>

<template>
  <SvgButton class="CloseButton" name="tab_close" :extraClickZone="8" />
</template>

<style scoped>
.CloseButton {
  --button-padding: 0px;
  --icon-width: 12px;
  --icon-height: 12px;

  &:hover {
    background-color: rgba(239, 68, 68, 0.8);
  }
}

.App.onMacOs .CloseButton {
  transition: color 0.1s;
  color: rgba(0, 0, 0, 0);
  &:not(:hover) {
    background-color: rgba(0, 0, 0, 0.3);
  }
  &:hover {
    color: rgba(0, 0, 0, 0.9);
  }
}
</style>
