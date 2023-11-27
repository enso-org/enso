<script setup lang="ts">
import { computed } from 'vue'
import App from '../src/App.vue'
import MockProjectStoreWrapper from '../stories/MockProjectStoreWrapper.vue'
import { getMainFile, setMainFile } from './mockEngine'

const mainFile = computed({
  get() {
    return getMainFile()
  },
  set(value) {
    setMainFile(value)
  },
})
</script>

<template>
  <MockProjectStoreWrapper v-model="mainFile"><App :config="{}" /></MockProjectStoreWrapper>
</template>

<style scoped>
:is(.viewport) {
  color: var(--color-text);
  font-family: 'M PLUS 1', sans-serif;
  font-size: 11.5px;
  font-weight: 500;
  line-height: 20px;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  height: 100vh;
}
</style>
