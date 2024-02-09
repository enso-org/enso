<script setup lang="ts">
import { computed, onMounted } from 'vue'
import { getMainFile, setMainFile } from '../mock/engine'
import App from '../src/App.vue'
import { useGraphStore } from '../src/stores/graph'
import MockProjectStoreWrapper from '../stories/MockProjectStoreWrapper.vue'

const mainFile = computed({
  get() {
    return getMainFile()
  },
  set(value) {
    setMainFile(value)
  },
})

const graphStore = useGraphStore()

onMounted(() => {
  const window_ = window as any
  window_.mockExpressionUpdate = graphStore.mockExpressionUpdate
})
</script>

<template>
  <MockProjectStoreWrapper v-model="mainFile">
    <App :config="{}" :accessToken="''" :metadata="{}" :unrecognizedOptions="[]" />
  </MockProjectStoreWrapper>
</template>

<style scoped>
:is(.viewport) {
  color: var(--color-text);
  font-family: var(--font-code);
  font-size: 11.5px;
  font-weight: 500;
  line-height: 20px;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  height: 100vh;
}
</style>
