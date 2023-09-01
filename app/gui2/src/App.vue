<script setup lang="ts">
import ProjectView from '@/views/ProjectView.vue'
import { ref } from 'vue'

const value = ref(0)
</script>

<template>
  <ProjectView class="flex" />
  <Slider :max="100" v-model="value" />
</template>

<style scoped>
.flex {
  flex: 1;
}
</style>
