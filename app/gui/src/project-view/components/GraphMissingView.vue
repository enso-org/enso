<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { useProjectStore } from '@/stores/project'
import StandaloneButton from './StandaloneButton.vue'

const project = useProjectStore()

function goToMain() {
  project.executionContext.desiredStack = [project.executionContext.getStackBottom()]
}
</script>

<template>
  <div class="GraphMissingView">
    <SvgIcon class="header-icon" name="error" />
    <span>The component you are viewing no longer exists.</span>
    <StandaloneButton icon="home2" label="Go back" @click="goToMain" />
  </div>
</template>

<style scoped>
.GraphMissingView {
  background-image: linear-gradient(to bottom, #00000000, #00000030);
  background-size: 100% 100%;
  position: absolute;
  inset: 0;
  display: flex;
  flex-direction: column;
  gap: 8px;
  align-items: center;
  justify-content: center;
}

.header-icon {
  --icon-size: 64px;
}
</style>
