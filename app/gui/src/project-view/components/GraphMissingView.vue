<script setup lang="ts">
import { useProjectStore } from '$/components/WithCurrentProject.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import StandaloneButton from './StandaloneButton.vue'

const project = useProjectStore()

function goToMain() {
  project.executionContext.desiredStack = [project.executionContext.getStackBottom()]
}
</script>

<template>
  <div class="GraphMissingView">
    <SvgIcon class="header-icon" name="error" />
    <span>The current component could not be found.</span>
    <StandaloneButton icon="home2" label="Go back" @activate="goToMain" />
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
