<script setup lang="ts">
import DocsIcon from '@/assets/icons/docs.svg'
import EyeIcon from '@/assets/icons/eye.svg'
import NoAutoReplayIcon from '@/assets/icons/no_auto_replay.svg'

const props = defineProps<{
  isAutoEvaluationDisabled: boolean
  isDocsVisible: boolean
  isVisualizationVisible: boolean
}>()
const emit = defineEmits<{
  'update:isAutoEvaluationDisabled': [isAutoEvaluationDisabled: boolean]
  'update:isDocsVisible': [isDocsVisible: boolean]
  'update:isVisualizationVisible': [isVisualizationVisible: boolean]
}>()
</script>

<template>
  <div class="CircularMenu">
    <div class="background"></div>
    <button
      :class="{ button: true, 'no-auto-evaluate-button': true, active: isAutoEvaluationDisabled }"
      @click="emit('update:isAutoEvaluationDisabled', !isAutoEvaluationDisabled)"
    >
      <img :src="NoAutoReplayIcon" />
    </button>
    <button
      :class="{ button: true, 'docs-button': true, active: isDocsVisible }"
      @click="emit('update:isDocsVisible', !isDocsVisible)"
    >
      <img :src="DocsIcon" />
    </button>
    <button
      :class="{ button: true, 'visualization-button': true, active: isVisualizationVisible }"
      @click="emit('update:isVisualizationVisible', !isVisualizationVisible)"
    >
      <img :src="EyeIcon" />
    </button>
  </div>
</template>

<style scoped>
.CircularMenu {
  user-select: none;
  position: absolute;
  top: 0px;
  left: -40px;
  width: 76px;
  height: 76px;
}

.CircularMenu > .background {
  position: absolute;
  clip-path: path('m0 20a56 56 0 0 0 56 56a16 16 0 0 0 0 -32a24 24 0 0 1-24-24a16 16 0 0 0-32 0');
  background: var(--color-app-bg);
  backdrop-filter: var(--blur-app-bg);
  width: 100%;
  height: 100%;
}

.button {
  cursor: pointer;
  background: none;
  padding: 0;
  border: none;
  opacity: 30%;
}

.button.active {
  opacity: unset;
}

.no-auto-evaluate-button {
  position: absolute;
  left: 11px;
  top: 10px;
}

.docs-button {
  position: absolute;
  left: 20px;
  top: 38px;
}

.visualization-button {
  position: absolute;
  left: 48px;
  top: 52px;
}
</style>
