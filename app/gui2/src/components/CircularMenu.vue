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
  position: absolute;
  clip-path: path('m0 16a52 52 0 0 0 52 52a16 16 0 0 0 0 -32a20 20 0 0 1-20-20a16 16 0 0 0-32 0');
  background: rgba(255, 255, 255, 80%);
  backdrop-filter: blur(64px);
  top: 0px;
  left: -36px;
  width: 68px;
  height: 68px;
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
  left: 9px;
  top: 8px;
}

.docs-button {
  position: absolute;
  left: 18.54px;
  top: 33.46px;
}

.visualization-button {
  position: absolute;
  left: 44px;
  top: 44px;
}
</style>
