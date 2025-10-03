<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { computed } from 'vue'

const { directoryStack } = defineProps<{
  directoryStack: string[]
  enabled: boolean
}>()

const emit = defineEmits<{
  popTo: [number]
}>()

const displayedStack = computed(() => ['Cloud', ...directoryStack])
</script>

<template>
  <div class="FileBrowserBreadcrumbs" :class="{ nonInteractive: !enabled }">
    <ActionButton action="fileBrowser.navigateUp" />
    <div class="breadcrumbs">
      <TransitionGroup>
        <template v-for="(directory, index) in displayedStack" :key="`${index}:${directory}`">
          <SvgIcon v-if="index > 0" name="navigate_breadcrumb" />
          <div
            class="clickable"
            :class="{ nonInteractive: index === displayedStack.length - 1 }"
            @click.stop="emit('popTo', index)"
            v-text="directory"
          ></div>
        </template>
      </TransitionGroup>
    </div>
  </div>
</template>

<style scoped>
.FileBrowserBreadcrumbs {
  display: flex;
  align-items: center;
  flex-grow: 1;
  gap: 8px; /* gap between up button and breadcrumbs */
}

.breadcrumbs {
  --transition-duration: 0.1s;
  display: flex;
  align-items: center;
  gap: 2px; /* breadcrumb spacing */
}

.nonInteractive {
  pointer-events: none;
}

.v-move,
.v-enter-active,
.v-leave-active {
  transition: all var(--transition-duration) ease;
}
.v-enter-from,
.v-leave-to {
  opacity: 0;
  transform: translateX(30px);
}
.list-leave-active {
  position: absolute;
}
</style>
