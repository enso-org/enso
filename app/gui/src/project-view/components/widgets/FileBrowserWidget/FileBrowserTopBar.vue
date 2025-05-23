<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { computed } from 'vue'

const { directoryStack } = defineProps<{
  enableSecretCreation: boolean
  directoryStack: string[]
  disabled: boolean
}>()

const emit = defineEmits<{
  popTo: [number]
  newDirectory: []
  newSecret: []
}>()

const displayedStack = computed(() => ['Cloud', ...directoryStack])
</script>

<template>
  <div class="FileBrowserTopBar" :class="{ nonInteractive: disabled }">
    <div class="directoryStack">
      <SvgButton
        name="navigate_up"
        title="Up"
        :disabled="disabled || !directoryStack.length"
        @click.stop="emit('popTo', directoryStack.length - 1)"
      />
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
    <SvgButton
      name="folder_add"
      title="New Folder"
      :disabled="disabled"
      @click.stop="emit('newDirectory')"
    />
    <SvgButton
      v-if="enableSecretCreation"
      name="key_add"
      title="New Secret"
      :disabled="disabled"
      @click.stop="emit('newSecret')"
    />
  </div>
</template>

<style scoped>
.FileBrowserTopBar {
  color: white;
  background-color: var(--background-color);
  display: flex;
  flex-direction: row;
  padding: 2px 8px;
  gap: 4px;
}

.directoryStack {
  display: flex;
  align-items: center;
  flex-grow: 1;
  color: white;
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
