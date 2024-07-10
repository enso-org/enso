<script setup lang="ts">
import ExtendedMenu from '@/components/ExtendedMenu.vue'
import NavBar from '@/components/NavBar.vue'
import RecordControl from '@/components/RecordControl.vue'
import SelectionMenu from '@/components/SelectionMenu.vue'
import { injectGuiConfig } from '@/providers/guiConfig'
import { computed } from 'vue'

const showColorPicker = defineModel<boolean>('showColorPicker', { required: true })
const showCodeEditor = defineModel<boolean>('showCodeEditor', { required: true })
const showDocumentationEditor = defineModel<boolean>('showDocumentationEditor', { required: true })
const props = defineProps<{
  zoomLevel: number
  componentsSelected: number
}>()
const emit = defineEmits<{
  fitToAllClicked: []
  zoomIn: []
  zoomOut: []
  collapseNodes: []
  removeNodes: []
}>()

const LEFT_PADDING_PX = 11

const config = injectGuiConfig()

const barStyle = computed(() => {
  const offset = Number(config.value.window?.topBarOffset ?? '0')
  return {
    marginLeft: `${offset + LEFT_PADDING_PX}px`,
  }
})
</script>

<template>
  <div class="TopBar" :style="barStyle">
    <NavBar />
    <RecordControl />
    <Transition name="selection-menu">
      <SelectionMenu
        v-if="componentsSelected > 1"
        v-model:showColorPicker="showColorPicker"
        :selectedComponents="componentsSelected"
        @collapseNodes="emit('collapseNodes')"
        @removeNodes="emit('removeNodes')"
      />
    </Transition>
    <ExtendedMenu
      v-model:showCodeEditor="showCodeEditor"
      v-model:showDocumentationEditor="showDocumentationEditor"
      :zoomLevel="props.zoomLevel"
      @fitToAllClicked="emit('fitToAllClicked')"
      @zoomIn="emit('zoomIn')"
      @zoomOut="emit('zoomOut')"
    />
  </div>
</template>

<style scoped>
.TopBar {
  position: absolute;
  display: flex;
  gap: 8px;
  top: 8px;
  left: 0;
  right: 0;
  pointer-events: none;
  > * {
    pointer-events: auto;
  }
}

.TopBar.extraRightSpace {
  right: 32px;
}

.selection-menu-enter-active,
.selection-menu-leave-active {
  transition: opacity 0.25s ease;
}

.selection-menu-enter-from,
.selection-menu-leave-to {
  opacity: 0;
}
</style>
