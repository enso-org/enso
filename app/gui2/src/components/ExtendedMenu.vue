<script setup lang="ts">
import { codeEditorBindings, documentationEditorBindings } from '@/bindings'
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { ref } from 'vue'

const showCodeEditor = defineModel<boolean>('showCodeEditor', { required: true })
const showDocumentationEditor = defineModel<boolean>('showDocumentationEditor', { required: true })
const props = defineProps<{
  zoomLevel: number
}>()
const emit = defineEmits<{
  zoomIn: []
  zoomOut: []
  fitToAllClicked: []
}>()

const open = ref(false)

const toggleCodeEditorShortcut = codeEditorBindings.bindings.toggle.humanReadable
const toggleDocumentationEditorShortcut = documentationEditorBindings.bindings.toggle.humanReadable
</script>

<template>
  <DropdownMenu
    v-model:open="open"
    placement="bottom-end"
    class="ExtendedMenu"
    title="Additional Options"
  >
    <template #button><SvgIcon name="3_dot_menu" class="moreIcon" /></template>
    <template #entries>
      <div>
        <div class="nonInteractive"><SvgIcon name="zoom" class="rowIcon" />Zoom</div>
        <div class="zoomControl rightSide">
          <SvgButton
            class="zoomButton"
            name="minus"
            title="Decrease Zoom"
            @click="emit('zoomOut')"
          />
          <span
            class="zoomScaleLabel"
            v-text="props.zoomLevel ? props.zoomLevel.toFixed(0) + '%' : '?'"
          ></span>
          <SvgButton class="zoomButton" name="add" title="Increase Zoom" @click="emit('zoomIn')" />
          <div class="divider"></div>
          <SvgButton
            name="show_all"
            class="showAllIcon"
            title="Show All Components"
            @click="emit('fitToAllClicked')"
          />
        </div>
      </div>
      <MenuButton v-model="showCodeEditor" @click="open = false">
        <SvgIcon name="bottom_panel" class="rowIcon" />
        Code Editor
        <div class="rightSide" v-text="toggleCodeEditorShortcut" />
      </MenuButton>
      <MenuButton v-model="showDocumentationEditor" @click="open = false">
        <SvgIcon name="right_panel" class="rowIcon" />
        Documentation Editor
        <div class="rightSide" v-text="toggleDocumentationEditorShortcut" />
      </MenuButton>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.ExtendedMenu {
  background: var(--color-frame-bg);
  border-radius: var(--radius-full);
  margin: 0 12px 0 auto;
}

.moreIcon {
  margin: 4px;
}

:deep(.DropdownMenuContent) {
  width: 250px;
  margin-top: 2px;
  padding: 4px;

  > * {
    display: flex;
    align-items: center;
    padding-left: 8px;
    padding-right: 8px;
  }
}

.toggledOn {
  background-color: var(--color-menu-entry-selected-bg);
}

.rowIcon {
  display: inline-block;
  margin-right: 4px;
}

.rightSide {
  margin-left: auto;
}

.nonInteractive {
  user-select: none;
  pointer-events: none;
}

.divider {
  border-left: 1px solid var(--color-text);
  border-right: 1px solid var(--color-text);
  height: 17px;
  margin-left: 10px;
  margin-right: 10px;
  opacity: 0.3;
}

.zoomControl {
  display: flex;
  gap: 4px;
  align-items: center;
}

.zoomScaleLabel {
  width: 4em;
  text-align: center;
}
</style>
