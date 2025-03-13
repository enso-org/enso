<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuEntry from '@/components/MenuEntry.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import SvgIcon from '@/components/SvgIcon.vue'

const open = defineModel<boolean>('open', { default: false })
</script>

<template>
  <DropdownMenu
    v-model:open="open"
    placement="bottom-start"
    class="ExtendedMenu"
    title="Additional Options"
  >
    <template #button><SvgIcon name="3_dot_menu" class="moreIcon" /></template>
    <template #menu>
      <MenuPanel>
        <MenuEntry action="graph.renameProject" @click="open = false" />
        <MenuEntry action="graph.refreshExecution" />
        <MenuEntry action="graph.recomputeAll" />
        <MenuEntry action="graph.undo" />
        <MenuEntry action="graph.redo" />
        <MenuEntry action="graph.addComponent" />
        <MenuEntry action="graph.fitAll" />
        <MenuEntry action="graph.toggleCodeEditor" />
        <MenuEntry action="graph.toggleDocumentationEditor" />
      </MenuPanel>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.ExtendedMenu {
  --arrow-offset: -5px;
  background: var(--color-frame-bg);
  border-radius: var(--radius-full);
  margin: 0;
}

.moreIcon {
  margin: 4px;
}

.MenuPanel {
  margin-top: 2px;
}
</style>
