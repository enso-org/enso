<script setup lang="ts">
import { useText } from '$/providers/text'
import ControlGroup from '@/components/ControlGroup.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import SvgIcon from '@/components/SvgIcon.vue'

const text = useText()
</script>

<template>
  <div class="HelpBar">
    <SvgIcon class="ensoIcon" name="enso_logo" />
    <MenuButton
      ><span>{{ text.getText('whatsNew') }}</span></MenuButton
    >
    <ControlGroup>
      <MenuButton>{{ text.getText('community') }}</MenuButton>
      <DropdownMenu>
        <template #button><SvgIcon name="3_dot_menu" class="moreIcon" /></template>
        <template #menu>
          <MenuPanel>
            <MenuButton>{{ text.getText('gettingStarted') }}</MenuButton>
            <MenuButton>{{ text.getText('askAQuestion') }}</MenuButton>
            <MenuButton>{{ text.getText('componentExamples') }}</MenuButton>
            <MenuButton>{{ text.getText('exampleWorkflows') }}</MenuButton>
          </MenuPanel>
        </template>
      </DropdownMenu>
    </ControlGroup>
    <MenuButton>{{ text.getText('docs') }}</MenuButton>
    <MenuButton>{{ text.getText('contactUs') }}</MenuButton>
  </div>
</template>

<style scoped>
.HelpBar {
  --color-menu-entry-hover-bg: rgb(255 255 255);
  flex-shrink: 0;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  overflow: hidden;
  background-color: rgba(0, 0, 0, 0.1);
  height: var(--top-bar-height);
  align-items: center;
  gap: 8px;
  padding-left: 8px;
  z-index: 1;
}

.ensoIcon {
  --icon-size: 32px;
}

.MenuButton {
  height: 32px;
}
</style>
