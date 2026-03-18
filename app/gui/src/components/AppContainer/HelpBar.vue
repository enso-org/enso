<script setup lang="ts">
import { useText } from '$/providers/text'
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'

const text = useText()
</script>

<template>
  <div class="HelpBar">
    <SvgButton class="ensoIcon" name="enso_logo" />
    <MenuButton class="helpButton">{{ text.getText('whatsNew') }}</MenuButton>
    <div class="communityGroup">
      <MenuButton class="helpButton">{{ text.getText('community') }}</MenuButton>
      <DropdownMenu class="helpButton" :showArrow="false">
        <template #button><SvgIcon name="folder_opened" /></template>
        <template #menu>
          <MenuPanel>
            <MenuButton>{{ text.getText('gettingStarted') }}</MenuButton>
            <MenuButton>{{ text.getText('askAQuestion') }}</MenuButton>
            <MenuButton>{{ text.getText('componentExamples') }}</MenuButton>
            <MenuButton>{{ text.getText('exampleWorkflows') }}</MenuButton>
          </MenuPanel>
        </template>
      </DropdownMenu>
    </div>
    <MenuButton class="helpButton">{{ text.getText('docs') }}</MenuButton>
    <MenuButton class="helpButton">{{ text.getText('contactUs') }}</MenuButton>
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
  height: var(--top-bar-height);
  align-items: center;
  gap: 0;
  padding-left: 8px;
  z-index: 1;
}

.ensoIcon {
  --icon-size: 32px;
  margin: 0 8px 0 0;
  padding: 0;
}

.communityGroup {
  display: flex;
  flex-direction: row;
  gap: 0;
  align-items: center;

  & > :first-child {
    --button-right-radius: 0;
    --button-padding: 0 8px 0 6px;
    order: -1;
  }

  &::before {
    content: '';
    width: 1px;
    align-self: stretch;
    background-color: rgba(0 0 0 / 0.1);
  }

  & > :last-child {
    --button-left-radius: 0;
    --button-padding: 0 12px 0 6px;
  }
}

.helpButton {
  height: 32px;
  margin: 0;
  --button-height: 32px;
  --button-padding: 0 12px;
}
</style>
