<script setup lang="ts">
import ActionButton, { UseActionDescription } from '@/components/ActionButton.vue'
import ActionMenu from '@/components/ActionMenu.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'

import SvgIcon from '@/components/SvgIcon.vue'
</script>

<template>
  <div class="HelpBar">
    <ActionButton class="helpButton" action="help.whatsNew" :label="UseActionDescription" />
    <div class="communityGroup">
      <ActionButton class="helpButton" action="help.community" :label="UseActionDescription" />
      <DropdownMenu class="helpButton" :showArrow="false">
        <template #button><SvgIcon name="folder_opened" /></template>
        <template #menu>
          <ActionMenu
            :actions="[
              'help.gettingStarted',
              'help.askAQuestion',
              'help.componentExamples',
              'help.exampleWorkflows',
            ]"
          />
        </template>
      </DropdownMenu>
    </div>
    <ActionButton class="helpButton" action="help.docs" :label="UseActionDescription" />
    <ActionButton class="helpButton" action="help.contactUs" :label="UseActionDescription" />
  </div>
</template>

<style scoped>
.HelpBar {
  --color-menu-entry-hover-bg: rgb(255 255 255);
  position: relative;
  flex-shrink: 0;
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
  overflow: hidden;
  height: var(--top-bar-height);
  align-items: center;
  gap: 0;
  padding-left: 48px;
  z-index: 1;
}

.communityGroup {
  display: flex;
  flex-direction: row;
  gap: 0;
  align-items: center;

  & > :first-child {
    --button-right-radius: 0;
    --button-padding: 0 6px 0 12px;
    order: -1;
  }

  &::before {
    content: '';
    width: 1px;
    height: 32px;
    background-color: rgba(0 0 0 / 0.1);
  }

  & > :last-child {
    --button-left-radius: 0;
    --button-padding: 0 12px 0 6px;
  }
}

.helpButton {
  height: 32px;
  margin: calc((var(--top-bar-height) - 32px) / 2) 0;
  --button-height: 32px;
  --button-padding: 0 12px;
}
</style>
