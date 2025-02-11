<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { HeaderLevel, ListType } from '@/util/codemirror/markdownEditing'
import { Icon } from '@/util/iconMetadata/iconName'
import { ref } from 'vue'

const emit = defineEmits<{
  toggleHeader: [HeaderLevel]
  toggleQuote: []
  toggleList: [ListType]
}>()

interface MenuItem {
  name: string
  icon: Icon
  action: () => void
}
const menuItems: MenuItem[] = [
  { name: 'Header 1', icon: 'header1', action: () => emit('toggleHeader', 1) },
  { name: 'Header 2', icon: 'header2', action: () => emit('toggleHeader', 2) },
  { name: 'Header 3', icon: 'header3', action: () => emit('toggleHeader', 3) },
  { name: 'Quote', icon: 'quote', action: () => emit('toggleQuote') },
  { name: 'Bullet list', icon: 'bullet-list', action: () => emit('toggleList', 'unordered') },
  { name: 'Numbered list', icon: 'numbered-list', action: () => emit('toggleList', 'ordered') },
]

const open = ref(false)
</script>

<template>
  <DropdownMenu v-model:open="open" title="Block type">
    <template #button>
      <SvgIcon name="text3" />
    </template>
    <template #menu>
      <MenuPanel>
        <template v-for="item in menuItems" :key="item.name">
          <MenuButton @click="(item.action(), (open = false))">
            <SvgIcon :name="item.icon" />
            <div class="iconLabel" v-text="item.name" />
          </MenuButton>
        </template>
      </MenuPanel>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.MenuPanel {
  box-shadow: 0 0 1px rgba(0, 0, 0, 0.2);
}

.MenuButton {
  margin: -4px;
  justify-content: unset;
}

.iconLabel {
  margin-left: 4px;
  padding-right: 4px;
}
</style>
