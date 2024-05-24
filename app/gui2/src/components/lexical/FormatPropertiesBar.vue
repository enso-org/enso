<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import { injectFormatting } from '@/components/lexical/formattingProvider'
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import type { Icon } from '@/util/iconName'
import { computed, type Ref } from 'vue'

const { bold, italic, strikethrough, subscript, superscript, blockType, clearFormatting } =
  injectFormatting()

function useEnumerationToggle<T>(current: Ref<T>, select: T, defaultValue: T) {
  return computed({
    get: () => current.value === select,
    set: (value) => {
      if (value && current.value !== select) {
        current.value = select
      } else if (!value && current.value === select) {
        current.value = defaultValue
      }
    },
  })
}
const code = useEnumerationToggle(blockType, 'code', 'paragraph')

const TODO: Icon = 'text'
</script>

<template>
  <ToggleIcon v-model="bold" icon="bold" title="Bold" />
  <ToggleIcon v-model="italic" icon="italic" title="Italic" />
  <ToggleIcon v-model="code" :icon="TODO" title="Insert Code Block" />
  <!-- TODO: Insert link -->
  <DropdownMenu>
    <template #button>Aa</template>
    <template #entries>
      <ToggleIcon v-model="strikethrough" icon="strike-through" label="Strikethrough" />
      <ToggleIcon v-model="subscript" :icon="TODO" label="Subscript" />
      <ToggleIcon v-model="superscript" :icon="TODO" label="Superscript" />
      <SvgButton :name="TODO" label="Clear Formatting" @click.stop="clearFormatting" />
    </template>
  </DropdownMenu>
</template>
