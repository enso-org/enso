<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import { type UseFormatting } from '@/components/lexical/formatting'
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import type { Icon } from '@/util/iconName'
import { computed, ref, type Ref } from 'vue'

const props = defineProps<{ formatting: UseFormatting }>()

const menuOpen = ref(false)

const { bold, italic, strikethrough, subscript, superscript, blockType, clearFormatting } =
  props.formatting

function useValueEqualsConstant<T>(value: Ref<T>, constant: T, valueWhenSetToFalse: T) {
  return computed({
    get: () => value.value === constant,
    set: (newValue) => {
      if (newValue && value.value !== constant) {
        value.value = constant
      } else if (!newValue && value.value === constant) {
        value.value = valueWhenSetToFalse
      }
    },
  })
}
const code = useValueEqualsConstant(blockType, 'code', 'paragraph')

const TODO: Icon = 'text'

const close = () => (menuOpen.value = false)
</script>

<template>
  <ToggleIcon v-model="bold" icon="bold" title="Bold" />
  <ToggleIcon v-model="italic" icon="italic" title="Italic" />
  <ToggleIcon v-model="code" :icon="TODO" title="Insert Code Block" />
  <!-- TODO: Insert link -->
  <DropdownMenu v-model:open="menuOpen">
    <template #button>Aa</template>
    <template #entries>
      <ToggleIcon
        v-model="strikethrough"
        icon="strike-through"
        label="Strikethrough"
        @click="close"
      />
      <ToggleIcon v-model="subscript" :icon="TODO" label="Subscript" @click="close" />
      <ToggleIcon v-model="superscript" :icon="TODO" label="Superscript" @click="close" />
      <SvgButton
        :name="TODO"
        label="Clear Formatting"
        @click.stop="clearFormatting"
        @click="close"
      />
    </template>
  </DropdownMenu>
</template>
