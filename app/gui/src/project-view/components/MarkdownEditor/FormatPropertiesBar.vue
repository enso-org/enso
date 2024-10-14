<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import { type UseFormatting } from '@/components/MarkdownEditor/formatting'
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import type { Icon } from '@/util/iconName'
import { computed, ref, type Ref } from 'vue'

const props = defineProps<{ formatting: UseFormatting }>()

const menuOpen = ref(false)

const { bold, italic, strikethrough, subscript, superscript, blockType, clearFormatting } =
  props.formatting

function useValueEqualsConstant<T>(value: Ref<T>, constant: T, valueWhenSetToFalse: T) {
  return {
    state: computed(() => value.value === constant),
    set: (newValue: boolean) => {
      if (newValue && value.value !== constant) {
        value.value = constant
      } else if (!newValue && value.value === constant) {
        value.value = valueWhenSetToFalse
      }
    },
  }
}
const code = useValueEqualsConstant(blockType.state, 'code', 'paragraph')

const TODO: Icon = 'text'

const close = () => (menuOpen.value = false)
</script>

<template>
  <ToggleIcon
    :modelValue="bold.state.value"
    icon="bold"
    title="Bold"
    @update:modelValue="bold.set"
  />
  <ToggleIcon
    :modelValue="italic.state.value"
    icon="italic"
    title="Italic"
    @update:modelValue="italic.set"
  />
  <ToggleIcon
    :modelValue="code.state.value"
    icon="code"
    title="Insert Code Block"
    @update:modelValue="code.set"
  />
  <!-- TODO: Insert link -->
  <DropdownMenu v-model:open="menuOpen">
    <template #button>Aa</template>
    <template #entries>
      <ToggleIcon
        :modelValue="strikethrough.state.value"
        icon="strike-through"
        label="Strikethrough"
        @update:modelValue="strikethrough.set"
        @click="close"
      />
      <ToggleIcon
        :modelValue="subscript.state.value"
        :icon="TODO"
        label="Subscript"
        @update:modelValue="subscript.set"
        @click="close"
      />
      <ToggleIcon
        :modelValue="superscript.state.value"
        :icon="TODO"
        label="Superscript"
        @update:modelValue="superscript.set"
        @click="close"
      />
      <SvgButton
        name="remove-textstyle"
        label="Clear Formatting"
        @click.stop="clearFormatting"
        @click="close"
      />
    </template>
  </DropdownMenu>
</template>
