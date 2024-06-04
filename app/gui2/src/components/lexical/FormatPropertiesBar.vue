<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import { type UseFormatting } from '@/components/lexical/formatting'
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import type { Icon } from '@/util/iconName'
import { ref, type Ref } from 'vue'

const props = defineProps<{ formatting: UseFormatting }>()

const menuOpen = ref(false)

const { bold, italic, strikethrough, subscript, superscript, blockType, clearFormatting } =
  props.formatting

function useValueEqualsConstant<T>(value: Ref<T>, constant: T, valueWhenSetToFalse: T) {
  return {
    state: () => value.value === constant,
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
  <ToggleIcon v-bind="bold.state" @update:modelValue="bold.set" icon="bold" title="Bold" />
  <ToggleIcon v-bind="italic.state" @update:modelValue="italic.set" icon="italic" title="Italic" />
  <ToggleIcon
    v-bind="code.state"
    @update:modelValue="code.set"
    :icon="TODO"
    title="Insert Code Block"
  />
  <!-- TODO: Insert link -->
  <DropdownMenu v-model:open="menuOpen">
    <template #button>Aa</template>
    <template #entries>
      <ToggleIcon
        v-bind="strikethrough.state"
        @update:modelValue="strikethrough.set"
        icon="strike-through"
        label="Strikethrough"
        @click="close"
      />
      <ToggleIcon
        v-bind="subscript.state"
        @update:modelValue="subscript.set"
        :icon="TODO"
        label="Subscript"
        @click="close"
      />
      <ToggleIcon
        v-bind="superscript.state"
        @update:modelValue="superscript.set"
        :icon="TODO"
        label="Superscript"
        @click="close"
      />
      <SvgButton
        :name="TODO"
        label="Clear Formatting"
        @click.stop="clearFormatting"
        @click="close"
      />
    </template>
  </DropdownMenu>
</template>
