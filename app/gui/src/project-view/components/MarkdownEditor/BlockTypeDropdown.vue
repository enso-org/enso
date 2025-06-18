<script setup lang="ts">
import { blockTypeAction } from '@/components/MarkdownEditor/blockTypeActions'
import { type BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import type { SelectionMenuOption } from '@/components/visualizations/toolbar'
import { resolveAction } from '@/providers/action'
import { computed, toValue } from 'vue'

const blockType = defineModel<BlockType | 'Unknown'>({ required: true })

const blockTypesOrdered: BlockType[] = [
  'Paragraph',
  'ATXHeading1',
  'ATXHeading2',
  'ATXHeading3',
  'BulletList',
  'OrderedList',
  'Blockquote',
]

const UNKNOWN_BLOCK_TYPE_OPTION: SelectionMenuOption = {
  icon: 'text',
  label: 'Paragraph type',
  labelExtension: undefined,
  disabled: false,
  hidden: true,
}

function menuOption(key: BlockType | 'Unknown', disableSettingTypes: boolean): SelectionMenuOption {
  if (key === 'Unknown') return UNKNOWN_BLOCK_TYPE_OPTION
  const action = resolveAction(blockTypeAction[key])
  return {
    icon: toValue(action.icon),
    label: toValue(action.description),
    labelExtension: action.shortcut && `(${action.shortcut.humanReadable})`,
    disabled: disableSettingTypes ? key !== blockType.value && key !== 'Paragraph' : false,
    hidden: false,
  }
}

const blockTypeOptions = computed(() => {
  // Always show the current type.
  const shownTypes =
    blockTypesOrdered.includes(blockType.value as BlockType) ? blockTypesOrdered : (
      [...blockTypesOrdered, blockType.value]
    )
  // Code cannot directly be converted to other block types. Switching to `Paragraph` removes the
  // delimiters, and allows whatever is contained to be interpreted as Markdown; once the content is
  // Markdown, further styling changes can be made.
  const disableSettingTypes = blockType.value === 'FencedCode'
  return Object.fromEntries(shownTypes.map((key) => [key, menuOption(key, disableSettingTypes)]))
})
</script>

<template>
  <SelectionDropdown v-model="blockType" :options="blockTypeOptions" labelButton />
</template>
