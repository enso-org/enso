<script setup lang="ts">
import { type BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import { type Icon } from '@/util/iconMetadata/iconName'
import { computed } from 'vue'

const blockType = defineModel<BlockType | 'Unknown'>({ required: true })

const blockIcon: Record<BlockType | 'Unknown', Icon> = {
  Unknown: 'text',
  Paragraph: 'text',
  BulletList: 'bullet-list',
  ATXHeading1: 'header1',
  ATXHeading2: 'header2',
  ATXHeading3: 'header3',
  OrderedList: 'numbered-list',
  Blockquote: 'quote',
  FencedCode: 'code',
}
const blockName: Record<BlockType | 'Unknown', string> = {
  Unknown: 'Paragraph type',
  Paragraph: 'Normal',
  BulletList: 'List',
  ATXHeading1: 'Header 1',
  ATXHeading2: 'Header 2',
  ATXHeading3: 'Header 3',
  OrderedList: 'Numbered List',
  Blockquote: 'Quote',
  FencedCode: 'Code',
}
const blockTypesOrdered: BlockType[] = [
  'Paragraph',
  'ATXHeading1',
  'ATXHeading2',
  'ATXHeading3',
  'BulletList',
  'OrderedList',
  'Blockquote',
]

const blockTypeOptions = computed(() => {
  // Always show the current type.
  const shownTypes =
    blockTypesOrdered.includes(blockType.value as BlockType) ? blockTypesOrdered : (
      [...blockTypesOrdered, blockType.value]
    )
  // Code cannot directly be converted to other block types. Switching to `Paragraph` removes the delimiters, and allows
  // whatever is contained to be interpreted as Markdown; once the content is Markdown, further styling changes can be
  // made.
  const disableSettingTypes = blockType.value === 'FencedCode'
  return Object.fromEntries(
    shownTypes.map((key) => [
      key,
      {
        icon: blockIcon[key],
        label: blockName[key],
        disabled: disableSettingTypes ? key !== blockType.value && key !== 'Paragraph' : false,
        hidden: key === 'Unknown',
      },
    ]),
  )
})
</script>

<template>
  <SelectionDropdown v-model="blockType" :options="blockTypeOptions" labelButton />
</template>
