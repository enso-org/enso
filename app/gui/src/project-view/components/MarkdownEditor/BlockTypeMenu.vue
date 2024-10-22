<script setup lang="ts">
import { blockTypeToBlockName, type BlockType } from '@/components/MarkdownEditor/formatting'
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import type { Icon } from '@/util/iconName'

const blockType = defineModel<BlockType>({ required: true })

const blockTypeIcon: Record<keyof typeof blockTypeToBlockName, Icon> = {
  paragraph: 'text',
  bullet: 'bullet-list',
  code: 'code',
  h1: 'header1',
  h2: 'header2',
  h3: 'header3',
  number: 'numbered-list',
  quote: 'quote',
}
const blockTypesOrdered: BlockType[] = [
  'paragraph',
  'h1',
  'h2',
  'h3',
  'code',
  'bullet',
  'number',
  'quote',
]

const blockTypeOptions = Object.fromEntries(
  blockTypesOrdered.map((key) => [
    key,
    { icon: blockTypeIcon[key], label: blockTypeToBlockName[key] },
  ]),
)
</script>

<template>
  <SelectionDropdown v-model="blockType" :options="blockTypeOptions" labelButton />
</template>
