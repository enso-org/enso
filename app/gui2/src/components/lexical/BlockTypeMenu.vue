<script setup lang="ts">
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { blockTypeToBlockName, type BlockType } from '@/components/lexical/formatting'
import { injectFormatting } from '@/components/lexical/formattingProvider'
import { assert } from '@/util/assert'
import type { Icon } from '@/util/iconName'

const { blockType } = injectFormatting()

const TODO: Icon = 'text'
const blockTypeIcon: Record<keyof typeof blockTypeToBlockName, Icon> = {
  paragraph: 'text',
  bullet: TODO,
  code: TODO,
  h1: TODO,
  h2: TODO,
  h3: TODO,
  number: TODO,
  quote: TODO,
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
for (const type of Object.keys(blockTypeToBlockName))
  assert(blockTypesOrdered.includes(type as keyof typeof blockTypeToBlockName))
</script>

<template>
  <SelectionDropdown v-model="blockType" :values="blockTypesOrdered">
    <template v-slot="{ value }">
      <SvgIcon :name="blockTypeIcon[value]" />
      <div class="iconLabel" v-text="blockTypeToBlockName[value]" />
    </template>
  </SelectionDropdown>
</template>
