<script lang="ts">
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { useVisualizationConfig } from '@/util/visualizationBuiltins'
import { computed } from 'vue'

export const name = 'Warnings'
export const icon = 'exclamation'
export const inputType = 'Any'
export const defaultPreprocessor = [
  'Standard.Visualization.Warnings',
  'process_to_json_text',
] as const

const removeWarnings = computed(() =>
  Pattern.new((ast) => Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('remove_warnings')!)),
)
</script>

<script setup lang="ts">
type Data = string[]

const props = defineProps<{ data: Data }>()

const config = useVisualizationConfig()

config.setToolbar([
  {
    icon: 'not_exclamation',
    title: 'Remove Warnings',
    disabled: () => props.data.length === 0,
    onClick: () => config.createNodes({ content: removeWarnings.value, commit: true }),
    dataTestid: 'remove-warnings-button',
  },
])
</script>

<template>
  <div class="WarningsVisualization">
    <ul>
      <li v-if="props.data.length === 0">There are no warnings.</li>
      <li v-for="(warning, index) in props.data" :key="index" v-text="warning"></li>
    </ul>
  </div>
</template>

<style scoped>
.WarningsVisualization {
  padding: 8px;
}

ul {
  white-space: pre;
  padding-inline-start: 0;
}

li {
  list-style: none;
}
</style>
