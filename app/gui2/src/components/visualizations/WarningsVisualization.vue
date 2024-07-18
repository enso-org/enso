<script lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { useVisualizationConfig, VisualizationContainer } from '@/util/visualizationBuiltins'
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
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <template #toolbar>
      <div class="inner-toolbar">
        <SvgButton
          name="not_exclamation"
          data-testid="remove-warnings-button"
          title="Remove Warnings"
          :disabled="props.data.length === 0"
          @click="config.createNodes({ content: removeWarnings, commit: true })"
        />
      </div>
    </template>
    <template #default>
      <div class="WarningsVisualization">
        <ul>
          <li v-if="props.data.length === 0">There are no warnings.</li>
          <li v-for="(warning, index) in props.data" :key="index" v-text="warning"></li>
        </ul>
      </div>
    </template>
  </VisualizationContainer>
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

.inner-toolbar {
  position: relative;
  display: flex;
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;
  z-index: 20;

  &:before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
    border-radius: var(--radius-full);
    background: var(--color-app-bg);
    backdrop-filter: var(--blur-app-bg);
  }
}
</style>
