<script lang="ts">
export const name = 'Warnings'
export const icon = 'exclamation'
export const inputType = 'Any'
export const defaultPreprocessor = [
  'Standard.Visualization.Warnings',
  'process_to_json_text',
] as const
</script>

<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { Pattern } from '@/util/ast/match'
import { useVisualizationConfig, VisualizationContainer } from '@/util/visualizationBuiltins'

type Data = string[]

const props = defineProps<{ data: Data }>()

const config = useVisualizationConfig()

const removeWarnings = Pattern.parse('__.remove_warnings')
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <template #toolbar>
      <button class="image-button" :class="{ active: props.data.length !== 0 }">
        <SvgIcon
          name="not_exclamation"
          class="removeWarnings"
          alt="Remove warnings"
          @click="config.createNode(removeWarnings)"
        />
      </button>
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
</style>
