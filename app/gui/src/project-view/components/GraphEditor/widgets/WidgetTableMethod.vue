<script lang="ts" setup>
import { widgetProps } from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectFunctionInfo } from '@/providers/functionInfo'
import { provideTableContext } from '@/providers/tableContext'
import { computed } from 'vue'

defineProps(widgetProps(widgetDefinition))

const fi = injectFunctionInfo()

provideTableContext(computed(() => fi.subject?.externalId))
</script>

<script lang="ts">
import { defineWidget, Score } from '$/providers/openedProjects/widgetRegistry'
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import { ProjectPath } from '@/util/projectPath'
import type { Identifier, QualifiedName } from '@/util/qualifiedName'

const TABLE_MODULE_PATH = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Table' as QualifiedName,
)
const TABLE_TYPE_PATH = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Table.Table' as QualifiedName,
)
function tableMethod(name: Identifier) {
  return {
    module: TABLE_MODULE_PATH,
    definedOnType: TABLE_TYPE_PATH,
    name,
  }
}

export const widgetDefinition = defineWidget(
  WidgetInputIsSpecificMethodCall(tableMethod('set' as Identifier)),
  {
    priority: 999,
    score: () => Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="input" />
</template>
