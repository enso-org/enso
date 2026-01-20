<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import {
  type SuggestionEntry,
  SuggestionKind,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import {
  defineWidget,
  Score,
  WidgetInput,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { withDropdownItems } from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import { ExpressionTag } from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { injectFunctionInfo } from '@/providers/functionInfo'
import { ANY_TYPE_QN } from '@/util/ensoTypes'
import { ProjectPath } from '@/util/projectPath'
import type { QualifiedName } from '@/util/qualifiedName'
import { map } from 'enso-common/src/utilities/data/iter'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const { suggestionDb, projectNames: names } = useCurrentProject()
const { subjectInfo } = injectFunctionInfo()

const sourceValueType = computed(() => {
  const typePath = subjectInfo?.typeInfo?.primaryType
  return typePath != null ? names.value?.printProjectPath(typePath) : undefined
})

/**
 * Scan the suggestion database and return all types that have a `from` method accepting `sourceType` as its `that`
 * argument.
 */
function* selectPossibleTargetTypes(
  entries: Iterable<SuggestionEntry>,
  sourceType: QualifiedName,
): Iterable<ProjectPath> {
  for (const entry of entries) {
    if (entry.kind !== SuggestionKind.Method || entry.name !== 'from') {
      continue
    }

    const thatArg = entry.arguments[0]
    if (thatArg == null || thatArg.name !== 'that') continue
    const thatType = thatArg.reprType

    if ((thatType === sourceType || thatType === ANY_TYPE_QN) && entry.selfType != null) {
      yield entry.selfType
    }
  }
}

const targetTypeDropdownItems = computed(() => {
  const db = suggestionDb.value
  const sourceType = sourceValueType.value
  if (db == null || sourceType == null) return []

  const items = [
    ...map(selectPossibleTargetTypes(db.entries.values(), sourceType), (ty: ProjectPath) =>
      ExpressionTag.FromProjectPath(db.entries, ty),
    ),
  ].filter((it) => it != null)

  return items.sort((a, b) => a.expression.localeCompare(b.expression))
})

const innerWidgetInput = computed(() => {
  return withDropdownItems(props.input, targetTypeDropdownItems.value)
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 45, // Higher priority than WidgetSelection but lower than specialized widgets
    score: (props) => {
      if (props.input.dynamicConfig?.kind === 'Any_To_Target') return Score.Perfect
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="innerWidgetInput" />
</template>
