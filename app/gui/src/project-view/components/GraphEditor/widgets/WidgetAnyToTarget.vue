<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { InheritedCallInfo } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import { withDropdownItems } from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import { ExpressionTag } from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { injectFunctionInfo } from '@/providers/functionInfo'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { SuggestionEntry, SuggestionKind } from '@/stores/suggestionDatabase/entry'
import { ArgumentInfoKey } from '@/util/callTree'
import { ANY_TYPE_QN } from '@/util/ensoTypes'
import { MethodPointer, methodPointerEquals } from '@/util/methodPointer'
import { ProjectPath } from '@/util/projectPath'
import { Identifier, QualifiedName } from '@/util/qualifiedName'
import { map } from 'enso-common/src/utilities/data/iter'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const { suggestionDb, names } = useCurrentProject().storesRefs
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
const ANY_MODULE_PATH = ProjectPath.create('Standard.Base' as QualifiedName, 'Any' as QualifiedName)
const ANY_TYPE_PATH = ProjectPath.create(
  'Standard.Base' as QualifiedName,
  'Any.Any' as QualifiedName,
)
const ANY_TO_METHOD_POINTER: MethodPointer = {
  module: ANY_MODULE_PATH,
  definedOnType: ANY_TYPE_PATH,
  name: 'to' as Identifier,
}
const TARGET_ARGUMENT_NAME = 'target_type' as Identifier

function isAnyToMethodCall(methodPointer: MethodPointer): boolean {
  return methodPointerEquals(methodPointer, ANY_TO_METHOD_POINTER)
}

export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 45, // Higher priority than WidgetSelection but lower than specialized widgets
    score: (props) => {
      const argInfo = props.input[ArgumentInfoKey]
      const callInfo = props.input[InheritedCallInfo]
      if (argInfo == null || callInfo == null) return Score.Mismatch
      const isFirstArg = argInfo.info?.name === TARGET_ARGUMENT_NAME
      return isFirstArg && isAnyToMethodCall(callInfo.methodCall.methodPointer) ?
          Score.Perfect
        : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="innerWidgetInput" />
</template>
