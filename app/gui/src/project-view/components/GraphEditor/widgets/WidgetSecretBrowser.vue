<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { CustomDropdownItemsKey } from '@/components/GraphEditor/widgets/WidgetSelection.vue'
import {
  ExpressionTag,
  type CustomDropdownItem,
} from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import FileBrowserWidget from '@/components/widgets/FileBrowserWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { requiredImportsByProjectPath } from '@/stores/graph/imports'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Ast } from '@/util/ast'
import { type Identifier } from '@/util/ast/abstract'
import { Pattern } from '@/util/ast/match'
import { ArgumentInfoKey } from '@/util/callTree'
import { methodPointerEquals, type MethodPointer } from '@/util/methodPointer'
import { ProjectPath, printAbsoluteProjectPath } from '@/util/projectPath'
import { qnJoin, type QualifiedName } from '@/util/qualifiedName'
import { computed, h } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const suggestionDb = useSuggestionDbStore()

const inputAllowsText = computed(() =>
  props.input[ArgumentInfoKey]?.info?.reprType?.includes(TEXT_TYPE_QN),
)

function isMethodCall(value: Ast.Expression | string | undefined, methodCall: MethodPointer) {
  if (typeof value !== 'object') return false
  const methodCallInfo = graph.db.getMethodCallInfo(value.id)
  if (!methodCallInfo) return false
  return methodPointerEquals(methodCallInfo.methodCall.methodPointer, methodCall)
}

const inputAst = computed(() =>
  typeof props.input.value === 'object' ? Ast.unwrapGroups(props.input.value) : undefined,
)

const inputIsEnsoSecretExpression = computed(() =>
  isMethodCall(inputAst.value, ENSO_SECRET_CONSTRUCTOR),
)

const currentValue = computed<Ast.TextLiteral | undefined>(() =>
  (
    inputIsEnsoSecretExpression.value &&
    inputAst.value instanceof Ast.App &&
    inputAst.value.argument instanceof Ast.TextLiteral
  ) ?
    inputAst.value.argument
  : undefined,
)

const pathText = computed(() => currentValue.value?.rawTextContent ?? '')

function portUpdate(path: string, module: Ast.MutableModule) {
  return {
    value: Pattern.parseExpression('(Enso_Secret.get __)').instantiate(module, [
      Ast.TextLiteral.new(path, module),
    ]),
    origin: props.input.portId,
  }
}

function widgetUpdate(path: string, currentValue: Ast.TextLiteral | undefined) {
  if (currentValue) {
    return {
      portUpdate: { value: Ast.TextLiteral.new(path), origin: currentValue.id },
    }
  } else {
    const edit = graph.startEdit()
    graph.addMissingImports(
      edit,
      requiredImportsByProjectPath(suggestionDb.entries, ENSO_SECRET_TYPE, true),
    )
    return {
      portUpdate: portUpdate(path, edit),
      edit,
    }
  }
}

const secretBrowserItem: CustomDropdownItem = {
  label: 'Choose secret from cloud...',
  icon: 'key',
  onClick: ({ setActivity, close }) => {
    setActivity(
      computed(() =>
        h(FileBrowserWidget, {
          type: 'secret',
          choosenPath: pathText.value,
          onPathAccepted: (path: string) => {
            props.onUpdate({
              directInteraction: true,
              ...widgetUpdate(path, currentValue.value),
            })
            close()
          },
        }),
      ),
      true,
    )
  },
}

const innerWidgetInput = computed(() => {
  const existingItems = props.input[CustomDropdownItemsKey] ?? []
  return {
    ...props.input,
    [CustomDropdownItemsKey]: [
      ...existingItems,
      secretBrowserItem,
      ...(inputAllowsText.value ? [textItem] : []),
    ],
  }
})
</script>

<script lang="ts">
const ENSO_SECRET_MODULE_PROJECT = 'Standard.Base' as QualifiedName
const ENSO_SECRET_MODULE_PATH = 'Enso_Cloud.Enso_Secret' as QualifiedName
const ENSO_SECRET_MODULE = ProjectPath.create(ENSO_SECRET_MODULE_PROJECT, ENSO_SECRET_MODULE_PATH)
const ENSO_SECRET_TYPE = ProjectPath.create(
  ENSO_SECRET_MODULE_PROJECT,
  qnJoin(ENSO_SECRET_MODULE_PATH, 'Enso_Secret' as QualifiedName),
)
const ENSO_SECRET_CONSTRUCTOR: MethodPointer = {
  module: ENSO_SECRET_MODULE,
  definedOnType: ENSO_SECRET_TYPE,
  name: 'get' as Identifier,
}
const SECRET_TYPE_QN = printAbsoluteProjectPath(ENSO_SECRET_TYPE)
const TEXT_TYPE = ProjectPath.create(
  'Standard.Base' as QualifiedName,
  'Data.Text.Text' as QualifiedName,
)
const TEXT_TYPE_QN = printAbsoluteProjectPath(TEXT_TYPE)

const textItem = new ExpressionTag("''", 'Provide secret as text', 'text')

export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 49,
    score: (props) =>
      (
        props.input.dynamicConfig?.kind === 'Secret_Browse' ||
        props.input[ArgumentInfoKey]?.info?.reprType?.includes(SECRET_TYPE_QN)
      ) ?
        Score.Perfect
      : Score.Mismatch,
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="innerWidgetInput" />
</template>
