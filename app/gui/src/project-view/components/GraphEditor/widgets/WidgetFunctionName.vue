<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { usePersisted } from '@/stores/persisted'
import { useProjectStore } from '@/stores/project'
import { injectProjectNames } from '@/stores/projectNames'
import { Ast } from '@/util/ast'
import { useCodeMirror, useStringSync } from '@/util/codemirror'
import { Err, Ok, type Result } from '@/util/data/result'
import { type MethodPointer } from '@/util/methodPointer'
import { type IdentifierOrOperatorIdentifier } from '@/util/qualifiedName'
import { useToast } from '@/util/toast'
import { type ComponentInstance, computed, useTemplateRef, watch } from 'vue'
import { PropertyAccess } from 'ydoc-shared/ast'
import { type ExpressionId } from 'ydoc-shared/languageServerTypes'
import NodeWidget from '../NodeWidget.vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore(true)
const persisted = usePersisted(true)
const projectNames = injectProjectNames()

const project = useProjectStore()
const renameError = useToast.error()

const thisArg = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.lhs : undefined,
)
const operator = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.operator : undefined,
)
const name = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.rhs : props.input.value,
)

const nameCode = computed(() => name.value.code())

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const { syncExt, connectSync } = useStringSync()
const { editorView } = useCodeMirror(editorRoot, {
  content: nameCode.value,
  extensions: [syncExt],
  readonly: false,
  contentTestId: 'widget-function-name-content',
  lineMode: 'single',
})

const { getText, setText } = connectSync(editorView)
watch(nameCode, (text) => setText(text))

async function newNameAccepted() {
  const newName = getText()
  if (newName !== nameCode.value) {
    const result = await renameFunction(newName)
    if (!result.ok) {
      renameError.reportError(result.error)
      setText(nameCode.value)
    }
  }
}

async function renameFunction(newName: string): Promise<Result> {
  if (!project.moduleProjectPath?.ok) return project.moduleProjectPath ?? Err('Unknown module Path')
  const modPath = projectNames.serializeProjectPathForBackend(project.moduleProjectPath.value)
  const editedName = props.input[FunctionName].editableNameExpression
  const oldMethodPointer = props.input[FunctionName].methodPointer
  const refactorResult = await project.lsRpcConnection.renameSymbol(modPath, editedName, newName)
  if (!refactorResult.ok) return refactorResult
  if (oldMethodPointer) {
    const newMethodPointer = {
      ...oldMethodPointer,
      name: refactorResult.value.newName as IdentifierOrOperatorIdentifier,
    }
    graph?.db.insertSyntheticMethodPointerUpdate(oldMethodPointer, newMethodPointer)
    persisted?.handleModifiedMethodPointer(oldMethodPointer, newMethodPointer)
  }
  return Ok()
}
</script>

<script lang="ts">
export const FunctionName: unique symbol = Symbol.for('WidgetInput:FunctionName')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [FunctionName]?: {
      /**
       * Id of expression which is accepted by Language Server's
       * [`refactoring/renameSymbol` method](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#refactoringrenamesymbol)
       */
      editableNameExpression: ExpressionId
      methodPointer: MethodPointer
    }
  }
}

function isFunctionName(input: WidgetInput): input is WidgetInput & {
  value: Ast.Ast
  [FunctionName]: { editableNameExpression: ExpressionId }
} {
  return WidgetInput.isAst(input) && FunctionName in input
}

export const widgetDefinition = defineWidget(
  isFunctionName,
  {
    priority: 2,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetFunctionName widgetRounded">
    <NodeWidget v-if="thisArg" :input="WidgetInput.FromAst(thisArg)" />
    <NodeWidget v-if="operator" :input="WidgetInput.FromAst(operator)" />
    <div class="widgetApplyPadding">
      <CodeMirrorRoot ref="editorRoot" @focusout="newNameAccepted" @keydown.enter.stop />
    </div>
  </div>
</template>

<style scoped>
.WidgetFunctionName {
  display: inline-flex;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  justify-content: center;
  align-items: center;
  min-width: var(--node-port-height);
  color: var(--color-node-text);

  &:has(> :focus) {
    outline: none;
    background: var(--color-widget-focus);
    color: var(--color-node-text-selected);
  }

  &:deep(::selection) {
    background: var(--color-widget-selection);
  }
}

/*noinspection CssUnusedSymbol*/
.CodeMirrorRoot {
  font-weight: 800;
}
</style>
