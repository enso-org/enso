<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import {
  defineWidget,
  Score,
  WidgetInput,
  widgetProps,
  type UpdateResult,
} from '$/providers/openedProjects/widgetRegistry'
import CodeMirrorWidgetBase from '@/components/GraphEditor/CodeMirrorWidgetBase.vue'
import { createContextStore } from '@/providers'
import { useLayoutAnimationReporter } from '@/providers/animationCounter'
import { registerWidgetActionHandlers } from '@/providers/widgetActions'
import { usePersisted } from '@/stores/persisted'
import { Ast } from '@/util/ast'
import { methodPointerEquals, type MethodPointer } from '@/util/methodPointer'
import { normalizeFunctionName } from '@/util/nameValidation'
import { Err } from 'enso-common/src/utilities/data/result'
import { computed, nextTick, ref, useTemplateRef, watch } from 'vue'
import { MutableModule, PropertyAccess } from 'ydoc-shared/ast'
import type { ExpressionId } from 'ydoc-shared/languageServerTypes'
import NodeWidget from '../NodeWidget.vue'
import { isModuleExpression } from './WidgetFunction/widgetFunctionCallInfo'
import { generateUniqueName, replaceVariableUsages } from './WidgetFunctionDef/argumentAst'

const props = defineProps(widgetProps(widgetDefinition))
const { projectNames: projectNames, module, graph, store: project } = useCurrentProject()
const persisted = usePersisted(true)

const requireUserAction = computed(() => !!props.input[FunctionName].requireUserAction)

const userRequestedEdit = ref(false)
const renameSchedule = useRenameSchedule(true)
if (
  renameSchedule &&
  requireUserAction.value &&
  renameSchedule.matchScheduled(props.input[FunctionName].methodPointer)
) {
  userRequestedEdit.value = true
}

const editFieldEnabled = computed(() => !requireUserAction.value || userRequestedEdit.value)
const animReporter = useLayoutAnimationReporter()
animReporter.reportAnimationWhile(editFieldEnabled)

registerWidgetActionHandlers({
  'component.widget.editMethodName': {
    available: requireUserAction,
    action: () => {
      userRequestedEdit.value = true
    },
  },
})

const cmWidget = useTemplateRef('cmWidget')
watch(cmWidget, (widget) => {
  if (widget && requireUserAction.value) {
    nextTick(() => widget.focusAndSelect())
  }
})

function onBlur() {
  userRequestedEdit.value = false
}

const thisArg = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.lhs : undefined,
)
const operator = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.operator : undefined,
)
const name = computed(() =>
  props.input.value instanceof PropertyAccess ? props.input.value.rhs : props.input.value,
)

const hideThisArg = computed(() => {
  const ast = thisArg.value
  return (
    ast &&
    project.value.moduleProjectPath?.ok &&
    isModuleExpression(ast, project.value.moduleProjectPath.value, projectNames)
  )
})

const nameCode = computed(() => name.value.code())
async function renameFunction(userProvidedName: string): Promise<UpdateResult> {
  const editedName = props.input[FunctionName].editableNameExpression
  const oldMethodPointer = props.input[FunctionName].methodPointer

  const newName = generateUniqueName(
    normalizeFunctionName(userProvidedName),
    module.value.ast?.root(),
  )

  // We can use language-server provided refactoring method, but that introduces a lot of data races
  // that are hard to deal with on the client side, since they all can happen in any order:
  // - refactor call finishing
  // - module code change
  // - execution context method pointer update
  // - suggestion database update
  // To not deal with this complexity, we perform the refactor ourselves on the client side. This also
  // makes the edit transaction undoable by the user without any special handling.
  const newMethodPointer = { ...oldMethodPointer, name: newName }

  // Perform client-side refactor on local module.
  return module.value.edit((edit) => {
    const moduleRoot = edit.root()
    if (!moduleRoot || !project.value.moduleProjectPath?.ok) return Err('Module root missing')
    const editedAstId = graph.value.db.idFromExternal(editedName)
    const originalName = edit.get(editedAstId)?.code()
    if (!originalName) return Err('Original name expression missing')

    // replace all occurences
    const newNameAst = Ast.Ident.new(MutableModule.Transient(), newName)
    const projectPath = project.value.moduleProjectPath.value
    replaceVariableUsages(edit, moduleRoot, originalName, newNameAst, (ast) =>
      isModuleExpression(ast, projectPath, projectNames),
    )
    // Instantly update execution context, so we avoid blinking due to temporarily unsynchronized
    // state and keeps this widget instance rendered. Real updates will arrive soon afterwards and
    // they should have no additional effect.
    rewriteMethodPointer(oldMethodPointer, newMethodPointer)
    return props.updateCallback({ edit, directInteraction: true })
  })
}

function rewriteMethodPointer(oldMethodPointer: MethodPointer, newMethodPointer: MethodPointer) {
  if (methodPointerEquals(oldMethodPointer, newMethodPointer)) return
  graph.value.db.insertSyntheticMethodPointerUpdate(oldMethodPointer, newMethodPointer)
  persisted?.handleModifiedMethodPointer(oldMethodPointer, newMethodPointer)
}

const widgetClass = computed(() => ({
  widgetSingleLine: editFieldEnabled.value,
  widgetParent: !editFieldEnabled.value,
}))
</script>

<script lang="ts">
export const FunctionName: unique symbol = Symbol.for('WidgetInput:FunctionName')
declare module '$/providers/openedProjects/widgetRegistry' {
  export interface WidgetInput {
    [FunctionName]?: {
      /**
       * Id of expression which is accepted by Language Server's
       * [`refactoring/renameSymbol` method](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#refactoringrenamesymbol)
       */
      editableNameExpression: ExpressionId
      methodPointer: MethodPointer
      requireUserAction?: boolean
    }
  }
}

export const [provideRenameSchedule, useRenameSchedule] = createContextStore(
  'functionRenameSchedule',
  () => {
    let scheduledRename: MethodPointer | null = null
    /** Inform the function name widget to start in "renaming" state the next time it is instantiated with a function of given name. */
    function scheduleFunctionRename(pointer: MethodPointer) {
      scheduledRename = pointer
    }

    /** Check if a function has a scheduled rename. If it does, remove it from schedule. */
    function matchScheduled(pointer: MethodPointer) {
      if (scheduledRename && methodPointerEquals(scheduledRename, pointer)) {
        scheduledRename = null
        return true
      }
      return false
    }

    return { scheduleFunctionRename, matchScheduled }
  },
)

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
  <div class="WidgetFunctionName" :class="widgetClass">
    <template v-if="!hideThisArg">
      <NodeWidget v-if="thisArg" :input="WidgetInput.FromAst(thisArg)" />
      <NodeWidget v-if="operator" :input="WidgetInput.FromAst(operator)" />
    </template>
    <div v-if="editFieldEnabled" class="widgetRounded widgetPill">
      <CodeMirrorWidgetBase
        ref="cmWidget"
        v-model="nameCode"
        contentTestId="widget-function-name-content"
        :onAccepted="renameFunction"
        :widgetTypeId="widgetTypeId"
        :input="input"
        lineMode="single"
        @blur="onBlur"
      />
    </div>
    <NodeWidget v-else :input="WidgetInput.FromAst(name)" />
  </div>
</template>

<style scoped>
/*noinspection CssUnusedSymbol*/
.CodeMirrorRoot {
  font-weight: 800;
}
</style>
