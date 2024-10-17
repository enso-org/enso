<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useWidgetFunctionCallInfo } from '@/components/GraphEditor/widgets/WidgetFunction/widgetFunctionCallInfo'
import { FunctionName } from '@/components/GraphEditor/widgets/WidgetFunctionName.vue'
import { injectFunctionInfo, provideFunctionInfo } from '@/providers/functionInfo'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
  type WidgetUpdate,
} from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import type { MethodCallInfo } from '@/stores/graph/graphDatabase'
import { useProjectStore } from '@/stores/project'
import { assert, assertUnreachable } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import {
  ArgumentApplication,
  ArgumentApplicationKey,
  ArgumentAst,
  ArgumentPlaceholder,
  getMethodCallInfoRecursively,
} from '@/util/callTree'
import { partitionPoint } from '@/util/data/array'
import { isIdentifier } from '@/util/qualifiedName'
import { computed, proxyRefs } from 'vue'
import { methodPointerEquals, type MethodPointer } from 'ydoc-shared/languageServerTypes'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const project = useProjectStore()

const exprInfo = computed(() => graph.db.getExpressionInfo(props.input.value.externalId))
const outputType = computed(() => exprInfo.value?.typename)

const { methodCallInfo, application } = useWidgetFunctionCallInfo(
  () => props.input,
  graph.db,
  project,
)

provideFunctionInfo(
  proxyRefs({
    prefixCalls: computed(() => {
      const ids = new Set<AstId>([props.input.value.id])
      let ast: any = props.input.value
      while (ast instanceof Ast.App) {
        ids.add(ast.function.id)
        ast = ast.function
      }
      return ids
    }),
    callInfo: methodCallInfo,
    outputType,
  }),
)

const innerInput = computed(() => {
  let input: WidgetInput
  if (application.value instanceof ArgumentApplication) {
    input = application.value.toWidgetInput()
  } else {
    input = { ...props.input }
  }
  const callInfo = methodCallInfo.value
  if (callInfo) {
    input[CallInfo] = callInfo
    if (input.value instanceof Ast.PropertyAccess || input.value instanceof Ast.Ident) {
      const definition = graph.getMethodAst(callInfo.methodCall.methodPointer)
      if (definition.ok) input[FunctionName] = { editableName: definition.value.name.externalId }
    }
  }
  return input
})

/**
 * Process an argument value update. Takes care of inserting assigned placeholder values, as well as
 * handling deletions of arguments and rewriting the applications to named as appropriate.
 */
function handleArgUpdate(update: WidgetUpdate): boolean {
  const app = application.value
  if (update.portUpdate && app instanceof ArgumentApplication) {
    const { value, origin } = update.portUpdate
    const edit = update.edit ?? graph.startEdit()
    // Find the updated argument by matching origin port/expression with the appropriate argument.
    // We are interested only in updates at the top level of the argument AST. Updates from nested
    // widgets do not need to be processed at the function application level.
    const applications = [...app.iterApplications()]
    const argAppIndex = applications.findIndex(
      (app) => 'portId' in app.argument && app.argument.portId === origin,
    )
    const argApp = applications[argAppIndex]

    // Perform appropriate AST update, either insertion or deletion.
    if (value != null && argApp?.argument instanceof ArgumentPlaceholder) {
      /* Case: Inserting value to a placeholder. */
      let newArg: Ast.Owned
      if (value instanceof Ast.Ast) {
        newArg = value
      } else {
        newArg = Ast.parse(value, edit)
      }
      const name =
        argApp.argument.insertAsNamed && isIdentifier(argApp.argument.argInfo.name) ?
          argApp.argument.argInfo.name
        : undefined
      edit
        .getVersion(argApp.appTree)
        .updateValue((oldAppTree) => Ast.App.new(edit, oldAppTree, name, newArg))
      props.onUpdate({ edit })
      return true
    } else if (value == null && argApp?.argument instanceof ArgumentAst) {
      /* Case: Removing existing argument. */

      // HACK: Temporarily modify expression info to include the deleted argument on a list, so it
      // immediately appears back as a placeholder after deletion, before the engine respones.
      // The engine will soon send another expression update, overwriting this change anyway.
      //
      // This update is unfortunately not saved in the undo stack. Undoing and redoing the edit will
      // still cause the placeholder to glitch out temporarily, but this is good enough for now.
      // Proper fix would involve adding a proper "optimistic response" mechanism that can also be
      // saved in the undo transaction.
      const deletedArgIdx = argApp.argument.index
      if (deletedArgIdx != null && methodCallInfo.value) {
        // Grab original expression info data straight from DB, so we modify the original state.
        const notAppliedArguments = graph.db.getExpressionInfo(
          methodCallInfo.value.methodCallSource,
        )?.methodCall?.notAppliedArguments
        if (notAppliedArguments != null) {
          const insertAt = partitionPoint(notAppliedArguments, (i) => i < deletedArgIdx)
          if (notAppliedArguments[insertAt] != deletedArgIdx) {
            // Insert the deleted argument back to the method info. This directly modifies observable
            // data in `ComputedValueRegistry`. That's on purpose.
            notAppliedArguments.splice(insertAt, 0, deletedArgIdx)
          }
        }
      }

      if (argApp.appTree instanceof Ast.App && argApp.appTree.argumentName != null) {
        /* Case: Removing named prefix argument. */

        // Named argument can always be removed immediately. Replace the whole application with its
        // target, effectively removing the argument from the call.
        const func = edit.take(argApp.appTree.function.id)
        assert(func != null)
        props.onUpdate({
          edit,
          portUpdate: {
            value: func,
            origin: argApp.appTree.id,
          },
        })
        return true
      } else if (argApp.appTree instanceof Ast.OprApp) {
        /* Case: Removing infix application. */

        // Infix application is removed as a whole. Only the target is kept.
        if (argApp.appTree.lhs) {
          const lhs = edit.take(argApp.appTree.lhs.id)
          props.onUpdate({
            edit,
            portUpdate: {
              value: lhs,
              origin: argApp.appTree.id,
            },
          })
        }
        return true
      } else if (argApp.appTree instanceof Ast.App && argApp.appTree.argumentName == null) {
        /* Case: Removing positional prefix argument. */

        // Since the update of this kind can affect following arguments, it may be necessary to
        // replace the AST for multiple levels of application.

        // Traverse the application chain, starting from the outermost application and going
        // towards the innermost target.
        for (const innerApp of applications) {
          if (innerApp.appTree.id === argApp.appTree.id) {
            // Found the application with the argument to remove. Skip the argument and use the
            // application target's code. This is the final iteration of the loop.
            const appTree = edit.getVersion(argApp.appTree)
            if (graph.db.isNodeId(appTree.externalId)) {
              // If the modified application is a node root, preserve its identity and metadata.
              appTree.replaceValue(appTree.function.take())
            } else {
              appTree.replace(appTree.function.take())
            }
            props.onUpdate({ edit })
            return true
          } else {
            // Process an argument to the right of the removed argument.
            assert(innerApp.appTree instanceof Ast.App)
            const infoName = innerApp.argument.argInfo?.name
            // Positional arguments following the deleted argument must all be rewritten to named.
            if (infoName && isIdentifier(infoName) && !innerApp.appTree.argumentName) {
              edit.getVersion(innerApp.appTree).setArgumentName(infoName)
            }
          }
        }
        assertUnreachable()
      } else if (value == null && argApp.argument instanceof ArgumentPlaceholder) {
        /* Case: Removing placeholder value. */
        // Do nothing. The argument already doesn't exist, so there is nothing to update.
        return true
      } else {
        // Any other case is handled by the default handler.
        return false
      }
    }
  }
  return false
}
</script>
<script lang="ts">
export const CallInfo: unique symbol = Symbol.for('WidgetInput:CallInfo')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [CallInfo]?: MethodCallInfo
  }
}

export const WidgetInputIsSpecificMethodCall =
  (methodPointer: MethodPointer) =>
  (
    input: WidgetInput,
  ): input is WidgetInput & { value: Ast.App | Ast.Ident | Ast.PropertyAccess | Ast.OprApp } => {
    const callInfo = input[CallInfo]
    // No need to check for AST type, since CallInfo depends on WidgetFunction being matched first.
    return callInfo != null && methodPointerEquals(callInfo.methodCall.methodPointer, methodPointer)
  }

export const widgetDefinition = defineWidget(
  WidgetInput.isFunctionCall,
  {
    priority: 200,
    score: (props, db) => {
      // If ArgumentApplicationKey is stored, we already are handled by some WidgetFunction.
      if (props.input[ArgumentApplicationKey]) return Score.Mismatch
      const ast = props.input.value
      if (ast.id == null) return Score.Mismatch
      const prevFunctionState = injectFunctionInfo(true)

      // It is possible to try to render the same function application twice, e.g. when detected an
      // application with no arguments applied yet, but the application target is also an infix call.
      // In that case, the reentrant call method info must be ignored to not create an infinite loop,
      // and to resolve the infix call as its own application.
      // We only render the function widget on the application chain’s top-level.
      if (prevFunctionState?.prefixCalls.has(ast.id)) return Score.Mismatch

      if (ast instanceof Ast.App || ast instanceof Ast.OprApp) return Score.Perfect

      const info = getMethodCallInfoRecursively(ast, db)
      return info != null ? Score.Perfect : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="innerInput" @update="handleArgUpdate" />
</template>
