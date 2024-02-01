<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectFunctionInfo, provideFunctionInfo } from '@/providers/functionInfo'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
  type WidgetUpdate,
} from '@/providers/widgetRegistry'
import {
  argsWidgetConfigurationSchema,
  functionCallConfiguration,
} from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore, type NodeVisualizationConfiguration } from '@/stores/project'
import { assert, assertUnreachable } from '@/util/assert'
import { Ast } from '@/util/ast'
import {
  ArgumentApplication,
  ArgumentApplicationKey,
  ArgumentAst,
  ArgumentPlaceholder,
  getAccessOprSubject,
  interpretCall,
} from '@/util/callTree'
import { partitionPoint } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { isIdentifier } from '@/util/qualifiedName.ts'
import type { ExternalId } from 'shared/yjsModel.ts'
import { computed, proxyRefs } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const project = useProjectStore()

provideFunctionInfo(
  proxyRefs({
    callId: computed(() => props.input.value.id),
  }),
)

const methodCallInfo = computed(() => {
  return graph.db.getMethodCallInfo(props.input.value.id)
})

const interpreted = computed(() => {
  return interpretCall(props.input.value, methodCallInfo.value == null)
})

const subjectInfo = computed(() => {
  const analyzed = interpreted.value
  if (analyzed.kind !== 'prefix') return
  const subject = getAccessOprSubject(analyzed.func)
  if (!subject) return
  return graph.db.getExpressionInfo(subject.id)
})

const selfArgumentPreapplied = computed(() => {
  const info = methodCallInfo.value
  const funcType = info?.methodCall.methodPointer.definedOnType
  return funcType != null && subjectInfo.value?.typename !== `${funcType}.type`
})

const subjectTypeMatchesMethod = computed(() => {
  const funcType = methodCallInfo.value?.methodCall.methodPointer.definedOnType
  return funcType != null && subjectInfo.value?.typename === `${funcType}.type`
})

const application = computed(() => {
  const call = interpreted.value
  if (!call) return null
  const noArgsCall = call.kind === 'prefix' ? graph.db.getMethodCall(call.func.id) : undefined

  return ArgumentApplication.FromInterpretedWithInfo(call, {
    suggestion: methodCallInfo.value?.suggestion,
    widgetCfg: widgetConfiguration.value,
    subjectAsSelf: selfArgumentPreapplied.value,
    notAppliedArguments:
      noArgsCall != null &&
      (!subjectTypeMatchesMethod.value || noArgsCall.notAppliedArguments.length > 0)
        ? noArgsCall.notAppliedArguments
        : undefined,
  })
})

const innerInput = computed(() => {
  if (application.value instanceof ArgumentApplication) {
    return application.value.toWidgetInput()
  } else {
    return props.input
  }
})

const escapeString = (str: string): string => {
  const escaped = str.replaceAll(/([\\'])/g, '\\$1')
  return `'${escaped}'`
}
const makeArgsList = (args: string[]) => '[' + args.map(escapeString).join(', ') + ']'

const selfArgumentExternalId = computed<Opt<ExternalId>>(() => {
  const analyzed = interpretCall(props.input.value, true)
  if (analyzed.kind === 'infix') {
    return analyzed.lhs?.externalId
  } else {
    const knownArguments = methodCallInfo.value?.suggestion?.arguments
    const hasSelfArgument = knownArguments?.[0]?.name === 'self'
    const selfArgument =
      hasSelfArgument && !selfArgumentPreapplied.value
        ? analyzed.args.find((a) => a.argName === 'self' || a.argName == null)?.argument
        : getAccessOprSubject(analyzed.func) ?? analyzed.args[0]?.argument

    return selfArgument?.externalId
  }
})

const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
  // If we inherit dynamic config, there is no point in attaching visualization.
  if (props.input.dynamicConfig) return null

  const expressionId = selfArgumentExternalId.value
  const astId = props.input.value.id
  if (astId == null || expressionId == null) return null
  const info = graph.db.getMethodCallInfo(astId)
  if (!info) return null
  const args = info.suggestion.annotations
  if (args.length === 0) return null
  const name = info.suggestion.name
  return {
    expressionId,
    visualizationModule: 'Standard.Visualization.Widgets',
    expression: {
      module: 'Standard.Visualization.Widgets',
      definedOnType: 'Standard.Visualization.Widgets',
      name: 'get_widget_json',
    },
    positionalArgumentsExpressions: [`.${name}`, makeArgsList(args)],
  }
})

const visualizationData = project.useVisualizationData(visualizationConfig)
const widgetConfiguration = computed(() => {
  if (props.input.dynamicConfig?.kind === 'FunctionCall') return props.input.dynamicConfig
  const data = visualizationData.value
  if (data?.ok) {
    const parseResult = argsWidgetConfigurationSchema.safeParse(data.value)
    if (parseResult.success) {
      return functionCallConfiguration(parseResult.data)
    } else {
      console.error('Unable to parse widget configuration.', data, parseResult.error)
    }
  } else if (data != null && !data.ok) {
    data.error.log('Cannot load dynamic configuration')
  }
  return undefined
})

/**
 * Process an argument value update. Takes care of inserting assigned placeholder values, as well as
 * handling deletions of arguments and rewriting the applications to named as appropriate.
 */
function handleArgUpdate(update: WidgetUpdate): boolean {
  const app = application.value
  if (update.portUpdate && app instanceof ArgumentApplication) {
    const {
      edit,
      portUpdate: { value, origin },
    } = update
    // Find the updated argument by matching origin port/expression with the appropriate argument.
    // We are interested only in updates at the top level of the argument AST. Updates from nested
    // widgets do not need to be processed at the function application level.
    const argApp = [...app.iterApplications()].find(
      (app) => 'portId' in app.argument && app.argument.portId === origin,
    )

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
        argApp.argument.insertAsNamed && isIdentifier(argApp.argument.argInfo.name)
          ? argApp.argument.argInfo.name
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
      if (deletedArgIdx != null) {
        const notAppliedArguments = methodCallInfo.value?.methodCall.notAppliedArguments
        if (notAppliedArguments != null) {
          const insertAt = partitionPoint(notAppliedArguments, (i) => i < deletedArgIdx)
          // Insert the deleted argument back to the method info. This directly modifies observable
          // data in `ComputedValueRegistry`. That's on purpose.
          notAppliedArguments.splice(insertAt, 0, deletedArgIdx)
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
      } else if (value == null && argApp.appTree instanceof Ast.OprApp) {
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
        for (let innerApp of [...app.iterApplications()]) {
          if (innerApp.appTree.id === argApp.appTree.id) {
            // Found the application with the argument to remove. Skip the argument and use the
            // application target's code. This is the final iteration of the loop.
            const appTree = edit.getVersion(argApp.appTree)
            appTree.replace(appTree.function.take())
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
export const widgetDefinition = defineWidget(WidgetInput.isFunctionCall, {
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
    if (prevFunctionState?.callId === ast.id) return Score.Mismatch

    if (ast instanceof Ast.App || ast instanceof Ast.OprApp) return Score.Perfect

    const info = db.getMethodCallInfo(ast.id)
    if (prevFunctionState != null && info?.partiallyApplied === true && ast instanceof Ast.Ident) {
      return Score.Mismatch
    }
    return info != null ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <NodeWidget :input="innerInput" @update="handleArgUpdate" />
</template>
