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
import type { Opt } from '@/util/data/opt'
import type { ExprId } from 'shared/yjsModel'
import { computed, proxyRefs } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const project = useProjectStore()

provideFunctionInfo(
  proxyRefs({
    callId: computed(() => props.input.value.exprId),
  }),
)

const methodCallInfo = computed(() => {
  return graph.db.getMethodCallInfo(props.input.value.exprId)
})

const interpreted = computed(() => {
  return interpretCall(props.input.value, methodCallInfo.value == null)
})

const application = computed(() => {
  const call = interpreted.value
  if (!call) return null
  const noArgsCall = call.kind === 'prefix' ? graph.db.getMethodCall(call.func.exprId) : undefined

  const info = methodCallInfo.value
  return ArgumentApplication.FromInterpretedWithInfo(
    call,
    {
      noArgsCall,
      appMethodCall: info?.methodCall,
      suggestion: info?.suggestion,
      widgetCfg: widgetConfiguration.value,
    },
    !info?.staticallyApplied,
  )
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

const selfArgumentAstId = computed<Opt<ExprId>>(() => {
  const analyzed = interpretCall(props.input.value, true)
  if (analyzed.kind === 'infix') {
    return analyzed.lhs?.exprId
  } else {
    const knownArguments = methodCallInfo.value?.suggestion?.arguments
    const selfArgument =
      knownArguments?.[0]?.name === 'self'
        ? getAccessOprSubject(analyzed.func)
        : analyzed.args[0]?.argument
    return selfArgument?.exprId
  }
})

const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
  // If we inherit dynamic config, there is no point in attaching visualization.
  if (props.input.dynamicConfig) return null

  const expressionId = selfArgumentAstId.value
  const astId = props.input.value.exprId
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
      let newArg: Ast.Owned<Ast.Ast>
      if (value instanceof Ast.Ast) {
        newArg = value
      } else {
        newArg = Ast.parse(value, edit)
      }
      const name = argApp.argument.insertAsNamed ? argApp.argument.argInfo.name : null
      edit.takeAndReplaceValue(app.appTree.exprId, (oldAppTree) =>
        Ast.App.new(oldAppTree, name, newArg, edit),
      )
      props.onUpdate({ edit })
      return true
    } else if (value == null && argApp?.argument instanceof ArgumentAst) {
      /* Case: Removing existing argument. */

      if (argApp.appTree instanceof Ast.App && argApp.appTree.argumentName != null) {
        /* Case: Removing named prefix argument. */

        // Named argument can always be removed immediately. Replace the whole application with its
        // target, effectively removing the argument from the call.
        const func = edit.take(argApp.appTree.function.exprId)
        assert(func != null)
        props.onUpdate({
          edit,
          portUpdate: {
            value: func.node,
            origin: argApp.appTree.exprId,
          },
        })
        return true
      } else if (value == null && argApp.appTree instanceof Ast.OprApp) {
        /* Case: Removing infix application. */

        // Infix application is removed as a whole. Only the target is kept.
        if (argApp.appTree.lhs) {
          const lhs = edit.take(argApp.appTree.lhs.exprId)
          assert(lhs != null)
          props.onUpdate({
            edit,
            portUpdate: {
              value: lhs.node,
              origin: argApp.appTree.exprId,
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
          if (innerApp.appTree.exprId === argApp.appTree.exprId) {
            // Found the application with the argument to remove. Skip the argument and use the
            // application target's code. This is the final iteration of the loop.
            const newFunction = edit.take(argApp.appTree.function.exprId)?.node
            assert(newFunction != undefined)
            edit.replaceRef(argApp.appTree.exprId, newFunction)
            props.onUpdate({ edit })
            return true
          } else {
            // Process an argument to the right of the removed argument.
            assert(innerApp.appTree instanceof Ast.App)
            const infoName = innerApp.argument.argInfo?.name ?? null
            // Positional arguments following the deleted argument must all be rewritten to named.
            if (infoName && !innerApp.appTree.argumentName) {
              const func = edit.take(innerApp.appTree.function.exprId)?.node
              const arg = edit.take(innerApp.appTree.argument.exprId)?.node
              assert(!!func)
              assert(!!arg)
              edit.replaceValue(innerApp.appTree.exprId, Ast.App.new(func, infoName, arg, edit))
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
  priority: -10,
  score: (props, db) => {
    // If ArgumentApplicationKey is stored, we already are handled by some WidgetFunction.
    if (props.input[ArgumentApplicationKey]) return Score.Mismatch
    const ast = props.input.value
    if (ast.exprId == null) return Score.Mismatch
    const prevFunctionState = injectFunctionInfo(true)

    // It is possible to try to render the same function application twice, e.g. when detected an
    // application with no arguments applied yet, but the application target is also an infix call.
    // In that case, the reentrant call method info must be ignored to not create an infinite loop,
    // and to resolve the infix call as its own application.
    if (prevFunctionState?.callId === ast.exprId) return Score.Mismatch

    if (ast instanceof Ast.App || ast instanceof Ast.OprApp) return Score.Perfect

    const info = db.getMethodCallInfo(ast.exprId)
    if (prevFunctionState != null && info?.staticallyApplied === true && ast instanceof Ast.Ident) {
      return Score.Mismatch
    }
    return info != null ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <NodeWidget :input="innerInput" @update="handleArgUpdate" />
</template>
