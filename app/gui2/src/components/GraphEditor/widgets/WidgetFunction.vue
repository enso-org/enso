<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Tree } from '@/generated/ast'
import { injectFunctionInfo, provideFunctionInfo } from '@/providers/functionInfo'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { AstExtended } from '@/util/ast'
import { ArgumentApplication } from '@/util/callTree'
import { computed, proxyRefs } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()

provideFunctionInfo(
  proxyRefs({
    callId: computed(() => props.input.astId),
  }),
)

const application = computed(() => {
  const astId = props.input.astId
  if (astId == null) return props.input
  const info = graph.db.getMethodCallInfo(astId)
  const interpreted = ArgumentApplication.Interpret(props.input, info == null)

  const noArgsCall =
    interpreted.kind === 'prefix' ? graph.db.getMethodCall(interpreted.func.astId) : undefined

  return ArgumentApplication.FromInterpretedWithInfo(
    interpreted,
    noArgsCall,
    info?.methodCall,
    info?.suggestion,
    !info?.staticallyApplied,
  )
})
</script>
<script lang="ts">
export const widgetDefinition = defineWidget(
  AstExtended.isTree([Tree.Type.App, Tree.Type.NamedApp, Tree.Type.Ident, Tree.Type.OprApp]),
  {
    priority: -10,
    score: (props, db) => {
      const ast = props.input
      if (ast.astId == null) return Score.Mismatch
      const prevFunctionState = injectFunctionInfo(true)

      // It is possible to try to render the same function application twice, e.g. when detected an
      // application with no arguments applied yet, but the application target is also an infix call.
      // In that case, the reentrant call method info must be ignored to not create an infinite loop,
      // and to resolve the infix call as its own application.
      if (prevFunctionState?.callId === ast.astId) return Score.Mismatch

      if (ast.isTree([Tree.Type.App, Tree.Type.NamedApp, Tree.Type.OprApp])) return Score.Perfect

      const info = db.getMethodCallInfo(ast.astId)
      if (
        prevFunctionState != null &&
        info?.staticallyApplied === true &&
        props.input.isTree(Tree.Type.Ident)
      ) {
        return Score.Mismatch
      }
      return info != null ? Score.Perfect : Score.Mismatch
    },
  },
)
</script>

<template>
  <NodeWidget :input="application" />
</template>
