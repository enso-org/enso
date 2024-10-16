import { createContextStore } from '@/providers'
import type { MethodCallInfo } from '@/stores/graph/graphDatabase'
import type { AstId } from '@/util/ast/abstract.ts'
import { identity } from '@vueuse/core'

interface FunctionInfo {
  /** Ids of all nested prefix applications inside top-level expression (including the top-level). */
  prefixCalls: Set<AstId>
  callInfo: MethodCallInfo | undefined
  outputType: string | undefined
}

export { injectFn as injectFunctionInfo, provideFn as provideFunctionInfo }
const { provideFn, injectFn } = createContextStore('Function info', identity<FunctionInfo>)
