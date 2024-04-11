import { createContextStore } from '@/providers'
import type { AstId } from '@/util/ast/abstract.ts'
import { identity } from '@vueuse/core'

interface FunctionInfo {
  /** Id of top-level application expression. */
  callId: AstId | undefined
  /** Ids of all nested prefix applications inside top-level expression. */
  prefixCalls: AstId[]
}

export { injectFn as injectFunctionInfo, provideFn as provideFunctionInfo }
const { provideFn, injectFn } = createContextStore('Function info', identity<FunctionInfo>)
