import { createContextStore } from '@/providers'
import type { AstId } from '@/util/ast/abstract.ts'
import { identity } from '@vueuse/core'

interface FunctionInfo {
  /** Ids of all nested prefix applications inside top-level expression (including the top-level). */
  prefixCalls: Set<AstId>
}

export { injectFn as injectFunctionInfo, provideFn as provideFunctionInfo }
const { provideFn, injectFn } = createContextStore('Function info', identity<FunctionInfo>)
