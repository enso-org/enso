import { createContextStore } from '@/providers'
import type { AstId } from '@/util/ast/abstract.ts'
import { identity } from '@vueuse/core'

interface FunctionInfo {
  callId: AstId | undefined
}

export { injectFn as injectFunctionInfo, provideFn as provideFunctionInfo }
const { provideFn, injectFn } = createContextStore('Function info', identity<FunctionInfo>)
