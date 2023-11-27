import { identity } from '@vueuse/core'
import type { ExprId } from 'shared/yjsModel'
import { createContextStore } from '.'

interface FunctionInfo {
  callId: ExprId | undefined
}

export { injectFn as injectFunctionInfo, provideFn as provideFunctionInfo }
const { provideFn, injectFn } = createContextStore('Function info', identity<FunctionInfo>)
