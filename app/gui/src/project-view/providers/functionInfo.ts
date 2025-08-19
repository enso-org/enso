import { createContextStore } from '@/providers'
import type { MethodCallInfo } from '@/stores/graph/graphDatabase'
import { ExpressionInfo } from '@/stores/project/computedValueRegistry'
import type { AstId } from '@/util/ast/abstract.ts'
import { ProjectPath } from '@/util/projectPath'
import { identity } from '@vueuse/core'

interface FunctionInfo {
  /** Ids of all nested prefix applications inside top-level expression (including the top-level). */
  prefixCalls: Set<AstId>
  callInfo: MethodCallInfo | undefined
  outputType: ProjectPath | undefined
  subjectInfo: ExpressionInfo | undefined
}

export const [provideFunctionInfo, injectFunctionInfo] = createContextStore(
  'Function info',
  identity<FunctionInfo>,
)
