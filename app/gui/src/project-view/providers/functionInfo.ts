import { createContextStore } from '@/providers'
import type { MethodCallInfo } from '@/stores/graph/graphDatabase'
import { ExpressionInfo } from '@/stores/project/computedValueRegistry'
import { Ast } from '@/util/ast'
import { ProjectPath } from '@/util/projectPath'
import { identity } from '@vueuse/core'

interface FunctionInfo {
  /** Ids of all nested prefix applications inside top-level expression (including the top-level). */
  prefixCalls: Set<Ast.AstId>
  callInfo: MethodCallInfo | undefined
  outputType: ProjectPath | undefined
  subject: Ast.Ast | undefined
  subjectInfo: ExpressionInfo | undefined
}

export const [provideFunctionInfo, injectFunctionInfo] = createContextStore(
  'Function info',
  identity<FunctionInfo>,
)
