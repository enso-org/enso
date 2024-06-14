import { parseWithSpans } from '@/stores/graph/__tests__/graphDatabase.test'
import type { MethodCallInfo } from '@/stores/graph/graphDatabase'
import type { ExpressionInfo } from '@/stores/project/computedValueRegistry'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  entryMethodPointer,
  makeArgument,
  makeConstructor,
  makeMethod,
  makeStaticMethod,
  makeType,
} from '@/stores/suggestionDatabase/entry'
import { assert } from '@/util/assert'
import type { AstId } from 'shared/ast'
import { expect, test } from 'vitest'
import {
  GET_WIDGETS_METHOD,
  WIDGETS_ENSO_MODULE,
  useWidgetFunctionCallInfo,
} from '../widgetFunctionCallInfo'

const moduleMethod = {
  ...makeMethod('local.Project.module_method', 'Text'),
  arguments: [makeArgument('arg')],
  annotations: ['arg'],
}
const con = {
  ...makeConstructor('local.Project.Type.Con'),
  arguments: [makeArgument('arg')],
  annotations: ['arg'],
}
const method = {
  ...makeMethod('local.Project.Type.method', 'Text'),
  arguments: [makeArgument('self'), makeArgument('arg')],
  annotations: ['arg'],
}
const staticMethod = {
  ...makeStaticMethod('local.Project.Type.static_method', 'Text'),
  arguments: [makeArgument('arg')],
  annotations: ['arg'],
}

test.each`
  code                                       | callSuggestion  | subjectSpan | selfSpan    | subjectType                  | methodName
  ${'val1.method val2'}                      | ${method}       | ${[0, 4]}   | ${[0, 4]}   | ${'local.Project.Type'}      | ${'.method'}
  ${'local.Project.Type.method val1 val2'}   | ${method}       | ${[0, 18]}  | ${[26, 30]} | ${'local.Project.Type.type'} | ${'.method'}
  ${'Type.method val1'}                      | ${method}       | ${[0, 4]}   | ${[12, 16]} | ${'local.Project.Type.type'} | ${'.method'}
  ${'local.Project.Type.method'}             | ${method}       | ${[0, 18]}  | ${[0, 18]}  | ${'local.Project.Type.type'} | ${'.method'}
  ${'local.Project.Type.static_method val1'} | ${staticMethod} | ${[0, 18]}  | ${[0, 18]}  | ${'local.Project.Type.type'} | ${'.static_method'}
  ${'Type.Con val1'}                         | ${con}          | ${[0, 4]}   | ${[0, 4]}   | ${'local.Project.Type.type'} | ${'.Con'}
  ${'..Con val1'}                            | ${con}          | ${null}     | ${null}     | ${null}                      | ${'.Con'}
  ${'local.Project.module_method val1'}      | ${moduleMethod} | ${[0, 13]}  | ${[0, 13]}  | ${'local.Project'}           | ${'.module_method'}
`(
  'Visualization config for $code',
  ({ code, callSuggestion, subjectSpan, selfSpan, subjectType, methodName }) => {
    const spans = {
      entireFunction: [0, code.length] as [number, number],
      ...(subjectSpan != null ? { subject: subjectSpan as [number, number] } : {}),
      ...(selfSpan != null ? { self: selfSpan as [number, number] } : {}),
    }
    const { ast, eid, id } = parseWithSpans(code, spans)
    const line = ast.lines[0]?.expression
    assert(line != null)
    expect(line.node.externalId).toBe(eid('entireFunction'))

    const db = {
      getMethodCallInfo(astId: AstId): MethodCallInfo | undefined {
        if (astId === id('entireFunction')) {
          return {
            suggestion: callSuggestion,
            methodCall: {
              notAppliedArguments: [],
              methodPointer: entryMethodPointer(callSuggestion)!,
            },
          }
        }
      },
      getExpressionInfo(astId: AstId): ExpressionInfo | undefined {
        if (subjectSpan != null && astId === id('subject')) {
          return {
            typename: subjectType,
            methodCall: undefined,
            payload: { type: 'Value' },
            profilingInfo: [],
          }
        }
      },
    }

    const info = useWidgetFunctionCallInfo(line.node, db)
    const visConfig = info.visualizationConfig.value
    assert(visConfig != null)
    if (typeof visConfig.expression === 'string') {
      expect(visConfig.expressionId).toBe(eid('entireFunction'))
      expect(visConfig.expression).toBe(
        `a -> ${WIDGETS_ENSO_MODULE}.${GET_WIDGETS_METHOD} ${callSuggestion.definedIn}`,
      )
    } else {
      expect(info.visualizationConfig.value?.expressionId).toBe(eid('self'))
    }
    expect(info.visualizationConfig.value?.positionalArgumentsExpressions![0]).toBe(methodName)
    expect(info.visualizationConfig.value?.positionalArgumentsExpressions![1]).toBe("['arg']")
  },
)
