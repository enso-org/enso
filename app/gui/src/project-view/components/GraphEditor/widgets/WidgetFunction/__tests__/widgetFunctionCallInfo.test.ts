import { WidgetInput } from '@/providers/widgetRegistry'
import { parseWithSpans } from '@/stores/graph/__tests__/graphDatabase.test'
import type { NodeVisualizationConfiguration } from '@/stores/project/executionContext'
import {
  entryMethodPointer,
  makeArgument,
  makeConstructor,
  makeMethod,
  makeStaticMethod,
} from '@/stores/suggestionDatabase/entry'
import { assert } from '@/util/assert'
import { expect, test } from 'vitest'
import { ref, type Ref } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'
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
  code                                       | callSuggestion  | subjectSpan | attachedSpan | subjectType                  | methodName
  ${'val1.method val2'}                      | ${method}       | ${[0, 4]}   | ${[0, 4]}    | ${'local.Project.Type'}      | ${'.method'}
  ${'local.Project.Type.method val1 val2'}   | ${method}       | ${[0, 18]}  | ${[26, 30]}  | ${'local.Project.Type.type'} | ${'.method'}
  ${'Type.method val1'}                      | ${method}       | ${[0, 4]}   | ${[12, 16]}  | ${'local.Project.Type.type'} | ${'.method'}
  ${'local.Project.Type.method'}             | ${method}       | ${[0, 18]}  | ${null}      | ${'local.Project.Type.type'} | ${'.method'}
  ${'foo.method'}                            | ${method}       | ${[0, 3]}   | ${null}      | ${'local.Project.Type.type'} | ${'.method'}
  ${'foo.method'}                            | ${method}       | ${[0, 3]}   | ${[0, 3]}    | ${'local.Project.Type'}      | ${'.method'}
  ${'local.Project.Type.static_method val1'} | ${staticMethod} | ${[0, 18]}  | ${[0, 18]}   | ${'local.Project.Type.type'} | ${'.static_method'}
  ${'Type.Con val1'}                         | ${con}          | ${[0, 4]}   | ${[0, 4]}    | ${'local.Project.Type.type'} | ${'.Con'}
  ${'..Con val1'}                            | ${con}          | ${null}     | ${null}      | ${null}                      | ${'.Con'}
  ${'local.Project.module_method val1'}      | ${moduleMethod} | ${[0, 13]}  | ${[0, 13]}   | ${'local.Project'}           | ${'.module_method'}
`(
  'Visualization config for $code',
  ({ code, callSuggestion, subjectSpan, attachedSpan, subjectType, methodName }) => {
    const spans = {
      entireFunction: [0, code.length] as [number, number],
      ...(subjectSpan != null ? { subject: subjectSpan as [number, number] } : {}),
      ...(attachedSpan != null ? { attached: attachedSpan as [number, number] } : {}),
    }
    const { ast, eid, id } = parseWithSpans(code, spans)
    const line = ast.lines[0]?.expression
    assert(line != null)
    expect(line.node.externalId).toBe(eid('entireFunction'))

    let visConfig: Ref<Opt<NodeVisualizationConfiguration>> | undefined
    useWidgetFunctionCallInfo(
      WidgetInput.FromAst(line.node),
      {
        getMethodCallInfo(astId) {
          if (astId === id('entireFunction')) {
            return {
              suggestion: callSuggestion,
              methodCallSource: astId,
              methodCall: {
                notAppliedArguments: [],
                methodPointer: entryMethodPointer(callSuggestion)!,
              },
            }
          }
        },
        getExpressionInfo(astId) {
          if (subjectSpan != null && astId === id('subject')) {
            return {
              typename: subjectType,
              methodCall: undefined,
              payload: { type: 'Value' },
              profilingInfo: [],
            }
          }
        },
      },
      {
        useVisualizationData(config) {
          expect(visConfig, 'Only one visualizaiton is expected').toBeUndefined()
          visConfig = config
          return ref(null)
        },
      },
    )
    assert(visConfig != null)
    assert(visConfig.value != null)
    if (typeof visConfig.value.expression === 'string') {
      expect(visConfig.value.expressionId).toBe(eid('entireFunction'))
      expect(visConfig.value.expression).toBe(
        `_ -> ${WIDGETS_ENSO_MODULE}.${GET_WIDGETS_METHOD} ${callSuggestion.memberOf}`,
      )
      expect(eid('attached')).toBeUndefined()
    } else {
      expect(visConfig.value.expressionId).toBe(eid('attached'))
    }
    expect(visConfig.value.positionalArgumentsExpressions![0]).toBe(methodName)
    expect(visConfig.value.positionalArgumentsExpressions![1]).toBe("['arg']")
  },
)
