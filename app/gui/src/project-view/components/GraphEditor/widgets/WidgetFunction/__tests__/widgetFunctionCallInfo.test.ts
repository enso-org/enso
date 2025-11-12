import { parseWithSpans } from '$/providers/openedProjects/graph/__tests__/graphDatabase.test'
import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import { type NodeVisualizationConfiguration } from '$/providers/openedProjects/project/executionContext'
import { mockProjectNameStore } from '$/providers/openedProjects/projectNames'
import { entryMethodPointer } from '$/providers/openedProjects/suggestionDatabase/entry'
import {
  makeArgument,
  makeConstructor,
  makeMethod,
  makeModuleMethod,
  makeStaticMethod,
} from '$/providers/openedProjects/suggestionDatabase/mockSuggestion'
import { WidgetInput } from '$/providers/openedProjects/widgetRegistry'
import {
  GET_WIDGETS_METHOD,
  WIDGETS_ENSO_MODULE,
} from '@/components/GraphEditor/widgets/WidgetFunction/consts'
import { useWidgetFunctionCallInfo } from '@/components/GraphEditor/widgets/WidgetFunction/widgetFunctionCallInfo'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { expect, test } from 'vitest'
import { ref, toValue, type WatchSource } from 'vue'
import { SourceRange } from 'ydoc-shared/util/data/text'

const projectNames = mockProjectNameStore('local', 'Project')

const moduleMethod = makeModuleMethod('local.Project.module_method', {
  returnType: 'Standard.Data.Text',
  args: [makeArgument('arg')],
  annotations: ['arg'],
  projectNames,
})
const con = makeConstructor('local.Project.Type.Con', {
  args: [makeArgument('arg')],
  annotations: ['arg'],
  projectNames,
})
const method = makeMethod('local.Project.Type.method', {
  returnType: 'Standard.Data.Text',
  args: [makeArgument('self'), makeArgument('arg')],
  annotations: ['arg'],
  projectNames,
})
const staticMethod = makeStaticMethod('local.Project.Type.static_method', {
  returnType: 'Standard.Data.Text',
  args: [makeArgument('arg')],
  annotations: ['arg'],
  projectNames,
})

test.each`
  code                                       | callSuggestion  | subjectSpan            | attachedSpan            | subjectType                  | methodName
  ${'val1.method val2'}                      | ${method}       | ${{ from: 0, to: 4 }}  | ${{ from: 0, to: 4 }}   | ${'local.Project.Type'}      | ${'.method'}
  ${'local.Project.Type.method val1 val2'}   | ${method}       | ${{ from: 0, to: 18 }} | ${{ from: 26, to: 30 }} | ${'local.Project.Type.type'} | ${'.method'}
  ${'Type.method val1'}                      | ${method}       | ${{ from: 0, to: 4 }}  | ${{ from: 12, to: 16 }} | ${'local.Project.Type.type'} | ${'.method'}
  ${'local.Project.Type.method'}             | ${method}       | ${{ from: 0, to: 18 }} | ${null}                 | ${'local.Project.Type.type'} | ${'.method'}
  ${'foo.method'}                            | ${method}       | ${{ from: 0, to: 3 }}  | ${null}                 | ${'local.Project.Type.type'} | ${'.method'}
  ${'foo.method'}                            | ${method}       | ${{ from: 0, to: 3 }}  | ${{ from: 0, to: 3 }}   | ${'local.Project.Type'}      | ${'.method'}
  ${'local.Project.Type.static_method val1'} | ${staticMethod} | ${{ from: 0, to: 18 }} | ${{ from: 0, to: 18 }}  | ${'local.Project.Type.type'} | ${'.static_method'}
  ${'Type.Con val1'}                         | ${con}          | ${{ from: 0, to: 4 }}  | ${{ from: 0, to: 4 }}   | ${'local.Project.Type.type'} | ${'.Con'}
  ${'..Con val1'}                            | ${con}          | ${null}                | ${null}                 | ${null}                      | ${'.Con'}
  ${'local.Project.module_method val1'}      | ${moduleMethod} | ${{ from: 0, to: 13 }} | ${{ from: 0, to: 13 }}  | ${'local.Project'}           | ${'.module_method'}
`(
  'Visualization config for $code',
  ({ code, callSuggestion, subjectSpan, attachedSpan, subjectType, methodName }) => {
    const spans = {
      entireFunction: SourceRange.fromStartAndLength(0, code.length),
      ...(subjectSpan != null ? { subject: subjectSpan } : {}),
      ...(attachedSpan != null ? { attached: attachedSpan } : {}),
    }
    const { ast, eid, id } = parseWithSpans(code, spans)
    const statement = ast.lines[0]?.statement?.node
    assert(statement instanceof Ast.ExpressionStatement)
    const node = statement.expression
    expect(node.externalId).toBe(eid('entireFunction'))

    let visConfig: WatchSource<Opt<NodeVisualizationConfiguration>> | undefined
    useWidgetFunctionCallInfo(
      WidgetInput.FromAst(node),
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
              typeInfo: TypeInfo.fromLsResponse([subjectType], [], projectNames)!,
              methodCall: undefined,
              payload: { type: 'Value' },
              evaluationId: 0,
              profilingInfo: [],
            }
          }
        },
      } as GraphDb,
      {
        useVisualizationData(config) {
          expect(visConfig, 'Only one visualization is expected').toBeUndefined()
          visConfig = config
          return ref(null)
        },
        moduleProjectPath: projectNames.parseProjectPathRaw('local.Project.Main'),
      },
      projectNames,
      [],
    )
    assert(visConfig != null)
    const visConfigValue = toValue<Opt<NodeVisualizationConfiguration>>(visConfig)
    assert(visConfigValue != null)
    if (typeof visConfigValue.expression === 'string') {
      expect(visConfigValue.expressionId).toBe(eid('entireFunction'))
      expect(visConfigValue.expression).toBe(
        `_ -> ${WIDGETS_ENSO_MODULE}.${GET_WIDGETS_METHOD} ${projectNames.printProjectPath(callSuggestion.memberOf)}`,
      )
      expect(eid('attached')).toBeUndefined()
    } else {
      expect(visConfigValue.expressionId).toBe(eid('attached'))
    }
    expect(visConfigValue.positionalArgumentsExpressions?.[0]).toBe(methodName)
    expect(visConfigValue.positionalArgumentsExpressions?.[1]).toBe("['arg']")
  },
)
