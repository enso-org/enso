import { GraphDb } from '@/stores/graph/graphDatabase'
import { AstExtended } from '@/util/ast'
import { ArgumentPlaceholder } from '@/util/callTree'
import { IdMap } from 'shared/yjsModel'
import { describe, expect, test } from 'vitest'
import { defineComponent } from 'vue'
import {
  Score,
  WidgetRegistry,
  type WidgetDefinition,
  type WidgetInput,
  type WidgetModule,
} from '../widgetRegistry'

describe('WidgetRegistry', () => {
  function makeMockWidget<T extends WidgetInput>(
    name: string,
    widgetDefinition: WidgetDefinition<T>,
  ): WidgetModule<T> {
    return {
      default: defineComponent({ name }),
      widgetDefinition,
    }
  }

  const widgetA = makeMockWidget('A', {
    priority: 1,
    match: AstExtended.isInstance,
    score: () => Score.Perfect,
  })

  const widgetB = makeMockWidget('B', {
    priority: 2,
    match: ArgumentPlaceholder.isInstance,
    score: () => Score.Perfect,
  })

  const widgetC = makeMockWidget('C', {
    priority: 10,
    match: (input): input is WidgetInput => true,
    score: () => Score.Good,
  })

  const widgetD = makeMockWidget('D', {
    priority: 20,
    match: AstExtended.isInstance,
    score: (info) => (info.input.repr() === '_' ? Score.Perfect : Score.Mismatch),
  })

  const someAst = AstExtended.parse('foo', IdMap.Mock())
  const blankAst = AstExtended.parse('_', IdMap.Mock())
  const somePlaceholder = new ArgumentPlaceholder(0, {
    name: 'foo',
    type: 'Any',
    isSuspended: false,
    hasDefault: false,
  })

  const mockGraphDb = GraphDb.Mock()
  const registry = new WidgetRegistry(mockGraphDb)
  registry.registerWidgetModule(widgetA)
  registry.registerWidgetModule(widgetB)
  registry.registerWidgetModule(widgetC)
  registry.registerWidgetModule(widgetD)

  test('selects a widget based on the input type', () => {
    const forAst = registry.select({ input: someAst, config: undefined, nesting: 0 })
    const forArg = registry.select({ input: somePlaceholder, config: undefined, nesting: 0 })
    expect(forAst).toStrictEqual(widgetA.default)
    expect(forArg).toStrictEqual(widgetB.default)
  })

  test('selects a widget outside of the excluded set', () => {
    const forAst = registry.select(
      { input: someAst, config: undefined, nesting: 0 },
      new Set([widgetA.default]),
    )
    const forArg = registry.select(
      { input: somePlaceholder, config: undefined, nesting: 0 },
      new Set([widgetB.default]),
    )
    expect(forAst).toStrictEqual(widgetC.default)
    expect(forArg).toStrictEqual(widgetC.default)
  })

  test('returns undefined when all options are exhausted', () => {
    const selected = registry.select(
      { input: someAst, config: undefined, nesting: 0 },
      new Set([widgetA.default, widgetC.default]),
    )
    expect(selected).to.be.undefined
  })

  test('prefers low priority perfect over good high priority', () => {
    const selectedFirst = registry.select(
      { input: blankAst, config: undefined, nesting: 0 },
      new Set([widgetA.default]),
    )
    const selectedNext = registry.select(
      { input: blankAst, config: undefined, nesting: 0 },
      new Set([widgetA.default, widgetD.default]),
    )
    expect(selectedFirst).toStrictEqual(widgetD.default)
    expect(selectedNext).toStrictEqual(widgetC.default)
  })
})
