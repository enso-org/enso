import { expect, test } from 'vitest'

import {
  makeCon,
  makeFunction,
  makeLocal,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
  makeType,
} from '@/stores/suggestionDatabase/entry'
import { labelOfEntry } from '..'
import { Filtering } from '../filtering'

test.each([
  { suggestion: makeModuleMethod('Standard.Base.Data', 'read', 'Any'), expected: 'Data.read' },
  {
    suggestion: makeStaticMethod('Standard.Base.Data.Vector', 'new', 'Any'),
    expected: 'Vector.new',
  },
  { suggestion: makeMethod('Standard.Base.Data.Vector', 'get', 'Any'), expected: 'get' },
  {
    suggestion: makeCon('Standard.Table.Data.Join_Kind.Join_Kind', 'LeftInner'),
    expected: 'Join_Kind.LeftInner',
  },
  {
    suggestion: makeModule('Standard.Table.Data.Join_Kind'),
    expected: 'Join_Kind',
  },
  {
    suggestion: makeModule('Standard.Table.Data'),
    mainExpected: 'Standard.Table.Data',
    expected: 'Data',
  },
  { suggestion: makeModuleMethod('local.Project', 'main', 'Any'), expected: 'Project.main' },
])(
  "$suggestion.name Component's label is $expected",
  ({ suggestion, expected, mainExpected: mainExpected }) => {
    const mainView = new Filtering({})
    const filteredView = new Filtering({ pattern: 'e' })
    expect(labelOfEntry(suggestion, filteredView)).toBe(expected)
    expect(labelOfEntry(suggestion, mainView)).toBe(mainExpected ?? expected)
  },
)
