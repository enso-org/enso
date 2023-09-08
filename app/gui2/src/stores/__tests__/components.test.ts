import { expect, test } from 'vitest'

import {
  makeCon,
  makeMethod,
  makeModule,
  makeModuleMethod,
  makeStaticMethod,
} from '@/stores/suggestionDatabase/entry'
import { labelOfEntry } from '../components'
import { Filtering } from '../components/filtering'

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
  { suggestion: makeModule('Standard.Table.Data'), expected: 'Standard.Table.Data' },
  { suggestion: makeModuleMethod('local.Project', 'main', 'Any'), expected: 'Project.main' },
])("$suggestion.name Component's label in the main view", ({ suggestion, expected }) => {
  expect(labelOfEntry(suggestion, new Filtering({}))).toBe(expected)
})
