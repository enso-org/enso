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
import { Filtering } from '../filtering'

test.each([
  { ...makeModuleMethod('Standard.Base.Data', 'read', 'Any'), groupIndex: 0 },
  { ...makeModuleMethod('Standard.Base.Data', 'write', 'Any'), groupIndex: 0 },
  { ...makeStaticMethod('Standard.Base.Data.Vector.Vector', 'new', 'Any'), groupIndex: 1 },
  makeModule('local.New_Project'),
  makeModule('Standard.Base.Data'),
])('$name entry is in the CB main view', (entry) => {
  const filtering = new Filtering({})
  expect(filtering.filter(entry)).not.toBeNull()
})

test.each([
  makeModuleMethod('Standard.Base.Data', 'convert', 'Any'), // not in group
  { ...makeMethod('Standard.Base.Data.Vector.Vector', 'get', 'Any'), groupIndex: 1 }, // not static method
  makeModule('Standard.Base.Data.Vector'), // Not top module
])('$name entry is not in the CB main view', (entry) => {
  const filtering = new Filtering({})
  expect(filtering.filter(entry)).toBeNull()
})

test.each([
  makeModuleMethod('local.Project.Module', 'module_method', 'Any'),
  makeType('local.Project.Module', 'Type'),
  makeCon('local.Project.Module.Type', 'Con'),
  makeStaticMethod('local.Project.Module.Type', 'method', 'Any'),
  makeModule('local.Project.Module.Submodule'),
  makeModuleMethod(
    'another.Project.Local.Project.Module',
    'module_method_with_matching_suffix',
    'Any',
  ),
])('$name entry is in the local.Project.Module content', (entry) => {
  const filtering = new Filtering({ qualifiedNamePattern: 'local.Project.Module' })
  expect(filtering.filter(entry)).not.toBeNull()
  const substringFiltering = new Filtering({ qualifiedNamePattern: 'local.Proj.Mod' })
  expect(substringFiltering.filter(entry)).not.toBeNull()
})

test.each([
  makeModuleMethod('local.Project.Another_Module', 'another_module_method', 'Any'),
  makeModuleMethod(
    'local.Project.Another_Module.Module',
    'another_module_with_same_name_method',
    'Any',
  ),
  makeModule('local.Project.Module'),
  makeModule('local.Project.Module.Submodule.Nested'),
  makeType('local.Project', 'In_Parent_Module'),
  makeType('local.Project.Module.Submodule', 'In_Submodule'),
])('$name entry is not in the local.Project.Module content', (entry) => {
  const filtering = new Filtering({ qualifiedNamePattern: 'local.Project.Module' })
  expect(filtering.filter(entry)).toBeNull()
  const substringFiltering = new Filtering({ qualifiedNamePattern: 'local.Proj.Mod' })
  expect(substringFiltering.filter(entry)).toBeNull()
})

test.each([
  makeModuleMethod('local.Project.Module', 'foo', 'Any'),
  makeModuleMethod('local.Project.Module.Submodule', 'foo_in_submodule', 'Any'),
  makeModuleMethod('local.Project.Module.Submodule.Nested', 'foo_nested', 'Any'),
  makeModule('local.Project.Module.Foo_Direct_Submodule'),
  makeModule('local.Project.Module.Submodule.Foo_Nested'),
  makeModuleMethod('another.Project.Local.Project.Module', 'foo_with_matching_suffix', 'Any'),
])(
  "$name entry is in the local.Project.Module content when filtering by pattern 'foo'",
  (entry) => {
    const filtering = new Filtering({
      pattern: 'foo',
      qualifiedNamePattern: 'local.Project.Module',
    })
    expect(filtering.filter(entry)).not.toBeNull()
  },
)

test.each([
  makeModuleMethod('local.Project.Module', 'bar', 'Any'),
  makeModuleMethod('local.Project.Another_Module', 'foo_in_another_module', 'Any'),
  makeModuleMethod(
    'local.Project.Another_Module.Module',
    'foo_in_another_module_with_same_name',
    'Any',
  ),
  makeModuleMethod('local.Project', 'foo_in_parent_module', 'Any'),
])(
  "$name entry is in not the local.Project.Module content when filtering by pattern 'foo'",
  (entry) => {
    const filtering = new Filtering({
      pattern: 'foo',
      qualifiedNamePattern: 'local.Project.Module',
    })
    expect(filtering.filter(entry)).toBeNull()
  },
)

test('An Instance method is shown when self type matches', () => {
  const entry = makeMethod('Standard.Base.Data.Vector.Vector', 'get', 'Any')
  const filteringWithSelfType = new Filtering({ selfType: 'Standard.Base.Data.Vector.Vector' })
  expect(filteringWithSelfType.filter(entry)).not.toBeNull()
  const filteringWithoutSelfType = new Filtering({ pattern: 'get' })
  expect(filteringWithoutSelfType.filter(entry)).toBeNull()
})

test.each([
  makeModule('Standard.Base.Data.Vector'),
  makeStaticMethod('Standard.Base.Data.Vector.Vector', 'new', 'Any'),
  makeCon('Standard.Base.Data.Vector.Vector', 'Vector_Con'),
  makeLocal('Standard.Base.Data.Vector', 'get', 'Any'),
  makeFunction('Standard.Base.Data.Vector', 'func', 'Any'),
  makeMethod('Standard.Base.Data.Vector.Vecto', 'get', 'Any'),
  makeMethod('Standard.Base.Data.Vector.Vector2', 'get', 'Any'),
])('$name is filtered out when Vector self type is specified', (entry) => {
  const filtering = new Filtering({ selfType: 'Standard.Base.Data.Vector.Vector' })
  expect(filtering.filter(entry)).toBeNull()
})

test.each(['bar', 'barfoo', 'fo', 'bar_fo_bar'])("%s is not matched by pattern 'foo'", (name) => {
  const pattern = 'foo'
  const entry = makeModuleMethod('local.Project', name, 'Any')
  const filtering = new Filtering({ pattern })
  expect(filtering.filter(entry)).toBeNull()
})

test('Matching pattern without underscores', () => {
  const pattern = 'foo'
  const filtering = new Filtering({ pattern })
  const matchedSorted = [
    { name: 'foo' }, // exact match
    { name: 'foobar' }, // name start match
    { name: 'bar', aliases: ['baz', 'foo'] }, // exact alias match
    { name: 'bar', aliases: ['bazbar', 'foobar'] }, // alias start match
    { name: 'bar_foo' }, // name word exact match
    { name: 'baz_foobar' }, // name word start match
    { name: 'bar', aliases: ['bar_foo'] }, // alias word exact match
    { name: 'bar', aliases: ['baz_foobar'] }, // alias word start match
    { name: 'frequent_objective_objections' }, // initials match
    { name: 'bar', aliases: ['frequent_objective_objections'] }, // alias initials match
  ]
  const matchResults = Array.from(matchedSorted, ({ name, aliases }) => {
    const entry = { ...makeModuleMethod('local.Project', name, 'Any'), aliases: aliases ?? [] }
    return filtering.filter(entry)
  })
  expect(matchResults[0]).not.toBeNull()
  for (let i = 1; i < matchResults.length; i++) {
    expect(matchResults[i]).not.toBeNull()
    expect(matchResults[i]?.score).toBeGreaterThan(matchResults[i - 1]?.score ?? Infinity)
  }
})

test('Matching pattern with underscores', () => {
  const pattern = 'foo_bar'
  const filtering = new Filtering({ pattern })
  const matchedSorted = [
    { name: 'foo_bar' }, // exact match
    { name: 'foo_xyz_barabc' }, // first word exact match
    { name: 'fooabc_barabc' }, // first word match
    { name: 'bar', aliases: ['foo_bar', 'foo'] }, // exact alias match
    { name: 'bar', aliases: ['foo', 'foo_xyz_barabc'] }, // alias first word exact match
    { name: 'bar', aliases: ['foo', 'fooabc_barabc'] }, // alias first word match
    { name: 'xyz_foo_abc_bar_xyz' }, // exact word match
    { name: 'xyz_fooabc_abc_barabc_xyz' }, // non-exact word match
    { name: 'bar', aliases: ['xyz_foo_abc_bar_xyz'] }, // alias word exact match
    { name: 'bar', aliases: ['xyz_fooabc_abc_barabc_xyz'] }, // alias word start match
  ]
  const matchResults = Array.from(matchedSorted, ({ name, aliases }) => {
    const entry = { ...makeModuleMethod('local.Project', name, 'Any'), aliases: aliases ?? [] }
    return filtering.filter(entry)
  })
  expect(matchResults[0]).not.toBeNull()
  for (let i = 1; i < matchResults.length; i++) {
    expect(matchResults[i]).not.toBeNull()
    expect(matchResults[i]?.score).toBeGreaterThan(matchResults[i - 1]?.score ?? Infinity)
  }
})
