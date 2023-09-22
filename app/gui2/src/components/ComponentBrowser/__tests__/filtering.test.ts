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
import type { QualifiedName } from '@/util/qualifiedName'
import { Filtering } from '../filtering'

test.each([
  { ...makeModuleMethod('Standard.Base.Data.read'), groupIndex: 0 },
  { ...makeModuleMethod('Standard.Base.Data.write'), groupIndex: 0 },
  { ...makeStaticMethod('Standard.Base.Data.Vector.Vector.new'), groupIndex: 1 },
  makeModule('local.New_Project'),
  makeModule('Standard.Base.Data'),
])('$name entry is in the CB main view', (entry) => {
  const filtering = new Filtering({})
  expect(filtering.filter(entry)).not.toBeNull()
})

test.each([
  makeModuleMethod('Standard.Base.Data.convert'), // not in group
  { ...makeMethod('Standard.Base.Data.Vector.Vector.get'), groupIndex: 1 }, // not static method
  makeModule('Standard.Base.Data.Vector'), // Not top module
])('$name entry is not in the CB main view', (entry) => {
  const filtering = new Filtering({})
  expect(filtering.filter(entry)).toBeNull()
})

test.each([
  makeModuleMethod('local.Project.Module.module_method'),
  makeType('local.Project.Module.Type'),
  makeCon('local.Project.Module.Type.Con'),
  makeStaticMethod('local.Project.Module.Type.method'),
  makeModule('local.Project.Module.Submodule'),
  makeModuleMethod('another.Project.Local.Project.Module.module_method_with_matching_suffix'),
])('$name entry is in the local.Project.Module content', (entry) => {
  const filtering = new Filtering({ qualifiedNamePattern: 'local.Project.Module' })
  const substringFiltering = new Filtering({ qualifiedNamePattern: 'local.Proj.Mod' })
  expect(filtering.filter(entry)).not.toBeNull()
  expect(substringFiltering.filter(entry)).not.toBeNull()
})

test.each([
  makeModuleMethod('local.Project.Another_Module.another_module_method'),
  makeModuleMethod('local.Project.Another_Module.Module.another_module_with_same_name_method'),
  makeModuleMethod('local.Project.Module.Submodule.submodules_method'),
  makeModule('local.Project.Module'),
  makeModule('local.Project.Module.Submodule.Nested'),
  makeType('local.Project.In_Parent_Module'),
  makeType('local.Project.Module.Submodule.In_Submodule'),
])('$name entry is not in the local.Project.Module content', (entry) => {
  const filtering = new Filtering({ qualifiedNamePattern: 'local.Project.Module' })
  const substringFiltering = new Filtering({ qualifiedNamePattern: 'local.Proj.Mod' })
  expect(filtering.filter(entry)).toBeNull()
  expect(substringFiltering.filter(entry)).toBeNull()
})

test.each([
  makeModuleMethod('local.Project.Module.foo'),
  makeModuleMethod('local.Project.Module.Submodule.foo_in_submodule'),
  makeModuleMethod('local.Project.Module.Submodule.Nested.foo_nested'),
  makeType('local.Project.Module.Submodule.Nested.Foo_Type'),
  makeCon('local.Project.Module.Submodule.Nested.Foo_Type.Foo_Con'),
  makeStaticMethod('local.Project.Module.Submodule.Nested.Foo_Type.foo_method'),
  makeModule('local.Project.Module.Foo_Direct_Submodule'),
  makeModule('local.Project.Module.Submodule.Foo_Nested'),
  makeModuleMethod('another.Project.Local.Project.Module.foo_with_matching_suffix'),
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
  makeModuleMethod('local.Project.Module.bar'),
  makeModuleMethod('local.Project.Another_Module.foo_in_another_module'),
  makeModuleMethod('local.Project.Another_Module.Module.foo_in_another_module_with_same_name'),
  makeModuleMethod('local.Project.foo_in_parent_module'),
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

test.each([
  makeStaticMethod('local.Project.Module.Type.foo_method'),
  makeCon('local.Project.Module.Type.Foo_Con'),
  {
    ...makeStaticMethod('local.Project.Module.Type.foo_extension'),
    definedIn: 'local.Project.Another_Module' as QualifiedName,
  },
])('$name entry is in the local.Project.Module.Type content', (entry) => {
  const filtering = new Filtering({ qualifiedNamePattern: 'local.Project.Module.Type' })
  const filteringWithPattern = new Filtering({
    pattern: 'foo',
    qualifiedNamePattern: 'local.Project.Module.Type',
  })
  expect(filtering.filter(entry)).not.toBeNull()
  expect(filteringWithPattern.filter(entry)).not.toBeNull()
})

test.each([
  makeType('local.Project.Module.Type'),
  makeModuleMethod('local.Project.Module.module_method'),
  makeStaticMethod('local.Project.Module.Another_Type.another_type_method'),
  makeStaticMethod('local.Project.Another_Module.Type.another_module_type_method'),
])('$name entry is not in the local.Project.Module.Type content', (entry) => {
  const filtering = new Filtering({ qualifiedNamePattern: 'local.Project.Module.Type' })
  expect(filtering.filter(entry)).toBeNull()
})

test('An Instance method is shown when self type matches', () => {
  const entry = makeMethod('Standard.Base.Data.Vector.Vector.get')
  const filteringWithSelfType = new Filtering({
    selfType: 'Standard.Base.Data.Vector.Vector' as QualifiedName,
  })
  expect(filteringWithSelfType.filter(entry)).not.toBeNull()
  const filteringWithoutSelfType = new Filtering({ pattern: 'get' })
  expect(filteringWithoutSelfType.filter(entry)).toBeNull()
})

test.each([
  makeModule('Standard.Base.Data.Vector'),
  makeStaticMethod('Standard.Base.Data.Vector.Vector.new'),
  makeCon('Standard.Base.Data.Vector.Vector.Vector_Con'),
  makeLocal('Standard.Base.Data.Vector', 'get'),
  makeFunction('Standard.Base.Data.Vector', 'func'),
  makeMethod('Standard.Base.Data.Vector.Vecto.get'),
  makeMethod('Standard.Base.Data.Vector.Vector2.get'),
])('$name is filtered out when Vector self type is specified', (entry) => {
  const filtering = new Filtering({
    selfType: 'Standard.Base.Data.Vector.Vector' as QualifiedName,
  })
  expect(filtering.filter(entry)).toBeNull()
})

test.each(['bar', 'barfoo', 'fo', 'bar_fo_bar'])("%s is not matched by pattern 'foo'", (name) => {
  const pattern = 'foo'
  const entry = makeModuleMethod(`local.Project.${name}`)
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
    const entry = { ...makeModuleMethod(`local.Project.${name}`), aliases: aliases ?? [] }
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
    const entry = { ...makeModuleMethod(`local.Project.${name}`), aliases: aliases ?? [] }
    return filtering.filter(entry)
  })
  expect(matchResults[0]).not.toBeNull()
  for (let i = 1; i < matchResults.length; i++) {
    expect(matchResults[i]).not.toBeNull()
    expect(matchResults[i]?.score).toBeGreaterThan(matchResults[i - 1]?.score ?? Infinity)
  }
})

test('Unstable filtering', () => {
  const stableEntry = makeStaticMethod('local.Project.Type.stable')
  const unstableEntry = {
    ...makeStaticMethod('local.Project.Type.unstable'),
    isUnstable: true,
  }
  const stableFiltering = new Filtering({ qualifiedNamePattern: 'local.Project.Type' })
  expect(stableFiltering.filter(stableEntry)).not.toBeNull()
  expect(stableFiltering.filter(unstableEntry)).toBeNull()
  const unstableFiltering = new Filtering({
    qualifiedNamePattern: 'local.Project.Type',
    showUnstable: true,
  })
  expect(unstableFiltering.filter(stableEntry)).not.toBeNull()
  expect(unstableFiltering.filter(unstableEntry)).not.toBeNull()
})
