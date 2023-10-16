import { parseDocs } from '@/util/docParser'
import { tryIdentifier, tryQualifiedName } from '@/util/qualifiedName'
import { unwrap } from '@/util/result'
import * as lsTypes from 'shared/languageServerTypes/suggestions'
import { expect, test } from 'vitest'
import { SuggestionDb, type Group } from '..'
import { SuggestionKind, entryQn, type SuggestionEntry } from '../entry'
import { applyUpdates } from '../lsUpdate'

test('Adding suggestion database entries', () => {
  const test = new Fixture()
  const db = new SuggestionDb()
  applyUpdates(db, test.addUpdatesForExpected(), test.groups)
  test.check(db)
})

test('Entry qualified names', () => {
  const test = new Fixture()
  const db = test.createDbWithExpected()
  expect(entryQn(db.get(1))).toStrictEqual('Standard.Base')
  expect(entryQn(db.get(2))).toStrictEqual('Standard.Base.Type')
  expect(entryQn(db.get(3))).toStrictEqual('Standard.Base.Type.Con')
  expect(entryQn(db.get(4))).toStrictEqual('Standard.Base.Type.method')
  expect(entryQn(db.get(5))).toStrictEqual('Standard.Base.Type.static_method')
  expect(entryQn(db.get(6))).toStrictEqual('Standard.Base.function')
  expect(entryQn(db.get(7))).toStrictEqual('Standard.Base.local')
})

test('Qualified name indexing', () => {
  const test = new Fixture()
  const db = new SuggestionDb()
  applyUpdates(db, test.addUpdatesForExpected(), test.groups)
  for (let i = 1; i <= 7; i++) {
    const qName = entryQn(db.get(i))
    expect(db.nameToId.lookup(qName)).toEqual(new Set([i]))
    expect(db.nameToId.reverseLookup(i)).toEqual(new Set([qName]))
  }
})

test('Parent-children indexing', () => {
  const test = new Fixture()
  const db = new SuggestionDb()
  applyUpdates(db, test.addUpdatesForExpected(), test.groups)
  // Parent lookup.
  expect(db.parent.lookup(1)).toEqual(new Set([]))
  expect(db.parent.lookup(2)).toEqual(new Set([1]))
  expect(db.parent.lookup(3)).toEqual(new Set([2]))
  expect(db.parent.lookup(4)).toEqual(new Set([2]))
  expect(db.parent.lookup(5)).toEqual(new Set([2]))
  expect(db.parent.lookup(6)).toEqual(new Set([1]))
  expect(db.parent.lookup(7)).toEqual(new Set([1]))

  // Children lookup.
  expect(db.parent.reverseLookup(1)).toEqual(new Set([2, 6, 7]))
  expect(db.parent.reverseLookup(2)).toEqual(new Set([3, 4, 5]))
  expect(db.parent.reverseLookup(3)).toEqual(new Set([]))
  expect(db.parent.reverseLookup(4)).toEqual(new Set([]))
  expect(db.parent.reverseLookup(5)).toEqual(new Set([]))
  expect(db.parent.reverseLookup(6)).toEqual(new Set([]))
  expect(db.parent.reverseLookup(7)).toEqual(new Set([]))

  // Add new entry.
  const modifications: lsTypes.SuggestionsDatabaseUpdate[] = [
    {
      type: 'Add',
      id: 8,
      suggestion: {
        type: 'method',
        module: 'Standard.Base',
        name: 'method2',
        selfType: 'Standard.Base.Type',
        isStatic: false,
        arguments: [],
        returnType: 'Standard.Base.Number',
        documentation: '',
        annotations: [],
      },
    },
  ]
  applyUpdates(db, modifications, test.groups)
  expect(db.parent.lookup(8)).toEqual(new Set([2]))
  expect(db.parent.reverseLookup(8)).toEqual(new Set([]))
  expect(db.parent.reverseLookup(2)).toEqual(new Set([3, 4, 5, 8]))

  // Remove entry.
  const modifications2: lsTypes.SuggestionDatabaseUpdate[] = [{ type: 'Remove', id: 3 }]
  applyUpdates(db, modifications2, test.groups)
  expect(db.parent.lookup(3)).toEqual(new Set([]))
  expect(db.parent.reverseLookup(2)).toEqual(new Set([4, 5, 8]))

  // Modify entry. Moving new method from `Standard.Base.Type` to `Standard.Base`.
  db.get(8).memberOf = 'Standard.Base'
  expect(db.parent.reverseLookup(1)).toEqual(new Set([2, 6, 7, 8]))
  expect(db.parent.lookup(8)).toEqual(new Set([1]))
  expect(db.parent.reverseLookup(8)).toEqual(new Set([]))
  expect(db.parent.reverseLookup(2)).toEqual(new Set([4, 5]))
})

test("Modifying suggestion entries' fields", () => {
  const scope2 = {
    start: { line: 1, character: 20 },
    end: { line: 20, character: 1 },
  }
  const typeDocs2 = 'ALIAS Test Type 2\n\nA Test type 2'
  const test = new Fixture()
  const modifications: lsTypes.SuggestionsDatabaseUpdate[] = [
    {
      type: 'Modify',
      id: 1,
      module: { tag: 'Set', value: 'Standard.Base2' },
      reexport: { tag: 'Set', value: 'Standard.Base.Yet.Another.Module' },
    },
    {
      type: 'Modify',
      id: 2,
      module: { tag: 'Set', value: 'Standard.Base2' },
      documentation: { tag: 'Set', value: typeDocs2 },
    },
    { type: 'Modify', id: 3, returnType: { tag: 'Set', value: 'Standard.Base2.Type' } },
    { type: 'Modify', id: 4, selfType: { tag: 'Set', value: 'Standard.Base2.Type' } },
    { type: 'Modify', id: 5, selfType: { tag: 'Set', value: 'Standard.Base2.Type' } },
    { type: 'Modify', id: 6, scope: { tag: 'Set', value: scope2 } },
  ]
  const db = test.createDbWithExpected()
  test.expectedModule.name = unwrap(tryIdentifier('Base2'))
  test.expectedModule.definedIn = unwrap(tryQualifiedName('Standard.Base2'))
  test.expectedModule.returnType = 'Standard.Base2'
  test.expectedModule.reexportedIn = unwrap(tryQualifiedName('Standard.Base.Yet.Another.Module'))
  test.expectedType.definedIn = unwrap(tryQualifiedName('Standard.Base2'))
  test.expectedType.returnType = 'Standard.Base2.Type'
  test.expectedType.aliases = ['Test Type 2']
  test.expectedType.documentation = parseDocs(typeDocs2)
  test.expectedCon.memberOf = unwrap(tryQualifiedName('Standard.Base2.Type'))
  test.expectedCon.returnType = unwrap(tryQualifiedName('Standard.Base2.Type'))
  test.expectedMethod.memberOf = unwrap(tryQualifiedName('Standard.Base2.Type'))
  test.expectedMethod.selfType = 'Standard.Base2.Type'
  test.expectedStaticMethod.memberOf = unwrap(tryQualifiedName('Standard.Base2.Type'))
  test.expectedFunction.scope = scope2

  applyUpdates(db, modifications, test.groups)
  test.check(db)
})

test("Unsetting suggestion entries' fields", () => {
  const test = new Fixture()
  const modifications: lsTypes.SuggestionsDatabaseUpdate[] = [
    {
      type: 'Modify',
      id: 1,
      reexport: { tag: 'Remove' },
    },
    {
      type: 'Modify',
      id: 2,
      documentation: { tag: 'Remove' },
    },
    { type: 'Modify', id: 3, documentation: { tag: 'Remove' } },
    { type: 'Modify', id: 4, documentation: { tag: 'Remove' } },
  ]
  const db = test.createDbWithExpected()
  delete test.expectedModule.reexportedIn
  test.expectedType.documentation = []
  test.expectedType.aliases = []
  test.expectedCon.documentation = []
  test.expectedCon.isUnstable = false
  test.expectedMethod.documentation = []
  delete test.expectedMethod.groupIndex

  applyUpdates(db, modifications, test.groups)
  test.check(db)
})

test('Removing entries from database', () => {
  const test = new Fixture()
  const update: lsTypes.SuggestionsDatabaseUpdate[] = [
    { type: 'Remove', id: 2 },
    { type: 'Remove', id: 6 },
  ]
  const db = test.createDbWithExpected()
  applyUpdates(db, update, test.groups)
  expect(db.get(1)).toStrictEqual(test.expectedModule)
  expect(db.get(2)).toBeUndefined()
  expect(db.get(3)).toStrictEqual(test.expectedCon)
  expect(db.get(4)).toStrictEqual(test.expectedMethod)
  expect(db.get(5)).toStrictEqual(test.expectedStaticMethod)
  expect(db.get(6)).toBeUndefined()
  expect(db.get(7)).toStrictEqual(test.expectedLocal)
})

test('Adding new argument', () => {
  const test = new Fixture()
  const newArg: lsTypes.SuggestionEntryArgument = {
    name: 'c',
    type: 'Any',
    hasDefault: false,
    isSuspended: false,
  }
  const modifications: lsTypes.SuggestionsDatabaseUpdate[] = [
    { type: 'Modify', id: 2, arguments: [{ type: 'Add', index: 0, argument: newArg }] },
    { type: 'Modify', id: 3, arguments: [{ type: 'Add', index: 1, argument: newArg }] },
    { type: 'Modify', id: 5, arguments: [{ type: 'Add', index: 1, argument: newArg }] },
  ]
  const db = test.createDbWithExpected()
  test.expectedType.arguments = [newArg, test.arg1]
  test.expectedCon.arguments = [test.arg1, newArg]
  test.expectedStaticMethod.arguments = [test.arg1, newArg, test.arg2]

  applyUpdates(db, modifications, test.groups)
  test.check(db)
})

test('Modifying arguments', () => {
  const newArg1 = {
    name: 'c',
    type: 'Standard.Base.Number',
    isSuspended: true,
    hasDefault: false,
  }
  const newArg2 = {
    name: 'b',
    type: 'Any',
    isSuspended: false,
    hasDefault: true,
    defaultValue: 'Nothing',
  }
  const test = new Fixture()
  const modifications: lsTypes.SuggestionsDatabaseUpdate[] = [
    {
      type: 'Modify',
      id: 5,
      arguments: [
        {
          type: 'Modify',
          index: 0,
          name: { tag: 'Set', value: 'c' },
          reprType: { tag: 'Set', value: 'Standard.Base.Number' },
          isSuspended: { tag: 'Set', value: true },
          hasDefault: { tag: 'Set', value: false },
          defaultValue: { tag: 'Remove' },
        },
        {
          type: 'Modify',
          index: 1,
          hasDefault: { tag: 'Set', value: true },
          defaultValue: { tag: 'Set', value: 'Nothing' },
        },
      ],
    },
  ]
  const db = test.createDbWithExpected()
  test.expectedStaticMethod.arguments = [newArg1, newArg2]

  applyUpdates(db, modifications, test.groups)
  test.check(db)
})

test('Removing Arguments', () => {
  const test = new Fixture()
  const update: lsTypes.SuggestionsDatabaseUpdate[] = [
    { type: 'Modify', id: 4, arguments: [{ type: 'Remove', index: 0 }] },
    { type: 'Modify', id: 5, arguments: [{ type: 'Remove', index: 1 }] },
  ]
  const db = test.createDbWithExpected()
  test.expectedMethod.arguments = []
  test.expectedStaticMethod.arguments = [test.arg1]

  applyUpdates(db, update, test.groups)
  test.check(db)
})

class Fixture {
  groups: Group[] = [
    { name: 'Test1', project: unwrap(tryQualifiedName('Standard.Base')) },
    { name: 'Test2', project: unwrap(tryQualifiedName('Standard.Base')) },
  ]
  arg1 = {
    name: 'a',
    type: 'Any',
    isSuspended: false,
    hasDefault: true,
    defaultValue: 'Nothing',
  }
  arg2 = {
    name: 'b',
    type: 'Any',
    isSuspended: false,
    hasDefault: false,
  }
  scope = {
    start: { line: 1, character: 10 },
    end: { line: 10, character: 1 },
  }
  moduleDocs = 'A base module'
  typeDocs = 'ALIAS Test Type\n\nA Test type'
  conDocs = 'ADVANCED\n\nA Constructor'
  methodDocs = 'GROUP Test1\n\nAn instance method'
  staticMethodDocs = 'GROUP Test2\n\nA static method'
  functionDocs = 'A local function'
  localDocs = 'A local variable'
  expectedModule: SuggestionEntry = {
    kind: SuggestionKind.Module,
    name: unwrap(tryIdentifier('Base')),
    definedIn: unwrap(tryQualifiedName('Standard.Base')),
    arguments: [],
    returnType: 'Standard.Base',
    documentation: parseDocs(this.moduleDocs),
    aliases: [],
    isPrivate: false,
    isUnstable: false,
    reexportedIn: unwrap(tryQualifiedName('Standard.Base.Another.Module')),
  }
  expectedType: SuggestionEntry = {
    kind: SuggestionKind.Type,
    name: unwrap(tryIdentifier('Type')),
    definedIn: unwrap(tryQualifiedName('Standard.Base')),
    arguments: [this.arg1],
    returnType: 'Standard.Base.Type',
    documentation: parseDocs(this.typeDocs),
    aliases: ['Test Type'],
    isPrivate: false,
    isUnstable: false,
    reexportedIn: unwrap(tryQualifiedName('Standard.Base.Another.Module')),
  }
  expectedCon: SuggestionEntry = {
    kind: SuggestionKind.Constructor,
    name: unwrap(tryIdentifier('Con')),
    definedIn: unwrap(tryQualifiedName('Standard.Base')),
    memberOf: unwrap(tryQualifiedName('Standard.Base.Type')),
    arguments: [this.arg1],
    returnType: 'Standard.Base.Type',
    documentation: parseDocs(this.conDocs),
    aliases: [],
    isPrivate: false,
    isUnstable: true,
    reexportedIn: unwrap(tryQualifiedName('Standard.Base.Another.Module')),
  }
  expectedMethod: SuggestionEntry = {
    kind: SuggestionKind.Method,
    name: unwrap(tryIdentifier('method')),
    definedIn: unwrap(tryQualifiedName('Standard.Base')),
    memberOf: unwrap(tryQualifiedName('Standard.Base.Type')),
    selfType: 'Standard.Base.Type',
    arguments: [this.arg1],
    returnType: 'Standard.Base.Number',
    documentation: parseDocs(this.methodDocs),
    groupIndex: 0,
    aliases: [],
    isPrivate: false,
    isUnstable: false,
  }
  expectedStaticMethod: SuggestionEntry = {
    kind: SuggestionKind.Method,
    name: unwrap(tryIdentifier('static_method')),
    definedIn: unwrap(tryQualifiedName('Standard.Base')),
    memberOf: unwrap(tryQualifiedName('Standard.Base.Type')),
    arguments: [this.arg1, this.arg2],
    returnType: 'Standard.Base.Number',
    documentation: parseDocs(this.staticMethodDocs),
    groupIndex: 1,
    aliases: [],
    isPrivate: false,
    isUnstable: false,
    reexportedIn: unwrap(tryQualifiedName('Standard.Base.Another.Module')),
  }
  expectedFunction: SuggestionEntry = {
    kind: SuggestionKind.Function,
    name: unwrap(tryIdentifier('function')),
    definedIn: unwrap(tryQualifiedName('Standard.Base')),
    arguments: [this.arg1],
    returnType: 'Standard.Base.Number',
    documentation: parseDocs(this.functionDocs),
    aliases: [],
    isPrivate: false,
    isUnstable: false,
    scope: this.scope,
  }
  expectedLocal: SuggestionEntry = {
    kind: SuggestionKind.Function,
    name: unwrap(tryIdentifier('local')),
    definedIn: unwrap(tryQualifiedName('Standard.Base')),
    arguments: [],
    returnType: 'Standard.Base.Number',
    documentation: parseDocs(this.localDocs),
    aliases: [],
    isPrivate: false,
    isUnstable: false,
    scope: this.scope,
  }

  addUpdatesForExpected(): lsTypes.SuggestionsDatabaseUpdate[] {
    return [
      {
        type: 'Add',
        id: 1,
        suggestion: {
          type: 'module',
          module: 'Standard.Base',
          documentation: this.moduleDocs,
          reexport: 'Standard.Base.Another.Module',
        },
      },
      {
        type: 'Add',
        id: 2,
        suggestion: {
          type: 'type',
          module: 'Standard.Base',
          name: 'Type',
          params: [this.arg1],
          documentation: this.typeDocs,
          reexport: 'Standard.Base.Another.Module',
        },
      },
      {
        type: 'Add',
        id: 3,
        suggestion: {
          type: 'constructor',
          module: 'Standard.Base',
          name: 'Con',
          arguments: [this.arg1],
          returnType: 'Standard.Base.Type',
          documentation: this.conDocs,
          reexport: 'Standard.Base.Another.Module',
          annotations: [],
        },
      },
      {
        type: 'Add',
        id: 4,
        suggestion: {
          type: 'method',
          module: 'Standard.Base',
          name: 'method',
          selfType: 'Standard.Base.Type',
          isStatic: false,
          arguments: [this.arg1],
          returnType: 'Standard.Base.Number',
          documentation: this.methodDocs,
          annotations: [],
        },
      },
      {
        type: 'Add',
        id: 5,
        suggestion: {
          type: 'method',
          module: 'Standard.Base',
          name: 'static_method',
          selfType: 'Standard.Base.Type',
          isStatic: true,
          arguments: [this.arg1, this.arg2],
          returnType: 'Standard.Base.Number',
          documentation: this.staticMethodDocs,
          reexport: 'Standard.Base.Another.Module',
          annotations: [],
        },
      },
      {
        type: 'Add',
        id: 6,
        suggestion: {
          type: 'function',
          module: 'Standard.Base',
          name: 'function',
          arguments: [this.arg1],
          returnType: 'Standard.Base.Number',
          scope: this.scope,
          documentation: this.functionDocs,
        },
      },
      {
        type: 'Add',
        id: 7,
        suggestion: {
          type: 'local',
          module: 'Standard.Base',
          name: 'local',
          returnType: 'Standard.Base.Number',
          scope: this.scope,
          documentation: this.localDocs,
        },
      },
    ]
  }

  createDbWithExpected(): SuggestionDb {
    const db = new SuggestionDb()
    db.set(1, structuredClone(this.expectedModule))
    db.set(2, structuredClone(this.expectedType))
    db.set(3, structuredClone(this.expectedCon))
    db.set(4, structuredClone(this.expectedMethod))
    db.set(5, structuredClone(this.expectedStaticMethod))
    db.set(6, structuredClone(this.expectedFunction))
    db.set(7, structuredClone(this.expectedLocal))
    return db
  }

  check(db: SuggestionDb): void {
    expect(db.get(1)).toStrictEqual(this.expectedModule)
    expect(db.get(2)).toStrictEqual(this.expectedType)
    expect(db.get(3)).toStrictEqual(this.expectedCon)
    expect(db.get(4)).toStrictEqual(this.expectedMethod)
    expect(db.get(5)).toStrictEqual(this.expectedStaticMethod)
    expect(db.get(6)).toStrictEqual(this.expectedFunction)
    expect(db.get(7)).toStrictEqual(this.expectedLocal)
  }
}
