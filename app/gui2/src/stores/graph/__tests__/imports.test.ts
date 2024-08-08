import {
  addImports,
  covers,
  detectImportConflicts,
  recognizeImport,
  requiredImportToAst,
  requiredImports,
  type ConflictInfo,
  type Import,
  type RequiredImport,
} from '@/stores/graph/imports'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  makeConstructor,
  makeMethod,
  makeModule,
  makeStaticMethod,
  makeType,
} from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { unwrap } from '@/util/data/result'
import { tryIdentifier, tryQualifiedName } from '@/util/qualifiedName'
import { expect, test } from 'vitest'

test.each([
  {
    description: 'Direct import of a module',
    existing: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'Module' },
    } as Import,
    required: {
      kind: 'Qualified',
      module: unwrap(tryQualifiedName('Standard.Base')),
    } as RequiredImport,
    expected: true,
  },
  {
    description: 'Module imported by alias',
    existing: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'Module', alias: unwrap(tryIdentifier('MyBase')) },
    } as Import,
    required: {
      kind: 'Qualified',
      module: unwrap(tryQualifiedName('Standard.Base')),
    } as RequiredImport,
    expected: true,
  },
  {
    description: 'Module imported from parent',
    existing: {
      from: unwrap(tryQualifiedName('Standard')),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Base'))] },
    } as Import,
    required: {
      kind: 'Qualified',
      module: unwrap(tryQualifiedName('Standard.Base')),
    } as RequiredImport,
    expected: true,
  },
  {
    description: 'Module imported from parent with all',
    existing: {
      from: unwrap(tryQualifiedName('Standard')),
      imported: { kind: 'All', except: [] },
    } as Import,
    required: {
      kind: 'Qualified',
      module: unwrap(tryQualifiedName('Standard.Base')),
    } as RequiredImport,
    expected: true,
  },
  {
    description: 'Module hidden when importing all from parent',
    existing: {
      from: unwrap(tryQualifiedName('Standard')),
      imported: { kind: 'All', except: [unwrap(tryIdentifier('Base'))] },
    } as Import,
    required: {
      kind: 'Qualified',
      module: unwrap(tryQualifiedName('Standard.Base')),
    } as RequiredImport,
    expected: false,
  },
  {
    description: 'Type imported from module by name',
    existing: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] },
    } as Import,
    required: {
      kind: 'Unqualified',
      from: unwrap(tryQualifiedName('Standard.Base')),
      import: unwrap(tryIdentifier('Table')),
    } as RequiredImport,
    expected: true,
  },
  {
    description: 'Type imported from module by all',
    existing: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'All', except: [] },
    } as Import,
    required: {
      kind: 'Unqualified',
      from: unwrap(tryQualifiedName('Standard.Base')),
      import: unwrap(tryIdentifier('Table')),
    } as RequiredImport,
    expected: true,
  },
  {
    description: 'Type hidden when importing all',
    existing: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'All', except: [unwrap(tryIdentifier('Table'))] },
    } as Import,
    required: {
      kind: 'Unqualified',
      from: unwrap(tryQualifiedName('Standard.Base')),
      import: unwrap(tryIdentifier('Table')),
    } as RequiredImport,
    expected: false,
  },
])('Existing imports cover required, $description', ({ existing, required, expected }) => {
  expect(covers(existing, required)).toStrictEqual(expected)
})

const mockDb = () => {
  const db = new SuggestionDb()
  const reexportedModule = makeModule('Standard.AWS.Connections')
  reexportedModule.reexportedIn = unwrap(tryQualifiedName('Standard.Base'))
  const reexportedType = makeModule('Standard.Database.DB_Table.DB_Table')
  reexportedType.reexportedIn = unwrap(tryQualifiedName('Standard.Base'))
  const extensionMethod = makeMethod('Standard.Network.URI.fetch')
  extensionMethod.definedIn = unwrap(tryQualifiedName('Standard.Base'))
  db.set(1, makeModule('Standard.Base'))
  db.set(2, makeType('Standard.Base.Type'))
  db.set(3, reexportedModule)
  db.set(4, reexportedType)
  db.set(5, makeConstructor('Standard.Base.Type.Constructor'))
  db.set(6, makeStaticMethod('Standard.Base.Type.staticMethod'))
  db.set(7, makeMethod('Standard.Base.Type.method'))
  db.set(8, makeType('Standard.Network.URI'))
  db.set(9, extensionMethod)
  db.set(10, makeType('Standard.Base.Vector'))
  db.set(11, makeStaticMethod('Standard.Base.Vector.new'))
  db.set(12, makeModule('Project.Foo'))
  db.set(13, makeType('Project.Foo.Vector'))
  db.set(14, makeStaticMethod('Project.Foo.Vector.new'))
  db.set(15, makeModule('Project.Foo.Base'))

  return db
}

const qn = (s: string) => unwrap(tryQualifiedName(s))
test.each([
  {
    description: 'Conflicting Vector',
    importing: {
      kind: 'Unqualified',
      from: qn('Project.Foo'),
      import: 'Vector',
    } as RequiredImport,
    alreadyImported: [
      { from: qn('Standard.Base'), imported: { kind: 'List', names: ['Vector'] } } as Import,
    ],
    expected: { name: 'Vector', fullyQualified: 'Project.Foo.Vector' },
  },
  {
    description: 'Conflicting Vector (2)',
    importing: {
      kind: 'Unqualified',
      from: qn('Project.Foo'),
      import: 'Vector',
    } as RequiredImport,
    alreadyImported: [
      { from: qn('Standard.Base'), imported: { kind: 'All', except: [] } } as Import,
    ],
    expected: { name: 'Vector', fullyQualified: 'Project.Foo.Vector' },
  },
])('Conflicting imports: $description', ({ importing, alreadyImported, expected }) => {
  const db = mockDb()

  const existingImports: Import[] = alreadyImported
  const conflicts = detectImportConflicts(db, existingImports, importing)
  expect(conflicts).toEqual({
    detected: true,
    pattern: expected.name,
    fullyQualified: expected.fullyQualified,
  } as ConflictInfo)
})

test.each([
  {
    id: 1,
    expected: [
      {
        kind: 'Qualified',
        module: unwrap(tryQualifiedName('Standard.Base')),
      },
    ],
  },
  {
    id: 2,
    expected: [
      {
        kind: 'Unqualified',
        from: unwrap(tryQualifiedName('Standard.Base')),
        import: unwrap(tryIdentifier('Type')),
      },
    ],
  },
  {
    id: 3,
    expected: [
      {
        kind: 'Unqualified',
        from: unwrap(tryQualifiedName('Standard.Base')),
        import: unwrap(tryIdentifier('Connections')),
      },
    ],
  },
  {
    id: 4,
    expected: [
      {
        kind: 'Unqualified',
        from: unwrap(tryQualifiedName('Standard.Base')),
        import: unwrap(tryIdentifier('DB_Table')),
      },
    ],
  },
  {
    id: 5,
    expected: [
      {
        kind: 'Unqualified',
        from: unwrap(tryQualifiedName('Standard.Base')),
        import: unwrap(tryIdentifier('Type')),
      },
    ],
  },
  {
    id: 6,
    expected: [
      {
        kind: 'Unqualified',
        from: unwrap(tryQualifiedName('Standard.Base')),
        import: unwrap(tryIdentifier('Type')),
      },
    ],
  },
  {
    id: 7,
    expected: [],
  },
  {
    id: 9,
    expected: [
      {
        kind: 'Qualified',
        module: unwrap(tryQualifiedName('Standard.Base')),
      },
    ],
  },
])('Required imports $id', ({ id, expected }) => {
  const db = mockDb()
  expect(requiredImports(db, db.get(id)!)).toStrictEqual(expected)
})

const parseImport = (code: string): Import | null => {
  const ast = Ast.Import.tryParse(code)
  return ast ? recognizeImport(ast) : null
}

test.each([
  { code: '1 + 1', expected: null },
  { code: 'import Standard.(2+2).Base', expected: null },
  {
    code: 'from Standard.Base import all',
    expected: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'All', except: [] },
    },
  },
  {
    code: 'from Standard.Base.Table import Table',
    expected: {
      from: unwrap(tryQualifiedName('Standard.Base.Table')),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] },
    },
  },
  {
    code: 'import AWS.Connection as Backend',
    expected: {
      from: unwrap(tryQualifiedName('AWS.Connection')),
      imported: { kind: 'Module', alias: 'Backend' },
    },
  },
  {
    code: 'import Standard.Base.Data',
    expected: {
      from: unwrap(tryQualifiedName('Standard.Base.Data')),
      imported: { kind: 'Module' },
    },
  },
  {
    code: 'import local',
    expected: {
      from: unwrap(tryQualifiedName('local')),
      imported: { kind: 'Module' },
    },
  },
  {
    code: 'from Standard.Base import Foo, Bar',
    expected: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: {
        kind: 'List',
        names: [unwrap(tryIdentifier('Foo')), unwrap(tryIdentifier('Bar'))],
      },
    },
  },
  {
    code: 'from   Standard  . Base import  Foo ,  Bar ,Buz',
    expected: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: {
        kind: 'List',
        names: [
          unwrap(tryIdentifier('Foo')),
          unwrap(tryIdentifier('Bar')),
          unwrap(tryIdentifier('Buz')),
        ],
      },
    },
  },
  {
    code: 'from Standard.Base import all hiding Foo, Bar',
    expected: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: {
        kind: 'All',
        except: [unwrap(tryIdentifier('Foo')), unwrap(tryIdentifier('Bar'))],
      },
    },
  },
  {
    code: 'from   Standard  . Base import  all  hiding  Foo ,  Bar ,Buz',
    expected: {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: {
        kind: 'All',
        except: [
          unwrap(tryIdentifier('Foo')),
          unwrap(tryIdentifier('Bar')),
          unwrap(tryIdentifier('Buz')),
        ],
      },
    },
  },
])('Recognizing import $code', ({ code, expected }) => {
  expect(parseImport(code)).toStrictEqual(expected)
})

test.each([
  {
    import: {
      kind: 'Unqualified',
      from: unwrap(tryQualifiedName('Standard.Base.Table')),
      import: unwrap(tryIdentifier('Table')),
    } satisfies RequiredImport,
    expected: 'from Standard.Base.Table import Table',
  },
  {
    import: {
      kind: 'Qualified',
      module: unwrap(tryQualifiedName('Standard.Base.Data')),
    } satisfies RequiredImport,
    expected: 'import Standard.Base.Data',
  },
  {
    import: {
      kind: 'Qualified',
      module: unwrap(tryQualifiedName('local')),
    } satisfies RequiredImport,
    expected: 'import local',
  },
])('Generating import $expected', ({ import: import_, expected }) => {
  expect(requiredImportToAst(import_).code()).toStrictEqual(expected)
})

test('Insert after other imports in module', () => {
  const module_ = Ast.parseBlock('from Standard.Base import all\n\nmain = 42\n')
  const edit = module_.module.edit()
  addImports(edit.getVersion(module_), [
    { kind: 'Qualified', module: unwrap(tryQualifiedName('Standard.Visualization')) },
  ])
  expect(edit.getVersion(module_).code()).toBe(
    'from Standard.Base import all\nimport Standard.Visualization\n\nmain = 42\n',
  )
})

test('Insert import in module with no other imports', () => {
  const module_ = Ast.parseBlock('main = 42\n')
  const edit = module_.module.edit()
  addImports(edit.getVersion(module_), [
    { kind: 'Qualified', module: unwrap(tryQualifiedName('Standard.Visualization')) },
  ])
  expect(edit.getVersion(module_).code()).toBe('import Standard.Visualization\nmain = 42\n')
})
