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
import { mockProjectNameStore } from '@/stores/projectNames'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  makeConstructor,
  makeMethod,
  makeModule,
  makeStaticMethod,
  makeType,
} from '@/stores/suggestionDatabase/mockSuggestion'
import { Ast } from '@/util/ast'
import { unwrap } from '@/util/data/result'
import { tryIdentifier, tryQualifiedName, type Identifier } from '@/util/qualifiedName'
import { expect, test } from 'vitest'
import { assertDefined } from 'ydoc-shared/util/assert'

const qn = (s: string) => unwrap(tryQualifiedName(s))

const projectNames = mockProjectNameStore('local', 'Project')

function projectPath(path: string) {
  return unwrap(projectNames.parseProjectPath(unwrap(tryQualifiedName(path))))
}

interface CoverCase {
  description: string
  existing: Import
  required: RequiredImport
  expected: boolean
}
test.each<CoverCase>([
  {
    description: 'Direct import of a module',
    existing: {
      from: qn('Standard.Base'),
      imported: { kind: 'Module' },
    },
    required: {
      kind: 'Qualified',
      module: projectPath('Standard.Base'),
    },
    expected: true,
  },
  {
    description: 'Module imported by alias',
    existing: {
      from: qn('Standard.Base'),
      imported: { kind: 'Module', alias: unwrap(tryIdentifier('MyBase')) },
    },
    required: {
      kind: 'Qualified',
      module: projectPath('Standard.Base'),
    },
    expected: true,
  },
  {
    description: 'Module imported from parent',
    existing: {
      from: qn('Standard.Numbers'),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Number'))] },
    },
    required: {
      kind: 'Qualified',
      module: projectPath('Standard.Numbers.Number'),
    },
    expected: true,
  },
  {
    description: 'Module imported from parent with all',
    existing: {
      from: qn('Standard.Numbers'),
      imported: { kind: 'All', except: [] },
    },
    required: {
      kind: 'Qualified',
      module: projectPath('Standard.Numbers.Number'),
    },
    expected: true,
  },
  {
    description: 'Module hidden when importing all from parent',
    existing: {
      from: qn('Standard.Numbers'),
      imported: { kind: 'All', except: [unwrap(tryIdentifier('Number'))] },
    },
    required: {
      kind: 'Qualified',
      module: projectPath('Standard.Numbers.Number'),
    },
    expected: false,
  },
  {
    description: 'Type imported from module by name',
    existing: {
      from: qn('Standard.Base'),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] },
    },
    required: {
      kind: 'Unqualified',
      from: projectPath('Standard.Base'),
      import: unwrap(tryIdentifier('Table')),
    },
    expected: true,
  },
  {
    description: 'Type imported from module by all',
    existing: {
      from: qn('Standard.Base'),
      imported: { kind: 'All', except: [] },
    },
    required: {
      kind: 'Unqualified',
      from: projectPath('Standard.Base'),
      import: unwrap(tryIdentifier('Table')),
    },
    expected: true,
  },
  {
    description: 'Type hidden when importing all',
    existing: {
      from: qn('Standard.Base'),
      imported: { kind: 'All', except: [unwrap(tryIdentifier('Table'))] },
    },
    required: {
      kind: 'Unqualified',
      from: projectPath('Standard.Base'),
      import: unwrap(tryIdentifier('Table')),
    },
    expected: false,
  },
])('Existing imports cover required, $description', ({ existing, required, expected }) => {
  expect(
    covers(
      {
        from: unwrap(projectNames.parseProjectPath(existing.from)),
        imported: existing.imported,
      },
      required,
    ),
  ).toStrictEqual(expected)
})

const mockDb = () => {
  const db = new SuggestionDb()
  db.set(1, makeModule('Standard.Base'))
  db.set(2, makeType('Standard.Base.Type'))
  db.set(
    3,
    makeModule('Standard.AWS.Connections', {
      reexport: 'Standard.Base',
    }),
  )
  db.set(
    4,
    makeModule('Standard.Database.DB_Table.DB_Table', {
      reexport: 'Standard.Base',
    }),
  )
  db.set(5, makeConstructor('Standard.Base.Type.Constructor'))
  db.set(6, makeStaticMethod('Standard.Base.Type.static_method'))
  db.set(7, makeMethod('Standard.Base.Type.method'))
  db.set(8, makeType('Standard.Network.URI'))
  db.set(
    9,
    makeMethod('Standard.Network.URI.fetch', {
      module: 'Standard.Base',
    }),
  )
  db.set(10, makeType('Standard.Base.Vector'))
  db.set(11, makeStaticMethod('Standard.Base.Vector.new'))
  db.set(12, makeModule('Project.Foo'))
  db.set(13, makeType('Project.Foo.Vector'))
  db.set(14, makeStaticMethod('Project.Foo.Vector.new'))
  db.set(15, makeModule('Project.Foo.Base'))

  return db
}

interface ConflictCase {
  description: string
  importing: RequiredImport
  alreadyImported: Import[]
  expected: { name: 'Vector'; fullyQualified: 'Project.Foo.Vector' }
}

test.each<ConflictCase>([
  {
    description: 'Conflicting Vector',
    importing: {
      kind: 'Unqualified',
      from: projectPath('Project.Foo'),
      import: 'Vector' as Identifier,
    },
    alreadyImported: [
      { from: qn('Standard.Base'), imported: { kind: 'List', names: ['Vector' as Identifier] } },
    ],
    expected: { name: 'Vector', fullyQualified: 'Project.Foo.Vector' },
  },
  {
    description: 'Conflicting Vector (2)',
    importing: {
      kind: 'Unqualified',
      from: projectPath('Project.Foo'),
      import: 'Vector' as Identifier,
    },
    alreadyImported: [{ from: qn('Standard.Base'), imported: { kind: 'All', except: [] } }],
    expected: { name: 'Vector', fullyQualified: 'Project.Foo.Vector' },
  },
])('Conflicting imports: $description', ({ importing, alreadyImported, expected }) => {
  const db = mockDb()

  const conflicts = detectImportConflicts(
    db,
    alreadyImported.map(({ from, imported }) => ({
      from: unwrap(projectNames.parseProjectPath(from)),
      imported,
    })),
    importing,
  )
  expect(conflicts).toEqual({
    detected: true,
    pattern: qn(expected.name),
    fullyQualified: projectPath(expected.fullyQualified),
  } satisfies ConflictInfo)
})

test.each([
  {
    id: 1,
    expected: [
      {
        kind: 'Qualified',
        module: projectPath('Standard.Base'),
      },
    ],
  },
  {
    id: 2,
    expected: [
      {
        kind: 'Unqualified',
        from: projectPath('Standard.Base'),
        import: unwrap(tryIdentifier('Type')),
      },
    ],
  },
  {
    id: 3,
    expected: [
      {
        kind: 'Unqualified',
        from: projectPath('Standard.Base'),
        import: unwrap(tryIdentifier('Connections')),
      },
    ],
  },
  {
    id: 4,
    expected: [
      {
        kind: 'Unqualified',
        from: projectPath('Standard.Base'),
        import: unwrap(tryIdentifier('DB_Table')),
      },
    ],
  },
  {
    id: 5,
    expected: [
      {
        kind: 'Unqualified',
        from: projectPath('Standard.Base'),
        import: unwrap(tryIdentifier('Type')),
      },
    ],
  },
  {
    id: 6,
    expected: [
      {
        kind: 'Unqualified',
        from: projectPath('Standard.Base'),
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
        module: projectPath('Standard.Base'),
      },
    ],
  },
])('Required imports $id', ({ id, expected }) => {
  const db = mockDb()
  const entry = db.get(id)
  assertDefined(entry)
  expect(requiredImports(db, entry)).toStrictEqual(expected)
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
      from: qn('Standard.Base'),
      imported: { kind: 'All', except: [] },
    },
  },
  {
    code: 'from Standard.Base.Table import Table',
    expected: {
      from: qn('Standard.Base.Table'),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] },
    },
  },
  {
    code: 'import AWS.Connection as Backend',
    expected: {
      from: qn('AWS.Connection'),
      imported: { kind: 'Module', alias: 'Backend' },
    },
  },
  {
    code: 'import Standard.Base.Data',
    expected: {
      from: qn('Standard.Base.Data'),
      imported: { kind: 'Module' },
    },
  },
  {
    code: 'import local',
    expected: {
      from: qn('local'),
      imported: { kind: 'Module' },
    },
  },
  {
    code: 'from Standard.Base import Foo, Bar',
    expected: {
      from: qn('Standard.Base'),
      imported: {
        kind: 'List',
        names: [unwrap(tryIdentifier('Foo')), unwrap(tryIdentifier('Bar'))],
      },
    },
  },
  {
    code: 'from   Standard  . Base import  Foo ,  Bar ,Buz',
    expected: {
      from: qn('Standard.Base'),
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
      from: qn('Standard.Base'),
      imported: {
        kind: 'All',
        except: [unwrap(tryIdentifier('Foo')), unwrap(tryIdentifier('Bar'))],
      },
    },
  },
  {
    code: 'from   Standard  . Base import  all  hiding  Foo ,  Bar ,Buz',
    expected: {
      from: qn('Standard.Base'),
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

test.each<{
  import: RequiredImport
  expected: string
}>([
  {
    import: {
      kind: 'Unqualified',
      from: projectPath('Standard.Base.Table'),
      import: unwrap(tryIdentifier('Table')),
    },
    expected: 'from Standard.Base.Table import Table',
  },
  {
    import: {
      kind: 'Qualified',
      module: projectPath('Standard.Base.Data'),
    },
    expected: 'import Standard.Base.Data',
  },
  {
    import: {
      kind: 'Qualified',
      module: projectPath('local.Other_Project'),
    },
    expected: 'import local.Other_Project',
  },
])('Generating import $expected', ({ import: import_, expected }) => {
  expect(requiredImportToAst(import_, projectNames).code()).toStrictEqual(expected)
})

test('Insert after other imports in module', () => {
  const module_ = Ast.parseBlock('from Standard.Base import all\n\nmain = 42\n')
  const edit = module_.module.edit()
  addImports(
    edit.getVersion(module_),
    [{ kind: 'Qualified', module: projectPath('Standard.Visualization') }],
    projectNames,
  )
  expect(edit.getVersion(module_).code()).toBe(
    'from Standard.Base import all\nimport Standard.Visualization\n\nmain = 42\n',
  )
})

test('Insert import in module with no other imports', () => {
  const module_ = Ast.parseBlock('main = 42\n')
  const edit = module_.module.edit()
  addImports(
    edit.getVersion(module_),
    [{ kind: 'Qualified', module: projectPath('Standard.Visualization') }],
    projectNames,
  )
  expect(edit.getVersion(module_).code()).toBe('import Standard.Visualization\nmain = 42\n')
})
