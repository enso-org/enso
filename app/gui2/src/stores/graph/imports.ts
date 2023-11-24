import { SuggestionDb } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  makeCon,
  makeMethod,
  makeModule,
  makeStaticMethod,
  makeType,
  type SuggestionEntry,
} from '@/stores/suggestionDatabase/entry'
import { Ast, AstExtended } from '@/util/ast'
import { GeneralOprApp } from '@/util/ast/opr'
import {
  normalizeQualifiedName,
  qnFromSegments,
  qnSplit,
  tryIdentifier,
  tryQualifiedName,
  type Identifier,
  type QualifiedName,
} from '@/util/qualifiedName'
import { unwrap } from '@/util/result'

// ========================
// === Imports analysis ===
// ========================

function parseIdent(ast: AstExtended): Identifier | null {
  if (ast.isTree(Ast.Tree.Type.Ident) || ast.isToken(Ast.Token.Type.Ident)) {
    const ident = tryIdentifier(ast.repr())
    return ident.ok ? ident.value : null
  } else {
    return null
  }
}

function parseIdents(ast: AstExtended): Identifier[] | null {
  if (ast.isTree(Ast.Tree.Type.Ident) || ast.isToken(Ast.Token.Type.Ident)) {
    const ident = tryIdentifier(ast.repr())
    return ident.ok ? [ident.value] : null
  } else if (ast.isTree(Ast.Tree.Type.OprApp)) {
    const opr = new GeneralOprApp(ast)
    const operands = opr.operandsOfLeftAssocOprChain(',')
    return [...operands].flatMap((operand) => {
      if (operand && operand.type === 'ast') {
        const ident = parseIdent(operand.ast)
        return ident != null ? [ident] : []
      } else {
        return []
      }
    })
  } else {
    return null
  }
}

function parseQualifiedName(ast: AstExtended): QualifiedName | null {
  if (ast.isTree(Ast.Tree.Type.Ident) || ast.isToken(Ast.Token.Type.Ident)) {
    const name = tryQualifiedName(ast.repr())
    return name.ok ? normalizeQualifiedName(name.value) : null
  } else if (ast.isTree(Ast.Tree.Type.OprApp)) {
    const opr = new GeneralOprApp(ast)
    const operands = opr.operandsOfLeftAssocOprChain('.')
    const idents = []
    for (const operand of operands) {
      if (operand && operand.type === 'ast') {
        const ident = parseIdent(operand.ast)
        if (ident == null) {
          // One of identifiers is invalid, so is the whole qualified name.
          return null
        } else {
          idents.push(ident)
        }
      }
    }
    return normalizeQualifiedName(qnFromSegments(idents))
  } else {
    return null
  }
}

/** Parse import statement. */
export function recognizeImport(ast: AstExtended<Ast.Tree.Import>): Import | null {
  const from = ast.tryMap((import_) => import_.from?.body)
  const as = ast.tryMap((import_) => import_.as?.body)
  const import_ = ast.tryMap((import_) => import_.import.body)
  const all = ast.tryMap((import_) => import_.all)
  const hiding = ast.tryMap((import_) => import_.hiding?.body)
  const module =
    from != null ? parseQualifiedName(from) : import_ != null ? parseQualifiedName(import_) : null
  if (!module) return null
  if (all) {
    const except = (hiding != null ? parseIdents(hiding) : []) ?? []
    return {
      from: module,
      imported: { kind: 'All', except },
    }
  } else if (from && import_) {
    const names = parseIdents(import_) ?? []
    return {
      from: module,
      imported: { kind: 'List', names },
    }
  } else if (import_) {
    const alias = as ? parseIdent(as) : null
    return {
      from: module,
      imported: alias ? { kind: 'Module', alias } : { kind: 'Module' },
    }
  } else {
    console.error('Unrecognized import', ast.debug())
    return null
  }
}

export type ModuleName = QualifiedName

/** Information about parsed import statement. */
export interface Import {
  from: ModuleName
  imported: ImportedNames
}

export type ImportedNames = Module | List | All

/** import Module.Path (as Alias)? */
export interface Module {
  kind: 'Module'
  alias?: Identifier
}

/** from Module.Path import (Ident),+ */
export interface List {
  kind: 'List'
  names: Identifier[]
}

/** from Module.Path import all (hiding (Ident),*)? */
export interface All {
  kind: 'All'
  except: Identifier[]
}

// ========================
// === Required imports ===
// ========================

/** Import required for the suggestion entry. */
export type RequiredImport = QualifiedImport | UnqualifiedImport

/** import Module.Path */
export interface QualifiedImport {
  kind: 'Qualified'
  module: QualifiedName
}

/** from Module.Path import SomeIdentifier */
export interface UnqualifiedImport {
  kind: 'Unqualified'
  from: QualifiedName
  import: Identifier
}

/** Get the string representation of the required import statement. */
export function requiredImportToText(value: RequiredImport): string {
  switch (value.kind) {
    case 'Qualified':
      return `import ${value.module}`
    case 'Unqualified':
      return `from ${value.from} import ${value.import}`
  }
}

/** A list of required imports for specific suggestion entry */
export function requiredImports(db: SuggestionDb, entry: SuggestionEntry): RequiredImport[] {
  switch (entry.kind) {
    case SuggestionKind.Module:
      return entry.reexportedIn
        ? [
            {
              kind: 'Unqualified',
              from: entry.reexportedIn,
              import: entry.name,
            },
          ]
        : [
            {
              kind: 'Qualified',
              module: entry.definedIn,
            },
          ]
    case SuggestionKind.Type: {
      const from = entry.reexportedIn ? entry.reexportedIn : entry.definedIn
      return [
        {
          kind: 'Unqualified',
          from,
          import: entry.name,
        },
      ]
    }
    case SuggestionKind.Constructor: {
      const selfType = selfTypeEntry(db, entry)
      return selfType ? requiredImports(db, selfType) : []
    }
    case SuggestionKind.Method: {
      const isStatic = entry.selfType == null
      const selfType = selfTypeEntry(db, entry)
      const isExtension = selfType && selfType.definedIn !== entry.definedIn
      const definedIn = definedInEntry(db, entry)
      const extensionImports = isExtension && definedIn ? requiredImports(db, definedIn) : []
      const selfTypeImports = isStatic && selfType ? requiredImports(db, selfType) : []
      if (isStatic) {
        return [...extensionImports, ...selfTypeImports]
      } else {
        return [...extensionImports]
      }
    }
    case SuggestionKind.Function:
    case SuggestionKind.Local:
    default:
      return []
  }
}

function selfTypeEntry(db: SuggestionDb, entry: SuggestionEntry): SuggestionEntry | undefined {
  if (entry.memberOf) {
    return db.getEntryByQualifiedName(entry.memberOf)
  }
}

function definedInEntry(db: SuggestionDb, entry: SuggestionEntry): SuggestionEntry | undefined {
  return db.getEntryByQualifiedName(entry.definedIn)
}

export function requiredImportEquals(left: RequiredImport, right: RequiredImport): boolean {
  if (left.kind != right.kind) return false
  switch (left.kind) {
    case 'Qualified':
      return left.module === (right as QualifiedImport).module
    case 'Unqualified':
      return (
        left.from === (right as UnqualifiedImport).from &&
        left.import === (right as UnqualifiedImport).import
      )
  }
}

/** Check if `existing` import statement covers `required`. */
export function covers(existing: Import, required: RequiredImport): boolean {
  const [parent, name] =
    required.kind === 'Qualified'
      ? qnSplit(required.module)
      : required.kind === 'Unqualified'
      ? [required.from, required.import]
      : [undefined, '']
  const directlyImported =
    required.kind === 'Qualified' &&
    existing.imported.kind === 'Module' &&
    existing.from === required.module
  const importedInList =
    existing.imported.kind === 'List' &&
    parent != null &&
    existing.from === parent &&
    existing.imported.names.includes(name)
  const importedWithAll =
    existing.imported.kind === 'All' &&
    parent != null &&
    existing.from === parent &&
    !existing.imported.except.includes(name)
  return directlyImported || importedInList || importedWithAll
}

export function filterOutRedundantImports(
  existing: { import: Import }[],
  required: RequiredImport[],
): RequiredImport[] {
  return required.filter((info) => !existing.some((existing) => covers(existing.import, info)))
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

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
    const reexportedType = makeModule('Standard.Database.Table.Table')
    reexportedType.reexportedIn = unwrap(tryQualifiedName('Standard.Base'))
    const extensionMethod = makeMethod('Standard.Network.URI.fetch')
    extensionMethod.definedIn = unwrap(tryQualifiedName('Standard.Base'))
    db.set(1, makeModule('Standard.Base'))
    db.set(2, makeType('Standard.Base.Type'))
    db.set(3, reexportedModule)
    db.set(4, reexportedType)
    db.set(5, makeCon('Standard.Base.Type.Constructor'))
    db.set(6, makeStaticMethod('Standard.Base.Type.staticMethod'))
    db.set(7, makeMethod('Standard.Base.Type.method'))
    db.set(8, makeType('Standard.Network.URI'))
    db.set(9, extensionMethod)

    return db
  }

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
          import: unwrap(tryIdentifier('Table')),
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
    let ast = null
    AstExtended.parse(code).visitRecursive((node) => {
      if (node.isTree(Ast.Tree.Type.Import)) {
        ast = node
        return false
      }
      return true
    })
    if (ast) {
      return recognizeImport(ast)
    }
    return null
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
}
