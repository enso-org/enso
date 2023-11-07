import { Ast, AstExtended } from "@/util/ast"
import { tryIdentifier, type Identifier, type QualifiedName, tryQualifiedName } from "@/util/qualifiedName"
import { unwrap } from "@/util/result"

function recognizeImport(ast: AstExtended<Ast.Tree.Import>): Import | null {
  const from = ast.tryMap((import_) => import_.from?.body)
  const as = ast.tryMap((import_) => import_.as?.body)
  const import_ = ast.tryMap((import_) => import_.import.body)
  const all = ast.tryMap((import_) => import_.all)
  const hiding = ast.tryMap((import_) => import_.hiding?.body)
  if (all) {
    return {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'All', except: [] }
    }
  } else {
    return {
      from: unwrap(tryQualifiedName('Standard.Base.Table')),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] }
    }
  }
}

type ModuleName = QualifiedName

interface Import {
  from: ModuleName,
  imported: ImportedNames,
}

type ImportedNames = Module | List | All

interface Module {
  kind: 'Module'
  alias?: Identifier
}

interface List {
  kind: 'List'
  names: Identifier[]
}

interface All {
  kind: 'All'
  except: Identifier[]
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

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


  test('Recognizing import expressions', () => {
    expect(parseImport('1 + 1')).toBeNull()
    expect(parseImport('from Standard.Base import all')).toStrictEqual({
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'All', except: [] }
    })

    expect(parseImport('import Table from Standard.Base.Table')).toStrictEqual({
      from: unwrap(tryQualifiedName('Standard.Base.Table')),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] }
    })
  })
}
