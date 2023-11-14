import { Ast, AstExtended } from "@/util/ast"
import { GeneralOprApp } from "@/util/ast/opr"
import { tryIdentifier, type Identifier, type QualifiedName, tryQualifiedName } from "@/util/qualifiedName"
import { unwrap } from "@/util/result"

function parseIdent(ast: AstExtended): Identifier | null {
  if (ast.isTree(Ast.Tree.Type.Ident) || ast.isToken(Ast.Token.Type.Ident)) {
    return unwrap(tryIdentifier(ast.repr()))
  } else {
    return null
  }
}

function parseIdents(ast: AstExtended): Identifier[] | null {
  if (ast.isTree(Ast.Tree.Type.Ident) || ast.isToken(Ast.Token.Type.Ident)) {
    return [unwrap(tryIdentifier(ast.repr()))]
  } else if (ast.isTree(Ast.Tree.Type.OprApp)) {
    const opr = new GeneralOprApp(ast)
    const operands = opr.operandsOfLeftAssocOprChain(',')
    const x = [...operands].flatMap((operand) => {
      if (operand && operand.type === 'ast') {
        const ident = parseIdent(operand.ast)
        return ident != null ? [ident] : []
      } else {
        return []
      }
    })
    return x
  } else {
    return null
  }
}

function recognizeImport(ast: AstExtended<Ast.Tree.Import>): Import | null {
  const from = ast.tryMap((import_) => import_.from?.body)
  const as = ast.tryMap((import_) => import_.as?.body)
  const import_ = ast.tryMap((import_) => import_.import.body)
  const all = ast.tryMap((import_) => import_.all)
  const hiding = ast.tryMap((import_) => import_.hiding?.body)
  // console.log('from: ', from ? from.repr() : 'undefined')
  // console.log('as: ', as ? as.repr() : 'undefined')
  // console.log('import_: ', import_ ? import_.repr() : 'undefined')
  // console.log('all: ', all ? all.repr() : 'undefined')
  // console.log('hiding: ', hiding ? hiding.repr() : 'undefined')
  if (all) {
    return {
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'All', except: [] }
    }
  } else if (from) {
    if (import_) {
      const names = parseIdents(import_) ?? []
      return {
        from: unwrap(tryQualifiedName(from.repr())),
        imported: { kind: 'List', names }
      }
    }
    return {
      from: unwrap(tryQualifiedName(from.repr())),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] }
    }
  } else if (import_) {
    const alias = as ? parseIdent(as) : null
    return {
      from: unwrap(tryQualifiedName(import_.repr())),
      imported: alias ? { kind: 'Module', alias } : { kind: 'Module' }
    }
  } else {
    console.error('Unrecognized import', ast.debug())
    return null
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

    expect(parseImport('from Standard.Base.Table import Table')).toStrictEqual({
      from: unwrap(tryQualifiedName('Standard.Base.Table')),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] }
    })

    expect(parseImport('from Standard.Collections import Array, HashMap')).toStrictEqual({
      from: unwrap(tryQualifiedName('Standard.Collections')),
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Array')), unwrap(tryIdentifier('HashMap'))] }
    })

    expect(parseImport('import Standard.Database')).toStrictEqual({
      from: unwrap(tryQualifiedName('Standard.Database')),
      imported: { kind: 'Module' }
    })
    expect(parseImport('import Standard.Base')).toStrictEqual({
      from: unwrap(tryQualifiedName('Standard.Base')),
      imported: { kind: 'Module' }
    })

    expect(parseImport('import AWS.Connection as Backend')).toStrictEqual({
      from: unwrap(tryQualifiedName('AWS.Connection')),
      imported: { kind: 'Module', alias: 'Backend' }
    })
  })
}
