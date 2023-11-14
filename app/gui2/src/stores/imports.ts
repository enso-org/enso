import { Ast, AstExtended } from "@/util/ast"
import { GeneralOprApp } from "@/util/ast/opr"
import { tryIdentifier, type Identifier, type QualifiedName, tryQualifiedName, fromSegments } from "@/util/qualifiedName"
import { unwrap, type Result } from "@/util/result"

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

function parseQualifiedName(ast: AstExtended): QualifiedName | null {
  if (ast.isTree(Ast.Tree.Type.Ident) || ast.isToken(Ast.Token.Type.Ident)) {
    const name = tryQualifiedName(ast.repr())
    return name.ok ? name.value : null
  } else if (ast.isTree(Ast.Tree.Type.OprApp)) {
    const opr = new GeneralOprApp(ast)
    const operands = opr.operandsOfLeftAssocOprChain('.')
    const idents = [...operands].flatMap((operand) => {
      if (operand && operand.type === 'ast') {
        const ident = parseIdent(operand.ast)
        return ident != null ? [ident] : []
      } else {
        return []
      }
    })
    return fromSegments(idents)
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
  const module = from != null ? parseQualifiedName(from) 
    : import_ != null ? parseQualifiedName(import_) : null
  if (!module) return null
  if (all) {
    return {
      from: module,
      imported: { kind: 'All', except: [] }
    }
  } else if (from) {
    if (import_) {
      const names = parseIdents(import_) ?? []
      return {
        from: module,
        imported: { kind: 'List', names }
      }
    }
    return {
      from: module,
      imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] }
    }
  } else if (import_) {
    const alias = as ? parseIdent(as) : null
    return {
      from: module,
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

  test.each([
    { code: '1 + 1', expected: null },
    {
      code: 'from Standard.Base import all',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Base')),
        imported: { kind: 'All', except: [] }
      }
    },
    {
      code: 'from Standard.Base.Table import Table',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Base.Table')),
        imported: { kind: 'List', names: [unwrap(tryIdentifier('Table'))] }
      }
    },
    {
      code: 'from Standard.Collections import Array, HashMap',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Collections')),
        imported: { kind: 'List', names: [unwrap(tryIdentifier('Array')), unwrap(tryIdentifier('HashMap'))] }
      }
    },
    {
      code: 'import Standard.Database',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Database')),
        imported: { kind: 'Module' }
      }
    },
    {
      code: 'import Standard.Base',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Base')),
        imported: { kind: 'Module' }
      }
    },
    {
      code: 'import AWS.Connection as Backend',
      expected: {
        from: unwrap(tryQualifiedName('AWS.Connection')),
        imported: { kind: 'Module', alias: 'Backend' }
      }
    },
    {
      code: 'import Standard.Base.Data',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Base.Data')),
        imported: { kind: 'Module' }
      }
    },
    {
      code: 'import local',
      expected: {
        from: unwrap(tryQualifiedName('local')),
        imported: { kind: 'Module' }
      }
    },
    {
      code: 'from Standard.Base import Foo, Bar',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Base')),
        imported: { kind: 'List', names: [unwrap(tryIdentifier('Foo')), unwrap(tryIdentifier('Bar'))] }
      }
    },
    {
      code: 'from   Standard  . Base import  Foo ,  Bar ,Buz',
      expected: {
        from: unwrap(tryQualifiedName('Standard.Base')),
        imported: { kind: 'List', names: [unwrap(tryIdentifier('Foo')), unwrap(tryIdentifier('Bar')), unwrap(tryIdentifier('Buz'))] }
      }
    },
  ])('Recognizing import $code', ({ code, expected }) => {
    expect(parseImport(code)).toStrictEqual(expected)
  })
}
