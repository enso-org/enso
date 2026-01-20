import { assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import { describe, expect, test } from 'vitest'
import { findExpressions, testCase, tryFindExpressions } from './testCase'

describe('Code edit', () => {
  test('Change argument type', () => {
    const beforeRoot = Ast.parseExpression('func arg1 arg2')
    assertDefined(beforeRoot)
    beforeRoot.module.setRoot(beforeRoot)
    const before = findExpressions(beforeRoot, {
      func: Ast.Ident,
      arg1: Ast.Ident,
      arg2: Ast.Ident,
      'func arg1': Ast.App,
      'func arg1 arg2': Ast.App,
    })
    const edit = beforeRoot.module.edit()
    const newCode = 'func 123 arg2'
    edit.getVersion(beforeRoot).syncToCode(newCode)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe(newCode)
    // Ensure the identities of all the original nodes were maintained.
    const after = findExpressions(edit.root()!, {
      func: Ast.Ident,
      '123': Ast.NumericLiteral,
      arg2: Ast.Ident,
      'func 123': Ast.App,
      'func 123 arg2': Ast.App,
    })
    expect(after.func.id).toBe(before.func.id)
    expect(after.arg2.id).toBe(before.arg2.id)
    expect(after['func 123'].id).toBe(before['func arg1'].id)
    expect(after['func 123 arg2'].id).toBe(before['func arg1 arg2'].id)
  })

  test('syncToCode does not create unneeded AST nodes', () => {
    const beforeRoot = Ast.parseModule('main = func 1 2\n')
    beforeRoot.module.setRoot(beforeRoot)
    const edit = beforeRoot.module.edit()
    const newCode = 'main = func 10 2\n'
    let changes: Record<string, number> | undefined = undefined
    edit.observe(
      (update) =>
        (changes = {
          added: update.nodesAdded.size,
          deleted: update.nodesDeleted.size,
          updated: update.nodesUpdated.size,
        }),
    )
    edit.syncToCode(newCode)
    expect(edit.root()?.code()).toBe(newCode)
    expect(edit.root()?.id).toBe(beforeRoot.id)
    expect(changes).toEqual({
      added: 0,
      deleted: 0,
      updated: 1,
    })
  })

  test('Insert argument names', () => {
    const beforeRoot = Ast.parseExpression('func arg1 arg2')
    assertDefined(beforeRoot)
    beforeRoot.module.setRoot(beforeRoot)
    const before = findExpressions(beforeRoot, {
      func: Ast.Ident,
      arg1: Ast.Ident,
      arg2: Ast.Ident,
      'func arg1': Ast.App,
      'func arg1 arg2': Ast.App,
    })
    const edit = beforeRoot.module.edit()
    const newCode = 'func name1=arg1 name2=arg2'
    edit.getVersion(beforeRoot).syncToCode(newCode)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe(newCode)
    // Ensure the identities of all the original nodes were maintained.
    const after = findExpressions(edit.root()!, {
      func: Ast.Ident,
      arg1: Ast.Ident,
      arg2: Ast.Ident,
      'func name1=arg1': Ast.App,
      'func name1=arg1 name2=arg2': Ast.App,
    })
    expect(after.func.id).toBe(before.func.id)
    expect(after.arg1.id).toBe(before.arg1.id)
    expect(after.arg2.id).toBe(before.arg2.id)
    expect(after['func name1=arg1'].id).toBe(before['func arg1'].id)
    expect(after['func name1=arg1 name2=arg2'].id).toBe(before['func arg1 arg2'].id)
  })

  test('Remove argument names', () => {
    const beforeRoot = Ast.parseExpression('func name1=arg1 name2=arg2')
    assertDefined(beforeRoot)
    beforeRoot.module.setRoot(beforeRoot)
    const before = findExpressions(beforeRoot, {
      func: Ast.Ident,
      arg1: Ast.Ident,
      arg2: Ast.Ident,
      'func name1=arg1': Ast.App,
      'func name1=arg1 name2=arg2': Ast.App,
    })
    const edit = beforeRoot.module.edit()
    const newCode = 'func arg1 arg2'
    edit.getVersion(beforeRoot).syncToCode(newCode)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe(newCode)
    // Ensure the identities of all the original nodes were maintained.
    const after = findExpressions(edit.root()!, {
      func: Ast.Ident,
      arg1: Ast.Ident,
      arg2: Ast.Ident,
      'func arg1': Ast.App,
      'func arg1 arg2': Ast.App,
    })
    expect(after.func.id).toBe(before.func.id)
    expect(after.arg1.id).toBe(before.arg1.id)
    expect(after.arg2.id).toBe(before.arg2.id)
    expect(after['func arg1'].id).toBe(before['func name1=arg1'].id)
    expect(after['func arg1 arg2'].id).toBe(before['func name1=arg1 name2=arg2'].id)
  })

  test('Rearrange block', () => {
    const beforeCase = testCase({
      'main =': Ast.FunctionDef,
      '    call_result = func sum 12': Ast.Assignment,
      '    sum = value + 23': Ast.Assignment,
      '    value = 42': Ast.Assignment,
    })
    const before = beforeCase.statements

    const edit = beforeCase.module.edit()
    const newCode = [
      'main =',
      '\n    value = 42',
      '\n    sum = value + 23',
      '\n    call_result = func sum 12',
    ].join('')
    edit.root()!.syncToCode(newCode)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe(newCode)
    // Ensure the identities of all the original nodes were maintained.
    const after = tryFindExpressions(edit.root()!, {
      'main =': Ast.FunctionDef,
      'call_result = func sum 12': Ast.Assignment,
      'sum = value + 23': Ast.Assignment,
      'value = 42': Ast.Assignment,
    })
    expect(after['call_result = func sum 12']?.id).toBe(before['    call_result = func sum 12'].id)
    expect(after['sum = value + 23']?.id).toBe(before['    sum = value + 23'].id)
    expect(after['value = 42']?.id).toBe(before['    value = 42'].id)
  })

  test('Rename binding', () => {
    const beforeCase = testCase({
      'main =': Ast.FunctionDef,
      '    value = 42': Ast.Assignment,
      '    sum = value + 23': Ast.Assignment,
      '    call_result = func sum 12': Ast.Assignment,
    })
    const before = beforeCase.statements

    const edit = beforeCase.module.edit()
    const newCode = [
      'main =',
      '\n    the_number = 42',
      '\n    sum = the_number + 23',
      '\n    call_result = func sum 12',
    ].join('')
    const editRoot = edit.root()
    assertDefined(editRoot)
    editRoot.syncToCode(newCode)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe(newCode)
    // Ensure the identities of all the original nodes were maintained.
    const after = tryFindExpressions(edit.root()!, {
      'main =': Ast.FunctionDef,
      'call_result = func sum 12': Ast.Assignment,
      'sum = the_number + 23': Ast.Assignment,
      'the_number = 42': Ast.Assignment,
    })
    expect(after['call_result = func sum 12']?.id).toBe(before['    call_result = func sum 12'].id)
    expect(after['sum = the_number + 23']?.id).toBe(before['    sum = value + 23'].id)
    expect(after['the_number = 42']?.id).toBe(before['    value = 42'].id)
  })

  test('Inline expression change', () => {
    const beforeRoot = Ast.parseExpression('func name1=arg1 name2=arg2')
    assertDefined(beforeRoot)
    beforeRoot.module.setRoot(beforeRoot)
    const before = findExpressions(beforeRoot, {
      func: Ast.Ident,
      arg1: Ast.Ident,
      arg2: Ast.Ident,
      'func name1=arg1': Ast.App,
      'func name1=arg1 name2=arg2': Ast.App,
    })
    const edit = beforeRoot.module.edit()
    const newArg1Code = 'arg1+1'
    edit.getVersion(before['arg1']).syncToCode(newArg1Code)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe('func name1=arg1+1 name2=arg2')
    // Ensure the identities of all the original nodes were maintained.
    const after = findExpressions(edit.root()!, {
      func: Ast.Ident,
      arg1: Ast.Ident,
      arg2: Ast.Ident,
      'arg1+1': Ast.OprApp,
      'func name1=arg1+1': Ast.App,
      'func name1=arg1+1 name2=arg2': Ast.App,
    })
    expect(after.func.id).toBe(before.func.id)
    expect(after.arg1.id).toBe(before.arg1.id)
    expect(after.arg2.id).toBe(before.arg2.id)
    expect(after['func name1=arg1+1'].id).toBe(before['func name1=arg1'].id)
    expect(after['func name1=arg1+1 name2=arg2'].id).toBe(before['func name1=arg1 name2=arg2'].id)
  })

  test('No-op statement change', () => {
    const code = 'a = 1'
    const expression = Ast.parseBlockStatement(code)
    assertDefined(expression)
    const module = expression.module
    module.setRoot(expression)
    expression.syncToCode(code)
    expect(module.root()?.code()).toBe(code)
  })

  test('No-op block change', () => {
    const code = 'main =\n    a = 1\n    b = 2\n'
    const block = Ast.parseModule(code)
    const module = block.module
    module.setRoot(block)
    block.syncToCode(code)
    expect(module.root()?.code()).toBe(code)
  })

  test('Shifting whitespace ownership', () => {
    const beforeRoot = Ast.parseModule('value = 1 +\n')
    beforeRoot.module.setRoot(beforeRoot)
    const before = findExpressions(beforeRoot, {
      value: Ast.Ident,
      '1': Ast.NumericLiteral,
      'value = 1 +': Ast.FunctionDef,
    })
    const edit = beforeRoot.module.edit()
    const newCode = 'value = 1 \n'
    edit.getVersion(beforeRoot).syncToCode(newCode)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe(newCode)
    // Ensure the identities of all the original nodes were maintained.
    const after = findExpressions(edit.root()!, {
      value: Ast.Ident,
      '1': Ast.NumericLiteral,
      'value = 1': Ast.FunctionDef,
    })
    expect(after.value.id).toBe(before.value.id)
    expect(after['1'].id).toBe(before['1'].id)
    expect(after['value = 1'].id).toBe(before['value = 1 +'].id)
  })

  test('Merging', () => {
    const block = Ast.parseModule('a = 1\nb = 2')
    const module = block.module
    module.setRoot(block)

    const editA = module.edit()
    editA.getVersion(block).syncToCode('a = 10\nb = 2')

    const editB = module.edit()
    editB.getVersion(block).syncToCode('a = 1\nb = 20')

    module.applyEdit(editA)
    module.applyEdit(editB)
    expect(module.root()?.code()).toBe('a = 10\nb = 20')
  })
})
