import { expect, test } from 'vitest'
import { assert } from '../../util/assert'
import { parseModule } from '../parse'
import { BodyBlock, FunctionDef, TextLiteral } from '../tree'

test.each([
  {
    // (Bug #12497)
    before: 'x = """\n    \n',
    after: 'x = """\n    x\n',
  },
  {
    before: 'x = """\n    \n',
    after: 'x = """\n\n',
  },
  {
    before: 'foreign python f x = """\n    \n',
    after: 'foreign python f x = """\n    r\n',
  },
])('syncToCode', ({ before, after }) => {
  const beforeRoot = parseModule(before)
  assert(beforeRoot.code() === before)
  const codeAfter = parseModule(after).code()
  assert(codeAfter === after)
  beforeRoot.module.setRoot(beforeRoot)
  const edit = beforeRoot.module.edit()
  edit.getVersion(beforeRoot).syncToCode(after)
  const afterRoot = edit.root()
  expect(afterRoot?.code()).toBe(after)
})

test.each(['f = """\n    \n'])('Print text literal in cloned module', (code) => {
  const beforeRoot = parseModule(code)
  beforeRoot.module.setRoot(beforeRoot)
  assert(beforeRoot.code() === code)

  const edit = beforeRoot.module.edit()
  const afterRoot = edit.getVersion(beforeRoot)
  const lit = (block: BodyBlock) => {
    const stmt = [...block.statements()][0]
    assert(stmt instanceof FunctionDef)
    const expr = stmt.body
    assert(expr instanceof TextLiteral)
    return expr
  }
  const lit1 = lit(beforeRoot)
  const lit2 = lit(afterRoot)
  expect(lit2.code()).toBe(lit1.code())
  expect(afterRoot.code()).toBe(code)
})
