import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { expect, test } from 'vitest'
import { MutableModule } from '../abstract'

//const disabledCases = [
//  ' a',
//  'a ',
//]
const cases = [
  'Console.',
  '(',
  '[',
  ']',
  '`',
  'export',
  'export as Foo',
  'export Foo as',
  'export Foo as Bar.Baz',
  'export Foo as Foo, Bar',
  'export Foo as Foo.Bar',
  'export Foo hiding',
  'export Foo hiding X,',
  'foo = (',
  'foo = [',
  'foo = ]',
  'foo ~',
  'foo = 1, 2',
  'foo @ bar',
  'foo@bar',
  'foo = case x of\n 4 ->',
  'foo = case x of\n 4',
  'foo = case x of\n 4->',
  'foo = if cond.x else.y',
  'foreign 4',
  'foreign 4 * 4',
  'foreign foo = "4"',
  'foreign js foo = 4',
  'from export all',
  'from Foo export all hiding',
  'from Foo export all hiding X.Y',
  'from Foo import all hiding',
  'from Foo import all hiding X.Y',
  'from Foo import all What_Is_This_Doing_Here hiding Bar',
  'from import all',
  'from Standard.Base.Data.Array import new as array_new',
  'import',
  'import as Foo',
  'import Foo as',
  'import Foo as Bar.Baz',
  'import Foo as Foo, Bar',
  'import Foo as Foo.Bar',
  'import Foo hiding',
  'import Foo hiding X,',
  'main ~ = x',
  'polyglot import Foo',
  'polyglot java import',
  'splice_outside_text = `',
  'type',
  '=-',
  'x =-',
  'x = y +-',
  'x =- y',
  'x = y +- z',
  'x = +- z',
  '=- y',
  'export Foo as Bar',
  'export prj.Data.Foo',
  'foo a b c = x',
  ':',
  'a \n',
  "'''\n and some \\u000Aescapes\\'",
  'a = \n x',
  '[]',
  '.',
  '{}',
  'foo : [Integer | Text] -> (Integer | Text)',
  'from Foo export all hiding Bar, Baz',
  'from Foo export Bar, Baz',
  'from Standard.Base import all',
  'from Standard.Base import all hiding Number, Boolean',
  'from Standard.Base import Foo, Bar, Baz',
  'f (~x = 1) = x',
  'f (x = 1) = x',
  'f ~x=1 = x',
  'f x=1 = x',
  'f (x = y)',
  'f x=y',
  'import project.IO',
  'import Standard.Base as Enso_List',
  "'''\n    \\nEscape at tart\n",
  'Point x_val = my_point',
  'type Foo\n Bar (a : B =C.D)',
  'type Foo\n ## Bar\n Baz',
  'x = """\n    Indented multiline\nx',
  "x =\n x = '''\n  x\nx",
  'x + + + x',
  'x + + x',
  '.log',
  'polyglot java import java.lang.Float',
  'polyglot java import java.net.URI as Java_URI',
  '\\a b -> x',
  '\\v -> v',
  '0.0.x',
  '0b10101010',
  '0b',
  '0o122137',
  '0o',
  '0xAE2F14',
  '0x',
  '1 .0',
  '1. 0',
  '100_000',
  '10_000.99',
  '1 . 0',
  '16_17ffffffffffffffa',
  '-1.up_to 100',
  '+1 + x',
  '_+1 + x',
  '-1.x',
  '2_01101101',
  '-2_01101101',
  '-2.1',
  '(a) (b)',
  '(a b)',
  '# a b c',
  '((a b) c)',
  'a b c',
  '+ a',
  'a +',
  'a |> f',
  '@a\n@b\nx',
  '@a z\n@b\nx',
  "'''",
  '@Builtin_Type\ntype Date',
  'f : A->B -> x',
  'f:A->B -> x',
  'v : A -> _ + x',
  'v:A->x',
  'v : A -> x -> x',
  'v : A -> x->x',
  'v:A->x->x',
  'f <| a',
  'f default',
  'f default x = x',
  'foo a b =',
  'foo a b c =',
  'foo a =',
  'foo a b = x',
  'foo a = x',
  "foo = bar '''\n baz",
  'foo =',
  'foo = x',
  'foo _ = x',
  'foreign python my_method a b = "42"',
  'FREEZE x.f',
  'FREEZE x.f y',
  'FREEZE x',
  'FREEZE x + y',
  'f -x',
  'f x default = x',
  'f ~_ = x',
  'f x->\n y',
  'Id.id x = x',
  'if True then\n True',
  'if True then True else False',
  'if True then True else\n False',
  'increment = 1 +',
  'main = # define main\n 4',
  '.map (+2 * 3) *7',
  '_.map (_+2 * 3) _*7',
  "'''\n \\t'",
  '-Number.positive_infinity',
  "'''\n x\n \\t'",
  '@on_problems P.g\nTable.select_columns : Text -> Table',
  'pi = 3.14',
  'private',
  'private func',
  '(")")',
  '"Inline raw text"',
  '`',
  "('\\n')",
  '"Non-escape: \\n"',
  '"Non-escape: \\"',
  "'Other quote type'",
  "'Simple case.'",
  "'` SpliceWithLeadingWhitespace`'",
  "'String with \\' escape'",
  "'String with \\n escape'",
  '"type"',
  "'\\U0000000Aescape'",
  "'\\u{0000A}escape'",
  "'\\u000Aescape'",
  "'\\u0915\\u094D\\u0937\\u093F'",
  'unclosed = "a',
  'unclosed = "',
  "'\\u'",
  "'\\U'",
  "'With a `splice`.'",
  "'\\x0Aescape'",
  "'\\x'",
  'zero_length = ""',
  'SKIP x.f',
  'SKIP x.f y',
  'SKIP x',
  'SKIP x + y',
  '.sum 1',
  '_.sum 1',
  '@Tail_Call go\n a\n b',
  '@Tail_Call go t',
  'type A a=0',
  'type A\n @a z\n @b\n x',
  'type Bool',
  'type Existing_Headers (column_names : Vector Text)',
  'type Foo (a : Int)',
  'type Option (a)',
  'type Option a',
  'Vector _ = x',
  'x - 1 + 2',
  'x=-1',
  'x)',
  '(x',
  '(-x)',
  'x ->\n y',
  'x->\n y',
  '-x',
  'x=-x',
  '-x*x',
  '-x+x',
  '-(x * x)',
  'x = y+-',
  'x=-y',
  'x -> y',
  'x-> y',
  'x->y',
  'x y . f v',
  'x = y+-z',
  'x->y-> z',
  'x --> y ---> z',
  'x <| y <<| z',
  'x + y + z',
  'x = +-z',
  'val : Bool',
  'val = foo (x : Int)',
  'val : List Int',
  'val = x : Int',
  'w + x + y * z',
  '[x]',
  '(- x)',
  '- x',
  '{x}',
  'x : List Int -> Int',
  '(x : My_Type _)',
  '- (x * x)',
  'x - x',
  'x-x',
  '[ x , y ]',
  '[x, y]',
  'x.-y',
  'x.~y',
  '{x, y}',
  '[ x , y , z ]',
  '[x, y, z]',
  'x + y * z',
  'x * y + z',
  ["'''", ' `splice` at start'].join('\n'),
  ['case a of', '    Some -> x', '    Int -> x'].join('\n'),
  [
    'case a of',
    '    ## The Some case',
    '    Some -> x',
    '    ## The Int case',
    '    Int -> x',
  ].join('\n'),
  ['case a of', '    Vector_2d x y -> x'].join('\n'),
  ['case foo of', '    v:My_Type -> x', '    v:(My_Type _ _) -> x'].join('\n'),
  ['case self of', '    Vector_2d ... -> x'].join('\n'),
  ['case self of', '    Vector_2d -> x', '    _ -> x'].join('\n'),
  ['foo', '    bar'].join('\n'),
  ['foo =', '    ', 'bar'].join('\n'),
  ['foo =', 'bar'].join('\n'),
  ['foo', '    + bar +'].join('\n'),
  ['foo', '    + bar', '        - baz'].join('\n'),
  ['main =', '    foo', 'bar'].join('\n'),
  ['main =', '  foo', ' bar', '  baz'].join('\n'),
  ['main =', '  foo', ' bar', 'baz'].join('\n'),
  ['main ~foo = x'].join('\n'),
  ['main =', '      ', '    x'].join('\n'),
  ['main =', '    ', '    x'].join('\n'),
  ['main =', '    x'].join('\n'),
  ['main =', '  ', '    x'].join('\n'),
  ['main =', '', '    x'].join('\n'),
  ['rect1', '    . width = 7', '    . center', '        + x'].join('\n'),
  ['type A', '    Foo (a : Integer, b : Integer)'].join('\n'),
  ['type Geo', '    number =', '        x', '    area self = x + x'].join('\n'),
  ['type Result error ok=Nothing', '    Ok value:ok = Nothing'].join('\n'),
  ['value = nums', '    * each random', '    + constant'].join('\n'),
  ['# Some comment', '# Other comment', '', 'private'].join('\n'),
  ['main =', '    +x', '    print x'].join('\n'),
  [
    '## The Identity Function',
    '',
    '   Arguments:',
    '   - x: value to do nothing to',
    'id x = x',
  ].join('\n'),
  [
    'type Foo',
    '    + : Foo -> Foo -> Foo',
    '    + self b = b',
    '    Foo.+ : Foo',
    '    Foo.+ self b = b',
  ].join('\n'),
  ['type Foo', ' ## Test indent handling', '  ', ' foo'].join('\n'),
  ['type Foo', '    type Bar', '    type Baz'].join('\n'),
  [
    'type Geo',
    '    Circle',
    '        radius : float',
    '        x',
    '    Rectangle width height',
    '    Point',
    '',
    '    number =',
    '        x',
    '    area self = x + x',
  ].join('\n'),
  [
    'type Geo',
    '    Circle',
    '        radius',
    '        x',
    '    Rectangle width height',
    '    Point',
  ].join('\n'),
  [
    'type Problem_Builder',
    '    ## Returns a vector containing all reported problems, aggregated.',
    '    build_problemset : Vector',
    '    build_problemset self =',
    '        self',
  ].join('\n'),
  ['type T', '', 'private'].join('\n'),
  ['value = foo', '    bar'].join('\n'),
  ['value = foo', '    +x', '    bar'].join('\n'),
  ['###', ' x'].join('\n'),
]
test.each(cases)('parse/print round trip: %s', (code) => {
  // Get an AST.
  const root = Ast.parseBlock(code)
  // Print AST back to source.
  const printed = Ast.print(root.exprId, root.module)
  const info1 = printed.info
  expect(printed.code).toEqual(code)

  // Re-parse.
  const root1 = Ast.parseBlock(printed)
  // Check that Identities match original AST.
  const reprinted = Ast.print(root1.exprId, root1.module)
  expect(reprinted.info.nodes).toEqual(info1.nodes)
  expect(reprinted.info.tokens).toEqual(info1.tokens)
})

const parseCases = [
  { code: 'foo bar+baz', tree: ['', [['foo'], [['bar'], '+', ['baz']]]] },
  { code: '(foo)', tree: ['', ['(', ['foo'], ')']] },
]
test.each(parseCases)('parse: %s', (testCase) => {
  const root = Ast.parseBlock(testCase.code)
  expect(Ast.tokenTree(root)).toEqual(testCase.tree)
})

test('Insert new expression', () => {
  const code = 'main =\n    text1 = "foo"\n'
  const root = Ast.parseBlock(code)
  const main = Ast.functionBlock(root.module, 'main')!
  expect(main).not.toBeNull()
  const edit = root.module.edit()
  const rhs = Ast.parse('42', edit)
  const assignment = Ast.Assignment.new(edit, 'baz', rhs)
  main.push(edit, assignment)
  const printed = root.code(edit)
  expect(printed).toEqual('main =\n    text1 = "foo"\n    baz = 42\n')
})

type SimpleModule = {
  root: Ast.BodyBlock
  assignment: Ast.Assignment
}
function simpleModule(): SimpleModule {
  const code = 'main =\n    text1 = "foo"\n'
  const root = Ast.parseBlock(code)
  const main = Ast.functionBlock(root.module, 'main')!
  expect(main).not.toBeNull()
  const assignment: Ast.Assignment = main.statements().next().value
  expect(assignment).toBeInstanceOf(Ast.Assignment)
  return { root, assignment }
}

test('Modify subexpression', () => {
  const { root, assignment } = simpleModule()
  expect(assignment.expression).not.toBeNull()
  const edit = root.module.edit()
  const newValue = Ast.TextLiteral.new('bar', edit)
  expect(newValue.code()).toBe("'bar'")
  const oldExprId = assignment.expression!.exprId
  edit.replaceValue(assignment.expression!.exprId, newValue)
  expect(assignment.expression!.exprId).toBe(oldExprId)
  expect(edit.get(assignment.expression!.exprId)?.code()).toBe("'bar'")
  const printed = root.code(edit)
  expect(printed).toEqual("main =\n    text1 = 'bar'\n")
})

test('Replace subexpression', () => {
  const { root, assignment } = simpleModule()
  expect(assignment.expression).not.toBeNull()
  const edit = root.module.edit()
  const newValue = Ast.TextLiteral.new('bar', edit)
  expect(newValue.code()).toBe("'bar'")
  edit.replaceRef(assignment.expression!.exprId, newValue)
  const assignment_ = edit.get(assignment.exprId)!
  assert(assignment_ instanceof Ast.Assignment)
  expect(assignment_.expression!.exprId).toBe(newValue.exprId)
  expect(edit.get(assignment_.expression!.exprId)?.code()).toBe("'bar'")
  const printed = root.code(edit)
  expect(printed).toEqual("main =\n    text1 = 'bar'\n")
})

test('Change ID of node', () => {
  const { root, assignment } = simpleModule()
  expect(assignment.expression).not.toBeNull()
  const edit = root.module.edit()
  const expression = edit.takeValue(assignment.expression!.exprId)!
  expect(expression.code()).toBe('"foo"')
  edit.replaceRef(assignment.expression!.exprId, expression)
  const assignment_ = edit.get(assignment.exprId)!
  assert(assignment_ instanceof Ast.Assignment)
  expect(assignment_.expression!.exprId).not.toBe(assignment.expression!.exprId)
  expect(edit.get(assignment_.expression!.exprId)?.code()).toBe('"foo"')
  const printed = root.code(edit)
  expect(printed).toEqual('main =\n    text1 = "foo"\n')
})

test('Delete expression', () => {
  const originalCode = 'main =\n    text1 = "foo"\n    text2 = "bar"\n'
  const root = Ast.parseBlock(originalCode)
  const main = Ast.functionBlock(root.module, 'main')!
  expect(main).not.toBeNull()
  const iter = main.statements()
  const _assignment1 = iter.next()
  const assignment2: Ast.Assignment = iter.next().value
  const edit = root.module.edit()
  edit.delete(assignment2.exprId)
  const printed = root.code(edit)
  expect(printed).toEqual('main =\n    text1 = "foo"\n')
})

test('Block lines interface', () => {
  const block = Ast.parseBlock('VLE  \nSISI\nGNIK \n')
  // Sort alphabetically, but keep the blank line at the end.
  const reordered = block.lines().sort((a, b) => {
    if (a.expression?.node.code() === b.expression?.node.code()) return 0
    if (!a.expression) return 1
    if (!b.expression) return -1
    return a.expression.node.code() < b.expression.node.code() ? -1 : 1
  })
  const newBlock = Ast.BodyBlock.new(reordered)
  // Note that trailing whitespace belongs to the following line.
  expect(newBlock.code()).toBe('GNIK  \nSISI\nVLE \n')
})

test('Splice', () => {
  const module = MutableModule.Observable()
  const edit = module.edit()
  const ident = Ast.Ident.new(edit, 'foo')
  expect(ident.code()).toBe('foo')
  const spliced = module.splice(ident)
  expect(spliced.module).toBe(module)
  expect(spliced.code()).toBe('foo')
})
