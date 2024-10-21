import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import {
  MutableModule,
  TextLiteral,
  escapeTextLiteral,
  findModuleMethod,
  substituteIdentifier,
  substituteQualifiedName,
  subtrees,
  tryEnsoToNumber,
  tryNumberToEnso,
  unescapeTextLiteral,
  type Identifier,
} from '@/util/ast/abstract'
import { fc, test } from '@fast-check/vitest'
import { describe, expect } from 'vitest'
import { findExpressions, testCase, tryFindExpressions } from './testCase'

test('Raw block abstracts to Ast.BodyBlock', () => {
  const code = 'value = 2 + 2'
  const rawBlock = Ast.rawParseModule(code)
  const edit = MutableModule.Transient()
  const abstracted = Ast.abstract(edit, rawBlock, code)
  expect(abstracted.root).toBeInstanceOf(Ast.BodyBlock)
})

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
  'private foo = 23',
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
  'type Foo\n private Bar',
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
  '[x ,y ,z]',
  '[,,,,,]',
  '[]',
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
  '\n',
  '\n\n',
  '\na',
  '\n\na',
]
test.each(cases)('parse/print round trip: %s', (code) => {
  // Get an AST.
  const { root } = Ast.parseModuleWithSpans(code)
  // Print AST back to source.
  const printed = Ast.print(root)
  expect(printed.code).toEqual(code)
  // Loading token IDs from IdMaps is not implemented yet, fix during sync.
  printed.info.tokens.clear()
  const idMap = Ast.spanMapToIdMap(printed.info)
  idMap.validate()

  // Parsed tree shouldn't need any repair.
  expect(Ast.repair(root).fixes).toBe(undefined)

  // Re-parse.
  const { root: root1, spans: spans1 } = Ast.parseModuleWithSpans(printed.code)
  Ast.setExternalIds(root1.module, spans1, idMap)
  // Check that Identities match original AST.
  const printed1 = Ast.print(root1)
  printed1.info.tokens.clear()
  const idMap1 = Ast.spanMapToIdMap(printed1.info)
  const mapsEqual = idMap1.isEqual(idMap)
  if (!mapsEqual) idMap1.compare(idMap)
  expect(mapsEqual).toBe(true)
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
  const main = Ast.functionBlock(root, 'main')!
  expect(main).toBeDefined()
  const edit = root.module.edit()
  const rhs = Ast.parse('42', edit)
  const assignment = Ast.Assignment.new(edit, 'baz' as Identifier, rhs)
  edit.getVersion(main).push(assignment)
  const printed = edit.getVersion(root).code()
  expect(printed).toEqual('main =\n    text1 = "foo"\n    baz = 42\n')
})

type SimpleModule = {
  root: Ast.BodyBlock
  main: Ast.Function
  mainBlock: Ast.BodyBlock
  assignment: Ast.Assignment
}
function simpleModule(): SimpleModule {
  const code = 'main =\n    text1 = "foo"\n'
  const root = Ast.parseBlock(code)
  const main = findModuleMethod(root, 'main')!
  const mainBlock = main.body instanceof Ast.BodyBlock ? main.body : null
  assert(mainBlock != null)
  expect(mainBlock).toBeInstanceOf(Ast.BodyBlock)
  const assignment: Ast.Assignment = mainBlock.statements().next().value
  expect(assignment).toBeInstanceOf(Ast.Assignment)
  return { root, main, mainBlock, assignment }
}

test('Subtrees', () => {
  const { root, main, mainBlock, assignment } = simpleModule()
  const module = root.module
  expect(subtrees(module, [assignment.id])).toEqual(
    new Set([assignment.id, mainBlock.id, main.id, root.id]),
  )
  expect(subtrees(module, [mainBlock.id])).toEqual(new Set([mainBlock.id, main.id, root.id]))
  expect(subtrees(module, [main.id])).toEqual(new Set([main.id, root.id]))
  expect(subtrees(module, [root.id])).toEqual(new Set([root.id]))
})

test('Modify subexpression', () => {
  const { root, assignment } = simpleModule()
  expect(assignment.expression).not.toBeNull()
  const edit = root.module.edit()
  const newValue = Ast.TextLiteral.new('bar', edit)
  expect(newValue.code()).toBe("'bar'")
  const oldExprId = assignment.expression!.externalId
  const assignment_ = edit.getVersion(assignment)
  assignment_.expression.replaceValue(newValue)
  expect(assignment_.expression?.externalId).toBe(oldExprId)
  expect(assignment_.expression?.code()).toBe("'bar'")
  const printed = edit.getVersion(root).code()
  expect(printed).toEqual("main =\n    text1 = 'bar'\n")
})

test('Replace subexpression', () => {
  const { root, assignment } = simpleModule()
  expect(assignment.expression).not.toBeNull()
  const edit = root.module.edit()
  const newValue = Ast.TextLiteral.new('bar', edit)
  expect(newValue.code()).toBe("'bar'")
  edit.replace(assignment.expression!.id, newValue)
  const assignment_ = edit.tryGet(assignment.id)!
  assert(assignment_ instanceof Ast.Assignment)
  expect(assignment_.expression!.id).toBe(newValue.id)
  expect(edit.tryGet(assignment_.expression!.id)?.code()).toBe("'bar'")
  const printed = edit.getVersion(root).code()
  expect(printed).toEqual("main =\n    text1 = 'bar'\n")
})

test('Modify subexpression - setting a vector', () => {
  // A case where the #9357 bug was visible.
  const code = 'main =\n    text1 = foo\n'
  const root = Ast.parseBlock(code)
  const main = Ast.functionBlock(root, 'main')!
  expect(main).not.toBeNull()
  const assignment: Ast.Assignment = main.statements().next().value
  expect(assignment).toBeInstanceOf(Ast.Assignment)

  const edit = root.module.edit()
  const transientModule = MutableModule.Transient()
  const newValue = Ast.Vector.new(transientModule, [Ast.parse('bar')])
  expect(newValue.code()).toBe('[bar]')
  edit.replaceValue(assignment.expression.id, newValue)
  const printed = edit.getVersion(root).code()
  expect(printed).toEqual('main =\n    text1 = [bar]\n')
})

test('Change ID of node', () => {
  const { root, assignment } = simpleModule()
  expect(assignment.expression).not.toBeNull()
  const edit = root.module.edit()
  const oldExternalId = assignment.expression.externalId
  const assignment_ = edit.getVersion(assignment)
  const expression = assignment_.expression.takeValue().node
  expect(expression.code()).toBe('"foo"')
  assignment_.expression?.replace(expression)
  expect(assignment_.expression?.externalId).not.toBe(oldExternalId)
  expect(assignment_.expression?.code()).toBe('"foo"')
  const printed = edit.getVersion(root).code()
  expect(printed).toEqual('main =\n    text1 = "foo"\n')
})

test('Block lines interface', () => {
  const block = Ast.parseBlock('VLE  \nSISI\nGNIK \n')
  // Sort alphabetically, but keep the blank line at the end.
  const reordered = block.takeLines().sort((a, b) => {
    if (a.expression?.node.code() === b.expression?.node.code()) return 0
    if (!a.expression) return 1
    if (!b.expression) return -1
    return a.expression.node.code() < b.expression.node.code() ? -1 : 1
  })
  const edit = block.module.edit()
  const newBlock = Ast.BodyBlock.new(reordered, edit)
  // Note that trailing whitespace belongs to the following line.
  expect(newBlock.code()).toBe('GNIK  \nSISI\nVLE \n')
})

test('Splice', () => {
  const module = MutableModule.Transient()
  const edit = module.edit()
  const ident = Ast.Ident.new(edit, 'foo' as Identifier)
  expect(ident.code()).toBe('foo')
  const spliced = module.copyIfForeign(ident)
  expect(spliced.module).toBe(module)
  expect(spliced.code()).toBe('foo')
})

test('Construct app', () => {
  const edit = MutableModule.Transient()
  const app = Ast.App.new(
    edit,
    Ast.Ident.new(edit, 'func' as Identifier),
    undefined,
    Ast.Ident.new(edit, 'arg' as Identifier),
  )
  expect(app.code()).toBe('func arg')
  const namedApp = Ast.App.new(
    edit,
    Ast.Ident.new(edit, 'func' as Identifier),
    'name' as Identifier,
    Ast.Ident.new(edit, 'arg' as Identifier),
  )
  expect(namedApp.code()).toBe('func name=arg')
})

test('Automatic parenthesis', () => {
  const block = Ast.parseBlock('main = func arg1 arg2')
  let arg1: Ast.MutableAst | undefined
  block.visitRecursiveAst((ast) => {
    if (ast instanceof Ast.MutableIdent && ast.code() === 'arg1') {
      assert(!arg1)
      arg1 = ast
    }
  })
  assert(arg1 != null)
  arg1.replace(Ast.parse('innerfunc innerarg', block.module))
  const correctCode = 'main = func (innerfunc innerarg) arg2'
  // This assertion will fail when smart printing handles this case.
  // At that point we should test tree repair separately.
  assert(block.code() !== correctCode)
  Ast.repair(block, block.module)
  expect(block.code()).toBe(correctCode)
})

test('Tree repair: Non-canonical block line attribution', () => {
  const beforeCase = testCase({
    'func a b =': Ast.Function,
    '    c = a + b': Ast.Assignment,
    'main =': Ast.Function,
    '    func arg1 arg2': Ast.App,
  })
  const before = beforeCase.statements

  const edit = beforeCase.module.edit()
  // Add a trailing line to the function's block. This is syntactically non-canonical; it should belong to the parent.
  edit.getVersion(before['func a b =']).bodyAsBlock().insert(1, undefined)
  const editedRoot = edit.root()
  assert(editedRoot instanceof Ast.BodyBlock)
  const editedCode = editedRoot.code()
  expect(editedCode).toContain('\n\n')

  const repair = edit.edit()
  Ast.repair(editedRoot, repair)
  const afterRepair = findExpressions(repair.root()!, {
    'func a b =': Ast.Function,
    'c = a + b': Ast.Assignment,
    'main =': Ast.Function,
    'func arg1 arg2': Ast.App,
  })
  const repairedFunc = afterRepair['func a b =']
  assert(repairedFunc.body instanceof Ast.BodyBlock)
  // The function's body has been corrected.
  expect(repairedFunc.body.lines.length).toBe(1)
  expect(repair.root()?.code()).toBe(editedCode)
  // The repair maintains identities in all nodes.
  expect(afterRepair['c = a + b'].id).toBe(before['    c = a + b'].id)
  expect(afterRepair['func arg1 arg2'].id).toBe(before['    func arg1 arg2'].id)
  // The repair maintains identities of other functions.
  expect(afterRepair['main ='].id).toBe(before['main ='].id)
})

describe('Code edit', () => {
  test('Change argument type', () => {
    const beforeRoot = Ast.parse('func arg1 arg2')
    beforeRoot.module.replaceRoot(beforeRoot)
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

  test('Insert argument names', () => {
    const beforeRoot = Ast.parse('func arg1 arg2')
    beforeRoot.module.replaceRoot(beforeRoot)
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
    const beforeRoot = Ast.parse('func name1=arg1 name2=arg2')
    beforeRoot.module.replaceRoot(beforeRoot)
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
      'main =': Ast.Function,
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
      'main =': Ast.Function,
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
      'main =': Ast.Function,
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
    edit.root()!.syncToCode(newCode)
    // Ensure the change was made.
    expect(edit.root()?.code()).toBe(newCode)
    // Ensure the identities of all the original nodes were maintained.
    const after = tryFindExpressions(edit.root()!, {
      'main =': Ast.Function,
      'call_result = func sum 12': Ast.Assignment,
      'sum = the_number + 23': Ast.Assignment,
      'the_number = 42': Ast.Assignment,
    })
    expect(after['call_result = func sum 12']?.id).toBe(before['    call_result = func sum 12'].id)
    expect(after['sum = the_number + 23']?.id).toBe(before['    sum = value + 23'].id)
    expect(after['the_number = 42']?.id).toBe(before['    value = 42'].id)
  })

  test('Inline expression change', () => {
    const beforeRoot = Ast.parse('func name1=arg1 name2=arg2')
    beforeRoot.module.replaceRoot(beforeRoot)
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

  test('No-op inline expression change', () => {
    const code = 'a = 1'
    const expression = Ast.parse(code)
    const module = expression.module
    module.replaceRoot(expression)
    expression.syncToCode(code)
    expect(module.root()?.code()).toBe(code)
  })

  test('No-op block change', () => {
    const code = 'a = 1\nb = 2\n'
    const block = Ast.parseBlock(code)
    const module = block.module
    module.replaceRoot(block)
    block.syncToCode(code)
    expect(module.root()?.code()).toBe(code)
  })

  test('Shifting whitespace ownership', () => {
    const beforeRoot = Ast.parseModuleWithSpans('value = 1 +\n').root
    beforeRoot.module.replaceRoot(beforeRoot)
    const before = findExpressions(beforeRoot, {
      value: Ast.Ident,
      '1': Ast.NumericLiteral,
      'value = 1 +': Ast.Function,
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
      'value = 1': Ast.Function,
    })
    expect(after.value.id).toBe(before.value.id)
    expect(after['1'].id).toBe(before['1'].id)
    expect(after['value = 1'].id).toBe(before['value = 1 +'].id)
  })

  test('merging', () => {
    const block = Ast.parseModuleWithSpans('a = 1\nb = 2').root
    const module = block.module
    module.replaceRoot(block)

    const editA = module.edit()
    editA.getVersion(block).syncToCode('a = 10\nb = 2')

    const editB = module.edit()
    editB.getVersion(block).syncToCode('a = 1\nb = 20')

    module.applyEdit(editA)
    module.applyEdit(editB)
    expect(module.root()?.code()).toBe('a = 10\nb = 20')
  })
})

test('Analyze app-like', () => {
  const appLike = Ast.parse('(Preprocessor.default_preprocessor 3 _ 5 _ <| 4) <| 6')
  const { func, args } = Ast.analyzeAppLike(appLike)
  expect(func.code()).toBe('Preprocessor.default_preprocessor')
  expect(args.map((ast) => ast.code())).toEqual(['3', '4', '5', '6'])
})

test.each([
  {
    original: 'Vector.new',
    pattern: 'Vector.new',
    substitution: 'Standard.Base.Vector.new',
    expected: 'Standard.Base.Vector.new',
  },
  {
    original: 'x = Table.from_vec (Vector.new 1 2 3)',
    pattern: 'Vector.new',
    substitution: 'NotReallyVector.create',
    expected: 'x = Table.from_vec (NotReallyVector.create 1 2 3)',
  },
  {
    original: 'x',
    pattern: 'x',
    substitution: 'y',
    expected: 'y',
  },
  {
    original: 'x + y',
    pattern: 'x',
    substitution: 'z',
    expected: 'z + y',
  },
  {
    original: 'Data.Table.new',
    pattern: 'Data',
    substitution: 'Project.Foo.Data',
    expected: 'Project.Foo.Data.Table.new',
  },
  {
    original: 'Data.Table.new',
    pattern: 'Table',
    substitution: 'ShouldNotWork',
    expected: 'Data.Table.new',
  },
])(
  'Substitute qualified name $pattern inside $original',
  ({ original, pattern, substitution, expected }) => {
    const expression = Ast.parse(original)
    const module = expression.module
    module.replaceRoot(expression)
    const edit = expression.module.edit()
    substituteQualifiedName(expression, pattern as Ast.Identifier, substitution as Ast.Identifier)
    module.applyEdit(edit)
    expect(module.root()?.code()).toEqual(expected)
  },
)

test.each([
  {
    original: 'some_name',
    pattern: 'some_name',
    substitution: 'other_name',
    expected: 'other_name',
  },
  {
    original: 'x = Table.from_vec (node1.new 1 2 3)',
    pattern: 'node1',
    substitution: 'node2',
    expected: 'x = Table.from_vec (node2.new 1 2 3)',
  },
  {
    original: 'x = some_func "node1"',
    pattern: 'node1',
    substitution: 'node2',
    expected: 'x = some_func "node1"',
  },
  {
    original: 'x + y',
    pattern: 'x',
    substitution: 'z',
    expected: 'z + y',
  },
  {
    original: 'node1.node2.node3',
    pattern: 'node2',
    substitution: 'ShouldNotWork',
    expected: 'node1.node2.node3',
  },
  {
    original: 'node1.node2.node3',
    pattern: 'node3',
    substitution: 'ShouldNotWork',
    expected: 'node1.node2.node3',
  },
  {
    original: '.node1',
    pattern: 'node1',
    substitution: 'ShouldNotWork',
    expected: '.node1',
  },
])(
  'Substitute identifier $pattern inside $original',
  ({ original, pattern, substitution, expected }) => {
    const expression = Ast.parse(original)
    const module = expression.module
    module.replaceRoot(expression)
    const edit = expression.module.edit()
    substituteIdentifier(expression, pattern as Ast.Identifier, substitution as Ast.Identifier)
    module.applyEdit(edit)
    expect(module.root()?.code()).toEqual(expected)
  },
)

test.each([
  ['', ''],
  ['\\x20', ' ', ' '],
  ['\\b', '\b'],
  ['abcdef_123', 'abcdef_123'],
  ["\\t\\r\\n\\v\\'\\`", "\t\r\n\v'`", "\\t\\r\\n\\v\\'\\`"],
  // Escaping a double quote is allowed, but not necessary.
  ['\\"', '"', '"'],
  // Undefined/malformed escape sequences are left unevaluated, and properly escaped when normalized.
  ['\\q\\u', '\\q\\u', '\\\\q\\\\u'],
  ['\\u00B6\\u{20}\\U\\u{D8\\xBFF}', '\xB6 \\U\xD8\xBFF}', '\xB6 \\\\U\xD8\xBFF}'],
  ['\\`foo\\` \\`bar\\` \\`baz\\`', '`foo` `bar` `baz`'],
  // Enso source code must be valid UTF-8 (per the specification), so Unicode unpaired surrogates must be escaped.
  ['\\uDEAD', '\uDEAD', '\\u{dead}'],
])(
  'Applying and escaping text literal interpolation',
  (escapedText: string, rawText: string, normalizedEscapedText?: string) => {
    if (normalizedEscapedText != null) {
      // If `normalizedEscapedText` is provided, it must be a representation of the same raw value as `escapedText`.
      const rawTextFromNormalizedInput = unescapeTextLiteral(normalizedEscapedText)
      expect(rawTextFromNormalizedInput).toBe(rawText)
    }
    const actualApplied = unescapeTextLiteral(escapedText)
    const actualEscaped = escapeTextLiteral(rawText)
    expect(actualEscaped).toBe(normalizedEscapedText ?? escapedText)
    expect(actualApplied).toBe(rawText)
  },
)

const sometimesUnicodeString = fc.oneof(fc.string(), fc.unicodeString())

test.prop({ rawText: sometimesUnicodeString })('Text interpolation roundtrip', ({ rawText }) => {
  expect(unescapeTextLiteral(escapeTextLiteral(rawText))).toBe(rawText)
})

test.prop({ rawText: sometimesUnicodeString })('AST text literal new', ({ rawText }) => {
  const literal = TextLiteral.new(rawText)
  expect(literal.rawTextContent).toBe(rawText)
})

test.prop({
  boundary: fc.constantFrom('"', "'"),
  rawText: sometimesUnicodeString,
})('AST text literal rawTextContent', ({ boundary, rawText }) => {
  const literal = TextLiteral.new('')
  literal.setBoundaries(boundary)
  literal.setRawTextContent(rawText)
  expect(literal.rawTextContent).toBe(rawText)
  const codeAsInterpolated = `'${escapeTextLiteral(rawText)}'`
  if (boundary === "'") {
    expect(literal.code()).toBe(codeAsInterpolated)
  } else {
    const codeAsRaw = `"${rawText}"`
    // Uninterpolated text will be promoted to interpolated if necessary to escape a special character.
    expect([codeAsInterpolated, codeAsRaw]).toContainEqual(literal.code())
  }
})

test('setRawTextContent promotes single-line uninterpolated text to interpolated if a newline is added', () => {
  const literal = TextLiteral.new('')
  literal.setBoundaries('"')
  const rawText = '\n'
  literal.setRawTextContent(rawText)
  expect(literal.rawTextContent).toBe(rawText)
  expect(literal.code()).toBe(`'${escapeTextLiteral(rawText)}'`)
})

const docEditCases = [
  { code: '## Simple\nnode', documentation: 'Simple' },
  {
    code: '## Preferred indent\n   2nd line\n   3rd line\nnode',
    documentation: 'Preferred indent\n2nd line\n3rd line',
  },
  {
    code: '## Extra-indented child\n 2nd line\n   3rd line\nnode',
    documentation: 'Extra-indented child\n2nd line\n3rd line',
    normalized: '## Extra-indented child\n   2nd line\n   3rd line\nnode',
  },
  {
    code: '## Extra-indented child, beyond 4th column\n 2nd line\n        3rd line\nnode',
    documentation: 'Extra-indented child, beyond 4th column\n2nd line\n    3rd line',
    normalized: '## Extra-indented child, beyond 4th column\n   2nd line\n       3rd line\nnode',
  },
  {
    code: '##Preferred indent, no initial space\n  2nd line\n  3rd line\nnode',
    documentation: 'Preferred indent, no initial space\n2nd line\n3rd line',
    normalized: '## Preferred indent, no initial space\n   2nd line\n   3rd line\nnode',
  },
  {
    code: '## Minimum indent\n 2nd line\n 3rd line\nnode',
    documentation: 'Minimum indent\n2nd line\n3rd line',
    normalized: '## Minimum indent\n   2nd line\n   3rd line\nnode',
  },
]
test.each(docEditCases)('Documentation edit round trip: $code', (docCase) => {
  const { code, documentation } = docCase
  const parsed = Ast.Documented.tryParse(code)
  assert(parsed != null)
  const parsedDocumentation = parsed.documentation()
  expect(parsedDocumentation).toBe(documentation)
  const edited = MutableModule.Transient().copy(parsed)
  edited.setDocumentationText(parsedDocumentation)
  expect(edited.code()).toBe(docCase.normalized ?? code)
})

test.each([
  '## Some documentation\nf x = 123',
  '## Some documentation\n    and a second line\nf x = 123',
  '## Some documentation## Another documentation??\nf x = 123',
])('Finding documentation: $code', (code) => {
  const block = Ast.parseBlock(code)
  const method = Ast.findModuleMethod(block, 'f')
  assertDefined(method)
  expect(method.documentingAncestor()).toBeDefined()
})

test.each([
  {
    code: '## Already documented\nf x = 123',
    expected: '## Already documented\nf x = 123',
  },
  {
    code: 'f x = 123',
    expected: '## \nf x = 123',
  },
])('Adding documentation: $code', ({ code, expected }) => {
  const block = Ast.parseBlock(code)
  const module = block.module
  const method = module.getVersion(Ast.findModuleMethod(block, 'f')!)
  method.getOrInitDocumentation()
  expect(block.code()).toBe(expected)
})

test('Creating comments', () => {
  const expr = Ast.parse('2 + 2')
  expr.module.replaceRoot(expr)
  expr.update((expr) => Ast.Documented.new('Calculate five', expr))
  expect(expr.module.root()?.code()).toBe('## Calculate five\n2 + 2')
})

test.each([
  { code: 'operator1', expected: { subject: 'operator1', accesses: [] } },
  { code: 'operator1 foo bar', expected: { subject: 'operator1 foo bar', accesses: [] } },
  { code: 'operator1.parse_json', expected: { subject: 'operator1', accesses: ['parse_json'] } },
  {
    code: 'operator1.parse_json operator2.to_json',
    expected: { subject: 'operator1.parse_json operator2.to_json', accesses: [] },
  },
  {
    code: 'operator1.parse_json foo bar',
    expected: { subject: 'operator1.parse_json foo bar', accesses: [] },
  },
  {
    code: 'operator1.parse_json.length',
    expected: { subject: 'operator1', accesses: ['parse_json', 'length'] },
  },
  {
    code: 'operator1.parse_json.length foo bar',
    expected: { subject: 'operator1.parse_json.length foo bar', accesses: [] },
  },
  { code: 'operator1 + operator2', expected: { subject: 'operator1 + operator2', accesses: [] } },
])('Access chain in $code', ({ code, expected }) => {
  const ast = Ast.parse(code)
  const { subject, accessChain } = Ast.accessChain(ast)
  expect({
    subject: subject.code(),
    accesses: accessChain.map((ast) => ast.rhs.code()),
  }).toEqual(expected)
})

test.each`
  initial     | pushed     | expected
  ${'[1, 2]'} | ${'"Foo"'} | ${'[1, 2, "Foo"]'}
  ${'[]'}     | ${'3'}     | ${'[3]'}
  ${'[,]'}    | ${'1'}     | ${'[,, 1]'}
`('Pushing $pushed to vector $initial', ({ initial, pushed, expected }) => {
  const vector = Ast.Vector.tryParse(initial)
  assertDefined(vector)
  vector.push(Ast.parse(pushed, vector.module))
  expect(vector.code()).toBe(expected)
})

test.each`
  initial            | predicate                                                 | expected
  ${'[1, 2, "Foo"]'} | ${(ast: Ast.Ast) => ast instanceof Ast.NumericLiteral}    | ${'[1, 2]'}
  ${'[1, "Foo", 3]'} | ${(ast: Ast.Ast) => ast instanceof Ast.NumericLiteral}    | ${'[1, 3]'}
  ${'["Foo", 2, 3]'} | ${(ast: Ast.Ast) => ast instanceof Ast.NumericLiteral}    | ${'[2, 3]'}
  ${'[1, 2, "Foo"]'} | ${(ast: Ast.Ast) => !(ast instanceof Ast.NumericLiteral)} | ${'["Foo"]'}
  ${'[1, "Foo", 3]'} | ${(ast: Ast.Ast) => !(ast instanceof Ast.NumericLiteral)} | ${'["Foo"]'}
  ${'["Foo", 2, 3]'} | ${(ast: Ast.Ast) => !(ast instanceof Ast.NumericLiteral)} | ${'["Foo"]'}
  ${'[]'}            | ${(ast: Ast.Ast) => ast instanceof Ast.NumericLiteral}    | ${'[]'}
  ${'[1, 2, 3]'}     | ${(ast: Ast.Ast) => ast.code() != '4'}                    | ${'[1, 2, 3]'}
  ${'[1, 2, 3]'}     | ${() => false}                                            | ${'[]'}
`('Keeping elements in vector ($initial -> $expected)', ({ initial, predicate, expected }) => {
  const vector = Ast.Vector.tryParse(initial)
  assertDefined(vector)
  vector.keep(predicate)
  expect(vector.code()).toBe(expected)
})

test.each`
  initial        | expectedVector | expectedValue
  ${'[1, 2, 3]'} | ${'[1, 2]'}    | ${'3'}
  ${'[1, 2, ]'}  | ${'[1, 2 ]'}   | ${undefined}
  ${'[]'}        | ${'[]'}        | ${undefined}
`('Popping elements from vector $initial', ({ initial, expectedVector, expectedValue }) => {
  const vector = Ast.Vector.tryParse(initial)
  assertDefined(vector)
  const value = vector.pop()
  expect(value?.code()).toBe(expectedValue)
  expect(vector.code()).toBe(expectedVector)
})

test.each`
  initial        | start | deletedCount | expectedVector
  ${'[1, 2, 3]'} | ${1}  | ${1}         | ${'[1, 3]'}
  ${'[1, 2, 3]'} | ${0}  | ${1}         | ${'[2, 3]'}
  ${'[1, 2, 3]'} | ${0}  | ${2}         | ${'[3]'}
  ${'[1, 2, 3]'} | ${0}  | ${3}         | ${'[]'}
  ${'[3]'}       | ${0}  | ${1}         | ${'[]'}
  ${'[1, 2, 3]'} | ${2}  | ${1}         | ${'[1, 2]'}
`('Splicing elements from vector $initial', ({ initial, start, deletedCount, expectedVector }) => {
  const vector = Ast.Vector.tryParse(initial)
  assertDefined(vector)
  vector.splice(start, deletedCount)
  expect(vector.code()).toBe(expectedVector)
})

test.each`
  initial        | fromIndex | toIndex | expectedVector
  ${'[1, 2, 3]'} | ${0}      | ${1}    | ${'[2, 1, 3]'}
  ${'[1, 2, 3]'} | ${2}      | ${0}    | ${'[3, 1, 2]'}
  ${'[1, 2, 3]'} | ${1}      | ${3}    | ${'[1, 3, 2]'}
  ${'[1, 2, 3]'} | ${3}      | ${0}    | ${'[1, 2, 3]'}
  ${'[]'}        | ${0}      | ${0}    | ${'[]'}
`(
  'Moving element in vector $initial -> $expectedVector',
  ({ initial, fromIndex, toIndex, expectedVector }) => {
    const vector = Ast.Vector.tryParse(initial)
    assertDefined(vector)
    vector.move(fromIndex, toIndex)
    expect(vector.code()).toBe(expectedVector)
  },
)

test.each`
  initial        | index | value  | expected
  ${'[1, 2, 3]'} | ${0}  | ${'4'} | ${'[4, 2, 3]'}
  ${'[1, 2, 3]'} | ${1}  | ${'4'} | ${'[1, 4, 3]'}
  ${'[1, 2, 3]'} | ${2}  | ${'4'} | ${'[1, 2, 4]'}
  ${'[,,]'}      | ${0}  | ${'4'} | ${'[4,,]'}
  ${'[,,]'}      | ${1}  | ${'4'} | ${'[, 4,]'}
  ${'[,,]'}      | ${2}  | ${'4'} | ${'[,, 4]'}
`(
  'Setting vector elements: in $initial on index $index to $value',
  ({ initial, index, value, expected }) => {
    const vector = Ast.Vector.tryParse(initial)
    assertDefined(vector)
    vector.set(index, Ast.parse(value, vector.module))
    expect(vector.code()).toBe(expected)
  },
)

test.each`
  ensoNumber               | jsNumber                                                  | expectedEnsoNumber
  ${'0'}                   | ${0}                                                      | ${'0'}
  ${'12345'}               | ${12345}                                                  | ${'12345'}
  ${'123_456'}             | ${123456}                                                 | ${'123456'}
  ${'-12345'}              | ${-12345}                                                 | ${'-12345'}
  ${'-123_456'}            | ${-123456}                                                | ${'-123456'}
  ${'0b101'}               | ${0b101}                                                  | ${'5'}
  ${'0o444'}               | ${0o444}                                                  | ${'292'}
  ${'0xabcdef'}            | ${0xabcdef}                                               | ${'11259375'}
  ${`1${'0'.repeat(300)}`} | ${1e300}                                                  | ${undefined /*Not yet implemented*/}
  ${`1${'0'.repeat(309)}`} | ${Infinity /*Limitation of IEEE 754-1985 double format*/} | ${undefined}
  ${undefined}             | ${NaN}                                                    | ${undefined}
`(
  'Conversions between enso literals and js numbers: $ensoNumber',
  ({ ensoNumber, jsNumber, expectedEnsoNumber }) => {
    if (ensoNumber != null) {
      const literal = Ast.parse(ensoNumber)
      expect(tryEnsoToNumber(literal)).toBe(jsNumber)
    }
    if (jsNumber != null) {
      const convertedToAst = tryNumberToEnso(jsNumber, MutableModule.Transient())
      expect(convertedToAst?.code()).toBe(expectedEnsoNumber)
    }
  },
)
