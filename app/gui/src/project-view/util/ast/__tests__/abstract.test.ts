import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import {
  escapeTextLiteral,
  findModuleMethod,
  substituteIdentifier,
  substituteQualifiedName,
  substituteQualifiedNameByPattern,
  subtrees,
  tryEnsoToNumber,
  tryNumberToEnso,
  unescapeTextLiteral,
  type Identifier,
} from '@/util/ast/abstract'
import { qnLastSegment } from '@/util/qualifiedName'
import { fc, test } from '@fast-check/vitest'
import { expect } from 'vitest'
import { BodyBlock } from 'ydoc-shared/ast'
import { findExpressions, testCase } from './testCase'

function functionBlock(topLevel: BodyBlock, name: string) {
  const func = findModuleMethod(topLevel, name)
  if (!(func?.statement.body instanceof BodyBlock)) return undefined
  return func.statement.body
}

test('Insert new expression', () => {
  const code = 'main =\n    text1 = "foo"\n'
  const root = Ast.parseBlock(code)
  const main = functionBlock(root, 'main')!
  expect(main).toBeDefined()
  const edit = root.module.edit()
  const rhs = Ast.parseExpression('42', edit)!
  const assignment = Ast.Assignment.new('baz' as Identifier, rhs, { edit })
  edit.getVersion(main).push(assignment)
  const printed = edit.getVersion(root).code()
  expect(printed).toEqual('main =\n    text1 = "foo"\n    baz = 42\n')
})

type SimpleModule = {
  root: Ast.BodyBlock
  main: Ast.FunctionDef
  mainBlock: Ast.BodyBlock
  assignment: Ast.Assignment
}
function simpleModule(): SimpleModule {
  const code = 'main =\n    text1 = "foo"\n'
  const root = Ast.parseBlock(code)
  const main = findModuleMethod(root, 'main')!.statement
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
  const assignment_ = edit.tryGet(assignment.id)
  assert(assignment_ instanceof Ast.MutableAssignment)
  expect(assignment_.expression!.id).toBe(newValue.id)
  expect(edit.tryGet(assignment_.expression!.id)?.code()).toBe("'bar'")
  const printed = edit.getVersion(root).code()
  expect(printed).toEqual("main =\n    text1 = 'bar'\n")
})

test('Modify subexpression - setting a vector', () => {
  // A case where the #9357 bug was visible.
  const code = 'main =\n    text1 = foo\n'
  const root = Ast.parseBlock(code)
  const main = functionBlock(root, 'main')!
  expect(main).not.toBeNull()
  const assignment: Ast.Assignment = main.statements().next().value
  expect(assignment).toBeInstanceOf(Ast.Assignment)

  const edit = root.module.edit()
  const transientModule = Ast.MutableModule.Transient()
  const barExpression = Ast.parseExpression('bar')
  assertDefined(barExpression)
  const newValue = Ast.Vector.new(transientModule, [barExpression])
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
    if (a.statement?.node.code() === b.statement?.node.code()) return 0
    if (!a.statement) return 1
    if (!b.statement) return -1
    return a.statement.node.code() < b.statement.node.code() ? -1 : 1
  })
  const edit = block.module.edit()
  const newBlock = Ast.BodyBlock.new(reordered, edit)
  // Note that trailing whitespace belongs to the following line.
  expect(newBlock.code()).toBe('GNIK  \nSISI\nVLE \n')
})

test('Splice', () => {
  const module = Ast.MutableModule.Transient()
  const edit = module.edit()
  const ident = Ast.Ident.new(edit, 'foo' as Identifier)
  expect(ident.code()).toBe('foo')
  const spliced = module.copyIfForeign(ident)
  expect(spliced.module).toBe(module)
  expect(spliced.code()).toBe('foo')
})

test('Construct app', () => {
  const edit = Ast.MutableModule.Transient()
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
  const block = Ast.parseModule('main = func arg1 arg2')
  block.module.setRoot(block)
  let arg1: Ast.MutableAst | undefined
  Ast.visitRecursive(block, (ast) => {
    if (ast instanceof Ast.MutableIdent && ast.code() === 'arg1') {
      assert(arg1 == null)
      arg1 = ast
    }
  })
  assert(arg1 != null)
  const replacementExpr = Ast.parseExpression('innerfunc innerarg', block.module)
  assertDefined(replacementExpr)
  arg1.replace(replacementExpr)
  const correctCode = 'main = func (innerfunc innerarg) arg2'
  // This assertion will fail when smart printing handles this case.
  // At that point we should test tree repair separately.
  assert(block.code() !== correctCode)
  Ast.repair(block, block.module)
  expect(block.code()).toBe(correctCode)
})

test('Tree repair: Non-canonical block line attribution', () => {
  const beforeCase = testCase({
    'func a b =': Ast.FunctionDef,
    '    c = a + b': Ast.Assignment,
    'main =': Ast.FunctionDef,
    '    func arg1 arg2': Ast.ExpressionStatement,
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
  const repairedRoot = repair.root()
  assertDefined(repairedRoot)
  const afterRepair = findExpressions(repairedRoot, {
    'func a b =': Ast.FunctionDef,
    'c = a + b': Ast.Assignment,
    'main =': Ast.FunctionDef,
    'func arg1 arg2': Ast.ExpressionStatement,
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

test('Analyze app-like', () => {
  const appLike = Ast.parseExpression('(Preprocessor.default_preprocessor 3 _ 5 _ <| 4) <| 6')
  assertDefined(appLike)
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
    const expression = Ast.parseExpression(original) ?? Ast.parseBlockStatement(original)
    assertDefined(expression)
    const result = substituteQualifiedNameByPattern(
      expression,
      pattern as Ast.Identifier,
      substitution as Ast.Identifier,
    )
    expect(result.code()).toEqual(expected)
  },
)

test.each([
  {
    original: 'Standard.Base.Vector Standard.Base.Number',
    expected: 'Vector Number',
  },
  {
    original: 'Standard.Base.Any.Any',
    expected: 'Any',
  },
])(
  'Substitute qualified name with function returning last segment in $original',
  ({ original, expected }) => {
    const expression = Ast.parseExpression(original)
    assertDefined(expression)
    const result = substituteQualifiedName(expression, (qn) => qnLastSegment(qn))
    expect(result.code()).toEqual(expected)
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
    const expression = Ast.parseExpression(original) ?? Ast.parseBlockStatement(original)
    assertDefined(expression)
    const module = expression.module
    module.setRoot(expression)
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
  const literal = Ast.TextLiteral.new(rawText)
  expect(literal.rawTextContent).toBe(rawText)
})

test.prop({
  boundary: fc.constantFrom('"', "'"),
  rawText: sometimesUnicodeString,
})('AST text literal rawTextContent', ({ boundary, rawText }) => {
  const literal = Ast.TextLiteral.new('')
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
  const literal = Ast.TextLiteral.new('')
  literal.setBoundaries('"')
  const rawText = '\n'
  literal.setRawTextContent(rawText)
  expect(literal.rawTextContent).toBe(rawText)
  expect(literal.code()).toBe(`'${escapeTextLiteral(rawText)}'`)
})

test.each`
  text               | fixed
  ${"'abc'"}         | ${"'abc'"}
  ${'"abc"'}         | ${'"abc"'}
  ${"'''abc\n  cde"} | ${"'''abc\n  cde"}
  ${"'abc"}          | ${"'abc'"}
  ${'"abc'}          | ${'"abc"'}
`('Fixing boundaries in $text', ({ text, fixed }) => {
  const literal = Ast.TextLiteral.tryParse(text)
  assert(literal != null)
  literal.fixBoundaries()
  expect(literal.code()).toBe(fixed)
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
  const ast = Ast.parseExpression(code)
  assertDefined(ast)
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
  const elem = Ast.parseExpression(pushed, vector.module)
  assertDefined(elem)
  vector.push(elem)
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
    const elemValue = Ast.parseExpression(value, vector.module)
    assertDefined(elemValue)
    vector.set(index, elemValue)
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
      const literal = Ast.parseExpression(ensoNumber)
      assertDefined(literal)
      expect(tryEnsoToNumber(literal)).toBe(jsNumber)
    }
    if (jsNumber != null) {
      const convertedToAst = tryNumberToEnso(jsNumber, Ast.MutableModule.Transient())
      expect(convertedToAst?.code()).toBe(expectedEnsoNumber)
    }
  },
)
