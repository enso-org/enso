import { Token, Tree } from '@/generated/ast'
import { LazyObject } from '@/util/parserSupport'
import { assert } from '../assert'
import { parseEnso2, parseEnsoLine } from '../ffi'

// TODO[ao] documentation here

export function readAstSpan(node: Tree, code: string): string {
  const leftOffsetbegin = node.whitespaceStart
  const leftOffsetEnd = leftOffsetbegin + node.whitespaceLength
  const end = leftOffsetEnd + node.childrenCodeLength
  return code.substring(leftOffsetEnd, end)
}

export function readTokenSpan(token: Token, code: string): string {
  const begin = token.codeStart
  const end = begin + token.codeLength
  return code.substring(begin, end)
}

export function* childrenAstNodes(obj: unknown): Generator<Tree> {
  function* astNodeOrChildren(obj: unknown) {
    if (obj instanceof Tree.AbstractBase) {
      yield obj as Tree
    } else {
      yield* childrenAstNodes(obj)
    }
  }
  if (obj == null) return
  const maybeIterable = obj as Record<symbol, unknown>
  const isIterator = typeof maybeIterable[Symbol.iterator] == 'function'
  if (obj instanceof LazyObject) {
    const prototype = Object.getPrototypeOf(obj)
    const descriptors = Object.getOwnPropertyDescriptors(prototype)
    const objWithProp = obj as unknown as Record<string, unknown>

    for (const prop in descriptors) {
      if (descriptors[prop]?.get == null) continue
      const value = objWithProp[prop]
      yield* astNodeOrChildren(value)
    }
  } else if (isIterator) {
    const iterable = obj as Iterable<unknown>
    for (const element of iterable) {
      yield* astNodeOrChildren(element)
    }
  }
}

export function* astContainingChar(charIndex: number, root: Tree): Generator<Tree> {
  for (const child of childrenAstNodes(root)) {
    console.log(`checking ${child.type}`)
    const begin = child.whitespaceStart + child.whitespaceLength
    const end = begin + child.childrenCodeLength
    console.log(begin, end, charIndex)
    if (charIndex >= begin && charIndex < end) {
      console.log('entering')
      yield* astContainingChar(charIndex, child)
    }
  }
  yield root
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test("Reading AST node's code", () => {
    const code = 'Data.read File\n2 + 3'
    const ast = parseEnso2(code)
    expect(readAstSpan(ast, code)).toStrictEqual(code)
    assert(ast.type === Tree.Type.BodyBlock)
    const lines = Array.from(ast.statements)

    assert(lines[0]?.expression != null)
    console.log(lines[0].expression.childrenCodeLength)
    expect(readAstSpan(lines[0].expression, code)).toStrictEqual('Data.read File')
    assert(lines[0].expression.type === Tree.Type.App)
    expect(readAstSpan(lines[0].expression.func, code)).toStrictEqual('Data.read')
    expect(readAstSpan(lines[0].expression.arg, code)).toStrictEqual('File')

    assert(lines[1]?.expression != null)
    // expect(readAstSpan(lines[1].expression, code)).toStrictEqual('2 + 3')
    assert(lines[1].expression.type === Tree.Type.OprApp)
    assert(lines[1].expression.lhs != null)
    assert(lines[1].expression.rhs != null)
    assert(lines[1].expression.opr.ok)
    // expect(readAstSpan(lines[1].expression.lhs, code)).toStrictEqual('2')
    expect(readTokenSpan(lines[1].expression.opr.value, code)).toStrictEqual('+')
    expect(readAstSpan(lines[1].expression.rhs, code)).toStrictEqual('3')
  })

  test.each([
    [
      '2 + a',
      [
        { type: Tree.Type.Number, repr: '2' },
        { type: Tree.Type.Ident, repr: 'a' },
      ],
    ],
    [
      'a.b',
      [
        { type: Tree.Type.Ident, repr: 'a' },
        { type: Tree.Type.Ident, repr: 'b' },
      ],
    ],
    [
      'Data.read foo',
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.Ident, repr: 'foo' },
      ],
    ],
    ['(2 + a)', [{ type: Tree.Type.OprApp, repr: '2 + a' }]],
    [
      'Data.read\n  foo\n  bar',
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.Ident, repr: 'foo' },
        { type: Tree.Type.Ident, repr: 'bar' },
      ],
    ],
    [
      'Data.read file=foo',
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.Ident, repr: 'Dat' },
      ],
    ],
  ])("Reading children of '%s'", (code, expected) => {
    const ast = parseEnsoLine(code)
    const children = Array.from(childrenAstNodes(ast))
    const childrenWithExpected = children.map((child, i) => {
      return { child, expected: expected[i] }
    })
    for (const { child, expected } of childrenWithExpected) {
      expect(child.type).toBe(expected?.type)
      expect(readAstSpan(child, code)).toBe(expected?.repr)
    }
  })

  test.each([
    [
      '2 + a',
      0,
      [
        { type: Tree.Type.Number, repr: '2' },
        { type: Tree.Type.OprApp, repr: '2 + a' },
        { type: Tree.Type.BodyBlock, repr: '2 + a' },
      ],
    ],
    [
      'Data.read foo',
      5,
      [
        { type: Tree.Type.Ident, repr: 'read' },
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.App, repr: 'Data.read foo' },
        { type: Tree.Type.BodyBlock, repr: 'Data.read foo' },
      ],
    ],
    [
      'Data.read foo',
      4,
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.App, repr: 'Data.read foo' },
        { type: Tree.Type.BodyBlock, repr: 'Data.read foo' },
      ],
    ],
    [
      'Data.read foo',
      9,
      [
        { type: Tree.Type.App, repr: 'Data.read foo' },
        { type: Tree.Type.BodyBlock, repr: 'Data.read foo' },
      ],
    ],
  ])("Reading AST from code '%s' and position %i", (code, position, expected) => {
    const ast = parseEnso2(code)
    const astAtPosition = Array.from(astContainingChar(position, ast))
    const resultWithExpected = astAtPosition.map((ast, i) => {
      return { ast, expected: expected[i] }
    })
    for (const { ast, expected } of resultWithExpected) {
      expect(ast.type).toBe(expected?.type)
      expect(readAstSpan(ast, code)).toBe(expected?.repr)
    }
  })
}
