import { Token, Tree } from '@/generated/ast'
import { LazyObject } from '@/util/parserSupport'
import { assert } from '../assert'
import { parseEnso2 } from '../ffi'

// TODO[ao] documentation here

export function readAstSpan(node: Tree, code: string): string {
  const leftOffsetbegin = node.spanLeftOffsetCodeOffsetUtf16
  const leftOffsetEnd = leftOffsetbegin + node.spanLeftOffsetCodeUtf16
  const end = leftOffsetEnd + node.spanCodeLengthUtf16
  return code.substring(leftOffsetEnd, end)
}

export function readTokenSpan(token: Token, code: string): string {
  const begin = token.leftOffsetCodeReprBegin
  const end = begin + token.leftOffsetCodeReprLen
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
    const begin = child.spanLeftOffsetCodeReprBegin
    const end = begin + child.spanLeftOffsetCodeReprLen
    if (charIndex >= begin && charIndex < end) {
      yield* astContainingChar(charIndex, child)
    }
  }
  yield root
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test("Reading AST node's code", () => {
    const code = 'Data.read\n2 + 2'
    const ast = parseEnso2(code)
    expect(readAstSpan(ast, code)).toStrictEqual(code)
    assert(ast.type === Tree.Type.BodyBlock)
    const lines = Array.from(ast.statements)
    expect(readAstSpan(lines[0].expression, code)).toStrictEqual('Data.read')
    console.log(lines[1].debug)
    console.log(
      lines[1].expression.debug(),
      lines[1].expression.spanLeftOffsetCodeReprBegin,
      lines[1].expression.spanLeftOffsetCodeReprLen,
    )
    expect(readAstSpan(lines[1].expression, code)).toStrictEqual('2 + 2')
  })
}
