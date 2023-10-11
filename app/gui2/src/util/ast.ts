import * as Ast from '@/generated/ast'
import { debug, validateSpans } from '@/util/parserSupport'
import { parse } from './ffi'

export { Ast }

export function parseEnso(code: string): Ast.Tree {
  const blob = parse(code)
  return Ast.deserializeTree(blob.buffer)
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest
  test('testParse', () => {
    const identInput = ' foo bar\n'
    const tree = parseEnso(identInput)
    expect(debug(tree)).toMatchObject({
      childrenLengthInCodeParsed: 8,
      whitespaceStartInCodeParsed: 0,
      whitespaceLengthInCodeParsed: 1,
      statements: [
        {
          expression: {
            arg: {
              childrenLengthInCodeParsed: 3,
              whitespaceStartInCodeParsed: 4,
              whitespaceLengthInCodeParsed: 1,
              token: {
                startInCodeBuffer: 5,
                lengthInCodeBuffer: 3,
                whitespaceLengthInCodeBuffer: 0,
              },
              type: 'Ident',
            },
            func: {
              childrenLengthInCodeParsed: 3,
              whitespaceLengthInCodeParsed: 0,
              token: {
                startInCodeBuffer: 1,
                lengthInCodeBuffer: 3,
                whitespaceLengthInCodeBuffer: 0,
              },
              type: 'Ident',
            },
            childrenLengthInCodeParsed: 7,
            whitespaceLengthInCodeParsed: 0,
            type: 'App',
          },
          newline: {
            lengthInCodeBuffer: 0,
            whitespaceLengthInCodeBuffer: 0,
          },
        },
        {
          expression: undefined,
          newline: {
            startInCodeBuffer: 8,
            lengthInCodeBuffer: 1,
            whitespaceLengthInCodeBuffer: 0,
          },
        },
      ],
      type: 'BodyBlock',
    })
  })
  test('testCase', () => {
    const input = 'Data.read\n2 + 2'
    const tree = parseEnso(input)
    const endPos = validateSpans(tree)
    expect(endPos).toStrictEqual(input.length)
  })
  test('testSpans', () => {
    const input = ' foo bar\n'
    const tree = parseEnso(input)
    const endPos = validateSpans(tree)
    expect(endPos).toStrictEqual(input.length)
  })
}
