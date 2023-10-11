import { parseEnso } from '@/util/ast'
import { debug } from '@/util/parserSupport'
import { expect, test } from 'vitest'

test.each([
  {
    code: ' foo bar\n',
    expected: {
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
          newline: {
            startInCodeBuffer: 8,
            lengthInCodeBuffer: 1,
            whitespaceLengthInCodeBuffer: 0,
          },
        },
      ],
      type: 'BodyBlock',
    },
  },
  {
    code: '2 + 3',
    expected: {
      childrenLengthInCodeParsed: 5,
      whitespaceStartInCodeParsed: 0,
      whitespaceLengthInCodeParsed: 0,
      statements: [
        {
          expression: {
            childrenLengthInCodeParsed: 5,
            whitespaceStartInCodeParsed: 0,
            whitespaceLengthInCodeParsed: 0,
            type: 'OprApp',
            lhs: {
              childrenLengthInCodeParsed: 1,
              whitespaceStartInCodeParsed: 0,
              whitespaceLengthInCodeParsed: 0,
              integer: {
                startInCodeBuffer: 0,
                lengthInCodeBuffer: 1,
                whitespaceStartInCodeBuffer: 0,
                whitespaceLengthInCodeBuffer: 0,
                type: 'Digits',
              },
              type: 'Number',
            },
            opr: {
              ok: true,
              value: {
                startInCodeBuffer: 2,
                lengthInCodeBuffer: 1,
                whitespaceStartInCodeBuffer: 1,
                whitespaceLengthInCodeBuffer: 1,
                type: 'Operator',
              },
            },
            rhs: {
              childrenLengthInCodeParsed: 1,
              whitespaceStartInCodeParsed: 3,
              whitespaceLengthInCodeParsed: 1,
              integer: {
                startInCodeBuffer: 4,
                lengthInCodeBuffer: 1,
                whitespaceStartInCodeBuffer: 4,
                whitespaceLengthInCodeBuffer: 0,
                type: 'Digits',
              },
              type: 'Number',
            },
          },
          newline: {
            startInCodeBuffer: 0,
            lengthInCodeBuffer: 0,
            whitespaceStartInCodeBuffer: 0,
            whitespaceLengthInCodeBuffer: 0,
            type: 'Newline',
          },
        },
      ],
      type: 'BodyBlock',
    },
  },
  {
    code: 'Data.read File\n2 + 3',
    expected: {
      childrenLengthInCodeParsed: 20,
      statements: [
        {
          expression: {
            arg: {
              childrenLengthInCodeParsed: 4,
              token: {
                isTypeOrConstructor: true,
                lengthInCodeBuffer: 4,
                liftLevel: 0,
                startInCodeBuffer: 10,
                type: 'Ident',
                whitespaceLengthInCodeBuffer: 0,
                whitespaceStartInCodeBuffer: 10,
              },
              type: 'Ident',
              whitespaceLengthInCodeParsed: 1,
              whitespaceStartInCodeParsed: 9,
            },
            childrenLengthInCodeParsed: 14,
            func: {
              childrenLengthInCodeParsed: 9,
              lhs: {
                childrenLengthInCodeParsed: 4,
                token: {
                  isTypeOrConstructor: true,
                  lengthInCodeBuffer: 4,
                  liftLevel: 0,
                  startInCodeBuffer: 0,
                  type: 'Ident',
                  whitespaceLengthInCodeBuffer: 0,
                  whitespaceStartInCodeBuffer: 0,
                },
                type: 'Ident',
                whitespaceLengthInCodeParsed: 0,
                whitespaceStartInCodeParsed: 0,
              },
              opr: {
                ok: true,
                value: {
                  lengthInCodeBuffer: 1,
                  startInCodeBuffer: 4,
                  type: 'Operator',
                  whitespaceLengthInCodeBuffer: 0,
                  whitespaceStartInCodeBuffer: 4,
                },
              },
              rhs: {
                childrenLengthInCodeParsed: 4,
                token: {
                  lengthInCodeBuffer: 4,
                  liftLevel: 0,
                  startInCodeBuffer: 5,
                  type: 'Ident',
                  whitespaceLengthInCodeBuffer: 0,
                  whitespaceStartInCodeBuffer: 5,
                },
                type: 'Ident',
                whitespaceLengthInCodeParsed: 0,
                whitespaceStartInCodeParsed: 5,
              },
              type: 'OprApp',
              whitespaceLengthInCodeParsed: 0,
              whitespaceStartInCodeParsed: 0,
            },
            type: 'App',
            whitespaceLengthInCodeParsed: 0,
            whitespaceStartInCodeParsed: 0,
          },
          newline: {
            lengthInCodeBuffer: 0,
            startInCodeBuffer: 0,
            type: 'Newline',
            whitespaceLengthInCodeBuffer: 0,
            whitespaceStartInCodeBuffer: 0,
          },
        },
        {
          expression: {
            childrenLengthInCodeParsed: 5,
            lhs: {
              childrenLengthInCodeParsed: 1,
              integer: {
                lengthInCodeBuffer: 1,
                startInCodeBuffer: 15,
                type: 'Digits',
                whitespaceLengthInCodeBuffer: 0,
                whitespaceStartInCodeBuffer: 15,
              },
              type: 'Number',
              whitespaceLengthInCodeParsed: 0,
              whitespaceStartInCodeParsed: 15,
            },
            opr: {
              ok: true,
              value: {
                lengthInCodeBuffer: 1,
                startInCodeBuffer: 17,
                type: 'Operator',
                whitespaceLengthInCodeBuffer: 1,
                whitespaceStartInCodeBuffer: 16,
              },
            },
            rhs: {
              childrenLengthInCodeParsed: 1,
              integer: {
                lengthInCodeBuffer: 1,
                startInCodeBuffer: 19,
                type: 'Digits',
                whitespaceLengthInCodeBuffer: 0,
                whitespaceStartInCodeBuffer: 19,
              },
              type: 'Number',
              whitespaceLengthInCodeParsed: 1,
              whitespaceStartInCodeParsed: 18,
            },
            type: 'OprApp',
            whitespaceLengthInCodeParsed: 0,
            whitespaceStartInCodeParsed: 15,
          },
          newline: {
            lengthInCodeBuffer: 1,
            startInCodeBuffer: 14,
            type: 'Newline',
            whitespaceLengthInCodeBuffer: 0,
            whitespaceStartInCodeBuffer: 14,
          },
        },
      ],
      type: 'BodyBlock',
      whitespaceLengthInCodeParsed: 0,
      whitespaceStartInCodeParsed: 0,
    },
  },
  {
    code: 'function file=var',
    expected: {
      childrenLengthInCodeParsed: 17,
      statements: [
        {
          expression: {
            arg: {
              childrenLengthInCodeParsed: 3,
              token: {
                lengthInCodeBuffer: 3,
                liftLevel: 0,
                startInCodeBuffer: 14,
                type: 'Ident',
                whitespaceLengthInCodeBuffer: 0,
                whitespaceStartInCodeBuffer: 14,
              },
              type: 'Ident',
              whitespaceLengthInCodeParsed: 0,
              whitespaceStartInCodeParsed: 14,
            },
            childrenLengthInCodeParsed: 17,
            equals: {
              lengthInCodeBuffer: 1,
              startInCodeBuffer: 13,
              type: 'Operator',
              whitespaceLengthInCodeBuffer: 0,
              whitespaceStartInCodeBuffer: 13,
            },
            func: {
              childrenLengthInCodeParsed: 8,
              token: {
                lengthInCodeBuffer: 8,
                liftLevel: 0,
                startInCodeBuffer: 0,
                type: 'Ident',
                whitespaceLengthInCodeBuffer: 0,
                whitespaceStartInCodeBuffer: 0,
              },
              type: 'Ident',
              whitespaceLengthInCodeParsed: 0,
              whitespaceStartInCodeParsed: 0,
            },
            name: {
              lengthInCodeBuffer: 4,
              liftLevel: 0,
              startInCodeBuffer: 9,
              type: 'Ident',
              whitespaceLengthInCodeBuffer: 1,
              whitespaceStartInCodeBuffer: 8,
            },
            type: 'NamedApp',
            whitespaceLengthInCodeParsed: 0,
            whitespaceStartInCodeParsed: 0,
          },
          newline: {
            lengthInCodeBuffer: 0,
            startInCodeBuffer: 0,
            type: 'Newline',
            whitespaceLengthInCodeBuffer: 0,
            whitespaceStartInCodeBuffer: 0,
          },
        },
      ],
      type: 'BodyBlock',
      whitespaceLengthInCodeParsed: 0,
      whitespaceStartInCodeParsed: 0,
    },
  },
])('Parsing $code', ({ code, expected }) => {
  expect(debug(parseEnso(code))).toMatchObject(expected)
})
