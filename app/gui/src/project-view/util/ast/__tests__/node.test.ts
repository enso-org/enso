import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import { nodeFromAst, primaryApplication } from '@/util/ast/node'
import { expect, test } from 'vitest'
import { nodeDocumentationText } from '../node'

test.each`
  line                               | pattern      | rootExpr   | documentation
  ${'2 + 2'}                         | ${undefined} | ${'2 + 2'} | ${''}
  ${'foo = bar'}                     | ${'foo'}     | ${'bar'}   | ${''}
  ${'## Documentation\n2 + 2'}       | ${undefined} | ${'2 + 2'} | ${'Documentation'}
  ${'## Documentation\nfoo = 2 + 2'} | ${'foo'}     | ${'2 + 2'} | ${'Documentation'}
`('Node information from AST $line line', ({ line, pattern, rootExpr, documentation }) => {
  const ast = Ast.parseBlockStatement(line)
  assertDefined(ast)
  const node = nodeFromAst(ast, false)
  expect(node).toBeDefined()
  assertDefined(node)
  expect(node.outerAst).toBe(ast)
  expect(node.pattern?.code()).toBe(pattern)
  expect(node.rootExpr.code()).toBe(rootExpr)
  expect(node.innerExpr.code()).toBe(rootExpr)
  expect(nodeDocumentationText(node)).toBe(documentation)
})

test.each(['## Documentation only'])("'%s' should not be a node", (line) => {
  const ast = Ast.parseBlockStatement(line)
  assertDefined(ast)
  const node = nodeFromAst(ast, false)
  expect(node).toBeUndefined()
})

test.each`
  code                                        | selfArg        | func                                      | accessChain
  ${'operator1'}                              | ${undefined}   | ${undefined}                              | ${undefined}
  ${'operator1 foo bar'}                      | ${undefined}   | ${undefined}                              | ${undefined}
  ${'operator1.parse_json'}                   | ${'operator1'} | ${'operator1.parse_json'}                 | ${['parse_json']}
  ${'operator1 . parse_json'}                 | ${'operator1'} | ${'operator1 . parse_json'}               | ${['parse_json']}
  ${'operator1.parse_json operator2.to_json'} | ${'operator1'} | ${'operator1.parse_json'}                 | ${['parse_json']}
  ${'operator1.parse_json foo bar'}           | ${'operator1'} | ${'operator1.parse_json'}                 | ${['parse_json']}
  ${'operator1.parse_json.length'}            | ${'operator1'} | ${'operator1.parse_json.length'}          | ${['parse_json', 'length']}
  ${'operator1.parse_json.length foo bar'}    | ${'operator1'} | ${'operator1.parse_json.length'}          | ${['parse_json', 'length']}
  ${'operator1 + operator2'}                  | ${undefined}   | ${undefined}                              | ${undefined}
  ${'(operator1).parse_json'}                 | ${'operator1'} | ${'(operator1).parse_json'}               | ${['parse_json']}
  ${'(operator1:Type).parse_json'}            | ${'operator1'} | ${'(operator1:Type).parse_json'}          | ${['parse_json']}
  ${'((operator1:Type)).parse_json'}          | ${'operator1'} | ${'((operator1:Type)).parse_json'}        | ${['parse_json']}
  ${'operator1:Type . parse_json'}            | ${'operator1'} | ${'operator1:Type . parse_json'}          | ${['parse_json']}
  ${'(operator1):Type . parse_json'}          | ${'operator1'} | ${'(operator1):Type . parse_json'}        | ${['parse_json']}
  ${'(operator1:Type):Type . parse_json'}     | ${'operator1'} | ${'(operator1:Type):Type . parse_json'}   | ${['parse_json']}
  ${'((operator1):Type):Type . parse_json'}   | ${'operator1'} | ${'((operator1):Type):Type . parse_json'} | ${['parse_json']}
`('Primary application of $code', ({ code, selfArg, func, accessChain }) => {
  const ast = Ast.parseExpression(code)
  assertDefined(ast)
  const module = ast.module
  const primaryApp = primaryApplication(ast)
  const expected = { selfArg, function: func, accessChain }
  const analyzed = {
    selfArg: primaryApp.selfArgument ? module.get(primaryApp.selfArgument).code() : undefined,
    function: primaryApp.function ? module.get(primaryApp.function).code() : undefined,
    accessChain: primaryApp.accessChain?.map((id) => {
      const ast = module.get(id)
      assert(ast instanceof Ast.MutablePropertyAccess)
      return ast.rhs.code()
    }),
  }
  expect(analyzed).toEqual(expected)
})
