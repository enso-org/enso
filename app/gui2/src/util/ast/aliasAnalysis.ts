import * as Ast from '@/generated/ast'
import { Token, Tree } from '@/generated/ast'
import { assert, assertDefined, assertEqual, assertNotEqual } from '@/util/assert'
import {childrenAstNodes, parseEnso, parseEnsoLine, readTokenSpan} from '@/util/ast'
import { LazyObject, debug } from '@/util/parserSupport'

import * as fss from 'node:fs/promises'

/** Stack that always has at least one element.
 *
 * It is meant to be used with scope-based operations.
 * */
class NonEmptyStack<T> {
  /** The "actual" stack of elements. */
  private readonly stack: T[]

  /** Construct a new stack with the given initial value.
   *
   * The value will serve as an always-present bottom of the stack.
   */
  constructor(initial: T) {
    this.stack = [initial]
  }

  /** Temporary pushes the given value to the stack and calls the callback. */
  withPushed<R>(value: T, callback: (value: T) => R): R {
    this.stack.push(value)
    try {
      return callback(value)
    } finally {
      const popped = this.stack.pop()
      assertDefined(popped, 'Stack is empty.')
      assertEqual(popped, value, 'Stack is inconsistent.')
    }
  }

  /** Get the top-most element of the stack. */
  get top(): T {
    const ret = this.stack[this.stack.length - 1]
    assertDefined(ret, 'Stack is empty.')
    return ret
  }

  /** Iterate over the stack values from the top to the bottom. */
  *valuesFromTop(): Iterable<T> {
    for(let i = this.stack.length - 1; i >= 0; i--) {
      const value = this.stack[i]
      if(value != null) {
        yield value
      }
    }
  }
}

class Scope {
  /** The variables defined in this scope. */
  bindings: Map<string, Token> = new Map()

  /** The variables used (referred to) in this scope. */
  usages: Map<Token, Token[]> = new Map()

  /**
   * @param {Tree} [definition] - Optional Ast element that introduces the scope.
   */
  constructor(public definition?: Tree) {}
}

/** Context tells how the variables are to be treated. */
enum Context {
  Pattern = 'Pattern',
  Expression = 'Expression',
}

class AliasAnalyzer {
  /** All symbols that are not yet resolved (i.e. that were not bound in the analyzed tree). */
  readonly unresolvedSymbols: Set<Token> = new Set()

  /** The special "root" scope that contains the analyzed tree. */
  readonly rootScope: Scope = new Scope()

  /** The stack of scopes, where the top-most scope is the current one. Must never be empty. */
  readonly scopes: NonEmptyStack<Scope> = new NonEmptyStack(this.rootScope)

  /** The stack for keeping track whether we are in a pattern or expression context. */
  readonly contexts: NonEmptyStack<Context> = new NonEmptyStack(Context.Expression)

  /**
   * @param code text representation of the code being analyzed.
   */
  constructor(private readonly code: string) {}

  /** Invoke the given function in a new temporary scope. */
  withNewScope<T>(f: (scope: Scope) => T): T {
    const scope = new Scope()
    return this.withScope(scope, f)
  }

  /** Invoke the given function with the given scope on top of the stack. */
  withScope<T>(scope: Scope, f: (scope: Scope) => T): T {
    return this.scopes.withPushed(scope, value => {
      console.log('Entering a new scope')
      try {
        return f(value)
      }
      finally {
        console.log('Leaving a scope')
      }
    })
  }

  withContext<T>(context: Context, f: () => T): T {
    return this.contexts.withPushed(context, () => {
      console.log(`Entering a new context: ${context}`)
      try {
          return f()
      }
      finally {
          console.log(`Leaving a context: ${context}`)
      }
    })
  }

  bind(token: Token) {
    const scope = this.scopes.top
    const identifier = readTokenSpan(token, this.code)
    console.debug(`Binding ${identifier} to ${debug(token)}`)
    scope.bindings.set(identifier, token)
  }

  use(token: Token) {
    const identifier = readTokenSpan(token, this.code)
    console.debug(`Resolving ${identifier} usage.`)

    for (const scope of this.scopes.valuesFromTop()) {
      const binding = scope.bindings.get(identifier)
      if (binding != null) {
        const usages = scope.usages.get(binding)
        if (usages != null) {
          usages.push(token)
        } else {
          scope.usages.set(binding, [token])
        }
        return
      }
    }

    console.error(`Unresolved identifier: '${identifier}'`)
    this.unresolvedSymbols.add(token)
  }

  processNodeInNewScope(node?: Tree): void {
    if (node == null) {
      return
    }
    this.withNewScope(() => {
      this.processNode(node)
    })
  }

  /** Method that processes a single AST node. */
  processNode(node?: Tree): void {
    if (node == null) {
      return
    }

    console.log('\nprocessNode', debug(node), '\n')

    if (node.type === Tree.Type.BodyBlock) {
      this.withNewScope(
          _ => {
            for (const child of childrenAstNodes(node)) {
              this.processNode(child)
            }
          }
      )
    } else if (node.type === Tree.Type.Function) {
      this.withNewScope(() => {
        this.withContext(Context.Pattern, () => {
          for (const argument of node.args) {
            this.processNode(argument.pattern)
          }
        })
        this.processNode(node.body)
      })
    }
    ///
    else if (node.type === Tree.Type.Documented) {
      this.processNode(node.expression)
    }
    else if (node.type === Tree.Type.Ident) {
      const currentContext = this.contexts.top
      if(currentContext === Context.Pattern) {
        this.bind(node.token)
      } else {
        this.use(node.token)
      }
    } else if (node.type === Tree.Type.Assignment) {
      this.withContext(Context.Pattern, () => {
        this.processNode(node.pattern)
      })
      this.processNode(node.expr)
    }
    ///

    else {
      if (childrenAstNodes(node).length === 0) {
        console.log(`No children for ${debug(node)}`)
      } else {     
        console.warn(`Unsupported AST node type: ${node.type}`)
        console.warn(`Full node: ${debug(node)}`)
      }
    }

  }
}

export function aliasAnalysis(
  code: string,
  tree: Tree
) {
  console.log('aliasAnalysis', debug(tree))
  const analyzer = new AliasAnalyzer(code)
  analyzer.processNode(tree)
}

/**
 * The testing process employs a unique DSL-like syntax to denote the anticipated results.
 *
 * Take this Enso program as an example:
 * ```
 * main =
 *     x = 1
 *     y = 2
 *     z = x -> x + y
 * ```
 *
 * It would be annotated in the following manner:
 * ```
 * main =
 *     «1,x» = 1
 *     «2,y» = 2
 *     z = «3,x» -> «3,x» + »2,y«
 *     u = »1,x« + »2,y«
 * ```
 *
 * In general: `»1,x«` indicates that the identifier links to the `x` variable as per the `«1,x»` pattern.
 * Numerical prefixes are used to differentiate variables of the same name, which is permitted by the language via
 * shadowing.
 *
 * All unannotated identifiers are assumed to preexist in the environment (captured from an external scope or imports).
 */

/** The type of annotation. */
enum AnnotationType {
  /** An identifier binding. */
  Binding = 'Binding',
  /** An identifier usage. */
  Usage = 'Usage',
}

/** Information about an annotated identifier. */
interface AnnotatedIdentifier {
  /** Whether this is an identifier binding or usage. */
  kind: AnnotationType

  /** The numerical prefix of the identifier. */
  prefix: number

  /** The initial index of the identifier in the unannotated code. */
  start: number

  /** The index past the last character of the identifier in the unannotated code. */
  end: number
}

function parseAnnotations(annotatedCode: string): {
  unannotatedCode: string
  annotations: AnnotatedIdentifier[]
} {
  const annotations: AnnotatedIdentifier[] = []

  // Iterate over all annotations (either bindings or usages).
  // I.e. we want to cover both `«1,x»` and `»1,x«` cases, while keeping the track of the annotation type.
  const annotationRegex = /«(\d+),([^»]+)»|»(\d+),([^»]+)«/g

  // As the annotations are removed from the code, we need to keep track of the offset between the annotated and
  // unannotated code. This is necessary to correctly calculate the start and length of the annotations.
  let accumulatedOffset = 0

  const unannotatedCode = annotatedCode.replace(
    annotationRegex,
    (match, bindingPrefix, bindingName, usagePrefix, usageName, offset) => {
      // Sanity check: either both binding prefix and name are present, or both usage prefix and name are present.
      // Otherwise, we have an internal error in the regex.
      assertEqual(bindingPrefix != null, bindingName != null)
      assertEqual(usagePrefix != null, usageName != null)
      assertNotEqual(bindingPrefix != null, usagePrefix != null)

      const prefix = parseInt(bindingPrefix ?? usagePrefix, 10)
      const name = bindingName ?? usageName
      const kind = bindingPrefix != null ? AnnotationType.Binding : AnnotationType.Usage

      const start = offset - accumulatedOffset
      const end = start + name.length

      const annotation = { kind, prefix, start, end }
      accumulatedOffset += match.length - name.length
      annotations.push(annotation)
      return name
    },
  )
  return { unannotatedCode, annotations }
}

/** Alias analysis test case, typically parsed from an annotated code. */
interface TestCase {
  /** The code of the program to be tested, after removing all annotations.
   *  Effectively, this is the code that will be passed to the parser.
   */
  code: string

  /** The list of expected identifier bindings. */
  expected_bindings: Set<Token>

  /** The list of expected identifier usages. */
  expected_usages: Set<Token>
}
//
// /** Parses a test case from the given annotated code string. */
// function parseTestCase(annotatedCode: string): TestCase {
//   const expected_bindings = new Set<Token>()
//   const expected_usages = new Set<Token>()
//
//   // Process code annotations.
//   const annotatedIdentifierRegex = /«(\d+),([^»]+)»/g
//
//   return {
//     code: code,
//     expected_bindings: expected_bindings,
//     expected_usages: expected_usages,
//   }
// }

// TESTS

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test('Parse annotations', () => {
    const annotatedCode = `main =
    «1,x» = 1
    «2,y» = 2
    z = «3,x» -> »3,x« + »2,y«
    u = »1,x« + »2,y«`

    const expectedUnannotatedCode = `main =
    x = 1
    y = 2
    z = x -> x + y
    u = x + y`

    const { unannotatedCode, annotations } = parseAnnotations(annotatedCode)
    assert(unannotatedCode === expectedUnannotatedCode)
    assert(annotations.length === 7)
    const validateAnnotation = (
      i: number,
      kind: AnnotationType,
      prefix: number,
      start: number,
      end: number,
      identifier: string,
    ) => {
      const a = annotations[i]
      try {
        assertDefined(a, `Annotation is not defined.`)
        assertEqual(a.kind, kind, 'Invalid annotation kind.')
        assertEqual(a.prefix, prefix, 'Invalid annotation prefix.')
        assertEqual(a.start, start, 'Invalid annotation start.')
        assertEqual(a.end, end, 'Invalid annotation end.')
        assertEqual(
          unannotatedCode.substring(a.start, a.end),
          identifier,
          'Invalid annotation identifier.',
        )
      } catch (e) {
        console.log(`Invalid annotation #${i}: ${JSON.stringify(a)}.`)
        throw e
      }
    }

    validateAnnotation(0, AnnotationType.Binding, 1, 11, 12, 'x')
    validateAnnotation(1, AnnotationType.Binding, 2, 21, 22, 'y')
    validateAnnotation(2, AnnotationType.Binding, 3, 35, 36, 'x')
    validateAnnotation(3, AnnotationType.Usage, 3, 40, 41, 'x')
    validateAnnotation(4, AnnotationType.Usage, 2, 44, 45, 'y')
    validateAnnotation(5, AnnotationType.Usage, 1, 54, 55, 'x')
    validateAnnotation(6, AnnotationType.Usage, 2, 58, 59, 'y')
  })

  test('Enso File', async () => {
    const path = `H:\\NBO\\enso\\distribution\\lib\\Standard\\Base\\0.0.0-dev\\src\\Data\\Maybe.enso`
    const content = await fss.readFile(path, 'utf-8')
    const ast = parseEnso(content)
    console.log(debug(ast))
    aliasAnalysis(ast)

  })


  test('Plain dependency', () => {
    const code = 'main =\n    x = 1\n    x'
    const ast = parseEnso(code)
    console.log(ast.type)

    console.log(`Alias analysis: ${JSON.stringify(aliasAnalysis(code, ast))}`)

    // if (ast.type === Tree.Type.BodyBlock) {
    //   Array.from(ast.statements).forEach((line, i) => {
    //     console.log(`=== line #${i} ===`)
    //     console.log(debug(line))
    //   })
    //   console.log('=== end ===')
    // }
    //
    // console.log('debug(ast)', debug(ast))
    // console.log('ast', ast)
    // ast.visitChildren((obj: any) => {
    //   console.log('obj', obj)
    // })
    //
    // const children = Array.from(ast.children())
    // console.log('children', children)
    //
    // const childrenNodes = childrenAstNodes(ast)
    // console.log('childrenNodes', childrenNodes)
  })

  test('parseEnsoLine', () => {
    const ast = parseEnsoLine('1 + 2')
    console.log(debug(ast))
    console.log(ast)
  })

  test('Traverse AST', () => {
    const program = parseEnso('main =\n    x = 1\n    x')

    childrenAstNodes(program).forEach((node, i) => {
        console.log(`=== node #${i} ===`)
        console.log(debug(node))
    })


    const children = Array.from(program.children())
    console.log('children', children)


    // console.log("visitChildren start")
    // const processChild: (obj: any) => {
    //   console.log('\tchild', obj)
    //   obj.visitChildren(processChild)
    // }
    // program.visitChildren((obj: any) => {
    //     console.log('\tchild', obj)
    // })
    // console.log("visitChildren end")
  })
}
