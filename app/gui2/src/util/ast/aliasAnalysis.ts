import * as Ast from '@/generated/ast'
import { Token, Tree } from '@/generated/ast'
import { assert, assertDefined, assertEqual, assertNotEqual } from '@/util/assert'
import {
  childrenAstNodes,
  debugAst,
  parseEnso,
  parseEnsoLine,
  parsedTreeOrTokenRange,
  readAstOrTokenSpan,
  readTokenSpan, astPrettyPrintType,
} from '@/util/ast'
import { LazyObject } from '@/util/parserSupport'

import * as fss from 'node:fs/promises'
import type { ContentRange } from '../../../shared/yjsModel.ts'

type Primitive = string | number | boolean | bigint | symbol | null | undefined

/**
 * Map that supports Object-based keys.
 *
 * Internally keys are converted to strings using the provided {@link keyMapper} function and then compared.
 *
 * @template Key The type of the keys.
 * @template Value The type of the values.
 */
export class ObjectKeyedMap<Key extends Object, Value> {
  /** The inner map that stores the values. */
  private readonly map = new Map<string, [Key, Value]>()

  /** Construct a new map, optionally setting a custom key mapper.
   *
   * @param keyMapper The function that maps the keys to strings. By default, strings are generated using {@link JSON.stringify}.
   *
   */
  constructor(private readonly keyMapper: (key: Key) => string = JSON.stringify) {}

  /** Set the value for the given key. */
  set(key: Key, value: Value): this {
    const innerKey = this.keyMapper(key)
    this.map.set(innerKey, [key, value])
    return this
  }

  /** Get the value for the given key, or `undefined` if it does not exist. */
  get(key: Key): Value | undefined {
    const innerKey = this.keyMapper(key)
    const entry = this.map.get(innerKey)
    return entry ? entry[1] : undefined
  }

  /** Check if the map contains a value for the given key. */
  has(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.map.has(innerKey)
  }

  /** Remove the value for the given key. */
  delete(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.map.delete(innerKey)
  }

  /** Remove all values from the map. */
  clear(): void {
    this.map.clear()
  }

  /** Get the number of values in the map. */
  get size(): number {
    return this.map.size
  }

  /** Iterate over the values in the map. */
  *[Symbol.iterator](): IterableIterator<[Key, Value]> {
    for (const [_innerKey, [key, value]] of this.map.entries()) {
      yield [key, value]
    }
  }
}

/**
 * Set that supports Object-based keys.
 *
 * Internally keys are converted to strings using the provided {@link keyMapper} function and then compared.
 *
 * @template Key The type of the keys.
 */
export class ObjectKeyedSet<Key extends Object> {
  /** The inner set that stores the keys. */
  private readonly set = new Set<string>()

  /** Construct a new set, optionally setting a custom key mapper.
   *
   * @param keyMapper The function that maps the keys to strings. By default, strings are generated using {@link JSON.stringify}.
   *
   */
  constructor(private readonly keyMapper: (key: Key) => string = JSON.stringify) {}

  /** Add the given key to the set. */
  add(key: Key): this {
    const innerKey = this.keyMapper(key)
    this.set.add(innerKey)
    return this
  }

  /** Check if the set contains the given key. */
  has(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.set.has(innerKey)
  }

  /** Remove the given key from the set. */
  delete(key: Key): boolean {
    const innerKey = this.keyMapper(key)
    return this.set.delete(innerKey)
  }

  /** Remove all keys from the set. */
  clear(): void {
    this.set.clear()
  }

  /** Get the number of keys in the set. */
  get size(): number {
    return this.set.size
  }

  /** Iterate over the keys in the set. */
  *[Symbol.iterator](): IterableIterator<Key> {
    for (const innerKey of this.set) {
      yield JSON.parse(innerKey) as Key
    }
  }
}

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
  withPushed<R>(value: T, callback: (value: T) => R): { value: T; result?: R } {
    this.stack.push(value)
    let result = undefined
    try {
      result = callback(value)
    } finally {
      const popped = this.stack.pop()
      assertDefined(popped, 'Stack is empty.')
      assertEqual(popped, value, 'Stack is inconsistent.')
    }
    return { value, result }
  }

  /** Get the top-most element of the stack. */
  get top(): T {
    const ret = this.stack[this.stack.length - 1]
    assertDefined(ret, 'Stack is empty.')
    return ret
  }

  /** Iterate over the stack values from the top to the bottom. */
  *valuesFromTop(): Iterable<T> {
    for (let i = this.stack.length - 1; i >= 0; i--) {
      const value = this.stack[i]
      // Be defensive against the stack mutation during the iteration.
      if (value != null) {
        yield value
      }
    }
  }
}

class Scope {
  /** The variables defined in this scope. */
  bindings: ObjectKeyedMap<string, Token> = new ObjectKeyedMap()

  /** The children scopes. */
  children: Scope[] = []

  /** Construct a new scope for the given range. If the parent scope is provided, the new scope will be added to its
   * children.
   *
   * @param range The range of the code that is covered by this scope.
   * @param parent The parent scope. If not provided, the scope will be considered the root scope.
   */
  constructor(
    public range: ContentRange,
    parent?: Scope,
  ) {
    if (parent != null) {
      parent.children.push(this)
    }
  }
}

/** Context tells how the variables are to be treated. */
enum Context {
  Pattern = 'Pattern',
  Expression = 'Expression',
}

export class AliasAnalyzer {
  /** All symbols that are not yet resolved (i.e. that were not bound in the analyzed tree). */
  readonly unresolvedSymbols: ObjectKeyedSet<ContentRange> = new ObjectKeyedSet()

  /** The AST representation of the code. */
  readonly ast: Tree

  /** The special "root" scope that contains the analyzed tree. */
  readonly rootScope: Scope

  /** The stack of scopes, where the top-most scope is the current one. Must never be empty. */
  private readonly scopes: NonEmptyStack<Scope>

  /** The stack for keeping track whether we are in a pattern or expression context. */
  private readonly contexts: NonEmptyStack<Context> = new NonEmptyStack(Context.Expression)

  readonly aliases: ObjectKeyedMap<ContentRange, ObjectKeyedSet<ContentRange>> = new ObjectKeyedMap()

  /**
   * @param code text representation of the code.
   * @param ast AST representation of the code. If not provided, it will be parsed from the text.
   */
  constructor(
    private readonly code: string,
    ast?: Tree,
  ) {
    this.ast = ast ?? parseEnso(code)
    this.rootScope = new Scope(parsedTreeOrTokenRange(this.ast))
    this.scopes = new NonEmptyStack(this.rootScope)
  }

  /** Invoke the given function in a new temporary scope. */
  withNewScopeOver(node: Tree | Token, f: () => undefined) {
    const range = parsedTreeOrTokenRange(node)
    const scope = new Scope(range)
    this.scopes.withPushed(scope, f)
  }

  /** Execute the given function while the given context is temporarily pushed to the stack. */
  withContext(context: Context, f: () => void) {
    return this.contexts.withPushed(context, f)
  }

  /** Marks given token as a binding, i.e. a definition of a new variable. */
  bind(token: Token) {
    const scope = this.scopes.top
    const identifier = readTokenSpan(token, this.code)
    const range = parsedTreeOrTokenRange(token)
    console.debug(`Binding ${identifier}@[${range}]`)
    scope.bindings.set(identifier, token)
    assert(!this.aliases.has(range), `Token at ${range} is already bound.`)
    this.aliases.set(range, new ObjectKeyedSet())
  }

  addConnection(source: Token, target: Token) {
    const sourceRange = parsedTreeOrTokenRange(source)
    const targetRange = parsedTreeOrTokenRange(target)
    const targets = this.aliases.get(sourceRange)
    if (targets != null) {
      console.log(`Usage of ${readTokenSpan(source, this.code)}@[${targetRange}] resolved to [${sourceRange}]`)
      targets.add(targetRange)
    } else {
      console.warn(`No targets found for source range ${sourceRange}`)
    }
  }

  /** Marks given token as a usage, i.e. a reference to an existing variable. */
  use(token: Token) {
    const identifier = readTokenSpan(token, this.code)
    const range = parsedTreeOrTokenRange(token)

    for (const scope of this.scopes.valuesFromTop()) {
      const binding = scope.bindings.get(identifier)
      if (binding != null) {
        this.addConnection(binding, token)
        return
      }
    }

    console.debug(`Usage of ${identifier}@[${range}] is unresolved.`)
    this.unresolvedSymbols.add(range)
  }

  /** Method that processes a single AST node. */
  public process() {
    this.processNode(this.ast)
  }

  /** Method that processes a single AST node. */
  processNode(node?: Tree): void {
    if (node == null) {
      return
    }

    const range = parsedTreeOrTokenRange(node)
    const repr = readAstOrTokenSpan(node, this.code)

    // console.log('\nprocessNode', astPrettyPrintType(node), 'Repr:\n====\n', readAstOrTokenSpan(node, this.code), '\n====\n')

    if (node.type === Tree.Type.BodyBlock) {
      this.withNewScopeOver(node, () => {
        for (const child of childrenAstNodes(node)) {
          this.processNode(child)
        }
      })
    }
    ///
    else if (node.type === Tree.Type.Ident) {
      if (this.contexts.top === Context.Pattern) {
        this.bind(node.token)
      } else {
        this.use(node.token)
      }
    } else if (node.type === Tree.Type.OprApp && node.opr.ok && readAstOrTokenSpan(node.opr.value, this.code) === '->') {
      // Lambda expression.
        this.withNewScopeOver(node, () => {
          this.withContext(Context.Pattern, () => {
            this.processNode(node.lhs)
          })
          this.processNode(node.rhs)
        })
    } else if (node.type === Tree.Type.OprApp && node.opr.ok && readAstOrTokenSpan(node.opr.value, this.code) === '.') {
      // Accessor expression.
      this.processNode(node.lhs)
      // Don't process the right-hand side, as it will be a method call, which is not a variable usage.
    } else if (node.type === Tree.Type.Assignment) {
      this.withContext(Context.Pattern, () => {
        this.processNode(node.pattern)
      })
      this.processNode(node.expr)
    } else if (node.type === Tree.Type.Function) {
      this.withNewScopeOver(node, () => {
        this.withContext(Context.Pattern, () => {
          for (const argument of node.args) {
            this.processNode(argument.pattern)
          }
        })
        this.processNode(node.body)
      })
    } else if (node.type === Tree.Type.Documented) {
      this.processNode(node.expression)
    }
    ///
    else {
      node.visitChildren((child) => {
          if (Tree.isInstance(child)) {
            this.processNode(child)
          }
      })
      console.warn(`Catch-all for ${astPrettyPrintType(node)} at ${range}:\n${repr}\n`)
    }
  }
}
