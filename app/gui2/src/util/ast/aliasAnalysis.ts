import { assert } from '@/util/assert'
import {
  RawAst,
  astPrettyPrintType,
  parseEnso,
  parsedTreeOrTokenRange,
  readAstOrTokenSpan,
  readTokenSpan,
} from '@/util/ast'
import { MappedKeyMap, MappedSet, NonEmptyStack } from '@/util/containers'
import type { LazyObject } from '@/util/parserSupport'
import { IdMap, rangeIsBefore, type ContentRange } from 'shared/yjsModel'

const ACCESSOR_OPERATOR = '.'

const LAMBDA_OPERATOR = '->'

/** Whether the debug logs of the alias analyzer should be enabled.
 *
 * It is recommended to keep them disabled (unless debugging this module), as they are very noisy and can.
 */
const LOGGING_ENABLED = false

class Scope {
  /** The variables defined in this scope. */
  bindings: Map<string, RawAst.Token> = new Map()

  /** Construct a new scope for the given range. If the parent scope is provided, the new scope will be added to its
   * children.
   *
   * @param range The range of the code that is covered by this scope.
   * @param parent The parent scope.
   */
  constructor(
    public range?: ContentRange,
    public parent?: Scope,
  ) {}

  /** Resolve the given identifier to a token that defines it.
   *
   * @param identifier The identifier to resolve.
   * @param location The location of the usage of the identifier. It affects visibility of the bindings within this
   * scope, so the variables are not visible before they are defined. If not provided, the lookup will include all
   * symbols from the scope.
   */
  resolve(identifier: string, location?: ContentRange): RawAst.Token | undefined {
    const localBinding = this.bindings.get(identifier)
    if (
      localBinding != null &&
      (location == null || rangeIsBefore(parsedTreeOrTokenRange(localBinding), location))
    ) {
      return localBinding
    } else if (this.parent != null) {
      return this.parent.resolve(identifier, location)
    } else {
      return undefined
    }
  }
}

/** Context tells how the variables are to be treated. */
enum Context {
  Pattern = 'Pattern',
  Expression = 'Expression',
}

export enum IdentifierType {
  Operator = 'Operator',
  Type = 'Type',
  TypeVariable = 'TypeVariable',
  Variable = 'Variable',
}

/** Check, what kind of identifier the given string is.
 *
 *  Note that the results should not be relied upon for ill-formed identifiers.
 */
export function identifierKind(identifier: string): IdentifierType {
  // Identifier kinds, as per draft Enso spec:
  // https://github.com/enso-org/design/blob/wip/wd/enso-spec/epics/enso-spec-1.0/03.%20Code%20format%20and%20layout.md
  // Regex that matches any character that is allowed as part of operator identifier.
  const operatorCharacter = /[!$%&*+\-/<>^~|:\\=.]/
  const firstCharacter = identifier.charAt(0)
  if (firstCharacter.match(operatorCharacter)) {
    return IdentifierType.Operator
  } else if (firstCharacter === '_') {
    return IdentifierType.TypeVariable
  } else if (firstCharacter === firstCharacter.toUpperCase()) {
    return IdentifierType.Type
  } else {
    return IdentifierType.Variable
  }
}

export class AliasAnalyzer {
  /** All symbols that are not yet resolved (i.e. that were not bound in the analyzed tree). */
  readonly unresolvedSymbols = new MappedSet<ContentRange>(IdMap.keyForRange)

  /** The AST representation of the code. */
  readonly ast: RawAst.Tree

  /** The special "root" scope that contains the analyzed tree. */
  readonly rootScope: Scope

  /** The stack of scopes, where the top-most scope is the current one. Must never be empty. */
  private readonly scopes: NonEmptyStack<Scope>

  /** The stack for keeping track whether we are in a pattern or expression context. */
  private readonly contexts: NonEmptyStack<Context> = new NonEmptyStack(Context.Expression)

  public readonly aliases = new MappedKeyMap<ContentRange, MappedSet<ContentRange>>(
    IdMap.keyForRange,
  )

  /**
   * @param code text representation of the code.
   * @param ast AST representation of the code. If not provided, it will be parsed from the text.
   */
  constructor(
    private readonly code: string,
    ast?: RawAst.Tree,
  ) {
    this.ast = ast ?? parseEnso(code)
    this.rootScope = new Scope(parsedTreeOrTokenRange(this.ast))
    this.scopes = new NonEmptyStack(this.rootScope)
  }

  /** Invoke the given function in a new temporary scope. */
  withNewScopeOver(nodeOrRange: ContentRange | RawAst.Tree | RawAst.Token, f: () => undefined) {
    const range = parsedTreeOrTokenRange(nodeOrRange)
    const scope = new Scope(range, this.scopes.top)
    this.scopes.withPushed(scope, f)
  }

  /** Execute the given function while the given context is temporarily pushed to the stack. */
  withContext(context: Context, f: () => void) {
    return this.contexts.withPushed(context, f)
  }

  /** Marks given token as a binding, i.e. a definition of a new variable. */
  bind(token: RawAst.Token) {
    const scope = this.scopes.top
    const identifier = readTokenSpan(token, this.code)
    const range = parsedTreeOrTokenRange(token)
    log(() => `Binding ${identifier}@[${range}]`)
    scope.bindings.set(identifier, token)
    assert(!this.aliases.has(range), `Token at ${range} is already bound.`)
    this.aliases.set(range, new MappedSet<ContentRange>(IdMap.keyForRange))
  }

  addConnection(source: RawAst.Token, target: RawAst.Token) {
    const sourceRange = parsedTreeOrTokenRange(source)
    const targetRange = parsedTreeOrTokenRange(target)
    const targets = this.aliases.get(sourceRange)
    if (targets != null) {
      log(
        () =>
          `Usage of ${readTokenSpan(
            source,
            this.code,
          )}@[${targetRange}] resolved to [${sourceRange}]`,
      )
      targets.add(targetRange)
    } else {
      console.warn(`No targets found for source range ${sourceRange}`)
    }
  }

  /** Marks given token as a usage, i.e. a reference to an existing variable. */
  use(token: RawAst.Token) {
    const identifier = readTokenSpan(token, this.code)
    const range = parsedTreeOrTokenRange(token)
    const binding = this.scopes.top.resolve(identifier, range)
    if (binding != null) {
      this.addConnection(binding, token)
    } else {
      log(() => `Usage of ${identifier}@[${range}] is unresolved.`)
      this.unresolvedSymbols.add(range)
    }
  }

  /** Method that processes a single AST node. */
  public process() {
    this.processTree(this.ast)
  }

  processToken(token?: RawAst.Token): void {
    if (token == null) {
      return
    }

    const repr = readTokenSpan(token, this.code)
    if (identifierKind(repr) === IdentifierType.Variable) {
      if (this.contexts.top === Context.Pattern) {
        this.bind(token)
      } else {
        this.use(token)
      }
    }
  }

  /** Process given AST node, assuming it does not change the alias analysis context.
   *
   * All AST children will be processed recursively.
   */
  processNodeChildren(node?: LazyObject): void {
    if (node == null) {
      return
    }

    node.visitChildren((child) => {
      if (RawAst.Tree.isInstance(child)) {
        this.processTree(child)
      } else if (RawAst.Token.isInstance(child)) {
        this.processToken(child)
      } else {
        this.processNode(child)
      }
    })
  }

  processNode(node?: LazyObject): void {
    if (node == null) {
      return
    }
    if (RawAst.Tree.isInstance(node)) {
      this.processTree(node)
    } else if (RawAst.Token.isInstance(node)) {
      this.processToken(node)
    } else {
      node.visitChildren((child) => {
        this.processNode(child)
      })
    }
  }

  /** Method that processes a single AST node. */
  processTree(node?: RawAst.Tree): void {
    if (node == null) {
      return
    }

    const range = () => parsedTreeOrTokenRange(node)
    const repr = () => readAstOrTokenSpan(node, this.code)

    log(() => `\nprocessNode ${astPrettyPrintType(node)}\n====\n${repr()}\n====\n`)

    switch (node.type) {
      case RawAst.Tree.Type.BodyBlock:
        this.withNewScopeOver(node, () => {
          this.processNodeChildren(node)
        })
        break
      case RawAst.Tree.Type.NamedApp:
        this.processTree(node.func)
        // Intentionally omit name, as it is not a variable usage.
        this.processTree(node.arg)
        break
      case RawAst.Tree.Type.OprApp: {
        const opr = node.opr.ok ? readAstOrTokenSpan(node.opr.value, this.code) : ''
        switch (opr) {
          case LAMBDA_OPERATOR:
            // Lambda expression. Left-hand side is a pattern, right-hand side is an expression. Introduces a new scope.
            // Note that this is not a RawAst.Tree.Type.Lambda, as that is for "new" lambdas syntax, like `\x -> x`.
            this.withNewScopeOver(node, () => {
              this.withContext(Context.Pattern, () => {
                this.processTree(node.lhs)
              })
              this.processTree(node.rhs)
            })
            break
          case ACCESSOR_OPERATOR:
            this.processTree(node.lhs)
            // Don't process the right-hand side, as it will be a method call, which is not a variable usage.
            break
          default:
            this.processNodeChildren(node)
            break
        }
        break
      }
      case RawAst.Tree.Type.MultiSegmentApp:
        for (const segment of node.segments) {
          // Intentionally omit segment header, as it is not a variable usage.
          this.processTree(segment.body)
        }
        break
      case RawAst.Tree.Type.Assignment:
        this.withContext(Context.Pattern, () => {
          this.processTree(node.pattern)
        })
        this.processTree(node.expr)
        break
      case RawAst.Tree.Type.Function:
        // Function name goes to the current scope, unlike its arguments.
        this.withContext(Context.Pattern, () => {
          this.processTree(node.name)
        })
        this.withNewScopeOver(node, () => {
          for (const argument of node.args) {
            this.withContext(Context.Pattern, () => {
              this.processTree(argument.pattern)
            })
            this.processTree(argument.default?.expression)
          }
          this.processTree(node.body)
        })
        break
      case RawAst.Tree.Type.CaseOf:
        this.processTree(node.expression)
        for (const caseLine of node.cases) {
          const pattern = caseLine.case?.pattern
          const arrow = caseLine.case?.arrow
          const expression = caseLine.case?.expression
          if (pattern) {
            const armStart = parsedTreeOrTokenRange(pattern)[0]
            const armEnd = expression
              ? parsedTreeOrTokenRange(expression)[1]
              : arrow
              ? parsedTreeOrTokenRange(arrow)[1]
              : parsedTreeOrTokenRange(pattern)[1]

            const armRange: ContentRange = [armStart, armEnd]
            this.withNewScopeOver(armRange, () => {
              this.withContext(Context.Pattern, () => {
                this.processTree(caseLine.case?.pattern)
              })
              this.processTree(caseLine.case?.expression)
            })
          }
        }
        break
      case RawAst.Tree.Type.Documented:
        // Intentionally omit documentation, as it is not a "real" code.
        this.processTree(node.expression)
        break
      default:
        log(() => `Catch-all for ${astPrettyPrintType(node)} at ${range()}:\n${repr()}\n`)
        this.processNodeChildren(node)
        break
    }
  }
}

// ===========
// === LOG ===
// ===========

/** Provisional logging function. Delegates to `console.log` if {@link LOGGING_ENABLED} is `true`.
 *
 * @param messages The messages to log. They are functions, so that they are only evaluated if logging is enabled.
 **/
function log(...messages: Array<() => any>) {
  if (LOGGING_ENABLED ?? false) {
    console.log(...messages.map((message) => message()))
  }
}
