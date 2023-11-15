import { Token, Tree } from '@/generated/ast'
import { assert } from '@/util/assert'
import {
  AstExtended,
  astPrettyPrintType,
  parseEnso,
  parsedTreeOrTokenRange,
  readAstOrTokenSpan,
  readTokenSpan,
} from '@/util/ast'

import { MappedKeyMap, MappedSet, NonEmptyStack } from '@/util/containers'
import type { LazyObject } from '@/util/parserSupport.ts'
import { mapIterator } from 'lib0/iterator'
import type { ContentRange, ExprId } from '../../../shared/yjsModel'
import { IdMap, rangeIsBefore } from '../../../shared/yjsModel'

const ACCESSOR_OPERATOR = '.'

const LAMBDA_OPERATOR = '->'

/** Whether the debug logs of the alias analyzer should be enabled.
 *
 * It is recommended to keep them disabled (unless debugging this module), as they are very noisy and can.
 */
const LOGGING_ENABLED = false

class Scope {
  /** The variables defined in this scope. */
  bindings: Map<string, AstExtended<Token>> = new Map()

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
  resolve(identifier: string, location?: ContentRange): AstExtended<Token> | undefined {
    const localBinding = this.bindings.get(identifier)
    if (
      localBinding != null &&
      (location == null || rangeIsBefore(localBinding.span(), location))
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
  readonly unresolvedSymbols = new Set<ExprId>()

  /** The AST representation of the code. */
  readonly ast: AstExtended<Tree>

  /** The special "root" scope that contains the analyzed tree. */
  readonly rootScope: Scope

  /** The stack of scopes, where the top-most scope is the current one. Must never be empty. */
  private readonly scopes: NonEmptyStack<Scope>

  /** The stack for keeping track whether we are in a pattern or expression context. */
  private readonly contexts: NonEmptyStack<Context> = new NonEmptyStack(Context.Expression)

  public readonly aliases = new Map<ExprId, Set<ExprId>>()

  /**
   * @param code text representation of the code.
   * @param ast AST representation of the code. If not provided, it will be parsed from the text.
   */
  constructor(ast: AstExtended<Tree>) {
    this.ast = ast
    this.rootScope = new Scope(this.ast.span())
    this.scopes = new NonEmptyStack(this.rootScope)
  }

  /** Invoke the given function in a new temporary scope. */
  withNewScopeOver(
    nodeOrRange: ContentRange | Tree | Token | AstExtended<Tree | Token>,
    f: () => void,
  ) {
    const range =
      nodeOrRange instanceof AstExtended ? nodeOrRange.span() : parsedTreeOrTokenRange(nodeOrRange)
    const scope = new Scope(range, this.scopes.top)
    this.scopes.withPushed(scope, f)
  }

  /** Execute the given function while the given context is temporarily pushed to the stack. */
  withContext(context: Context, f: () => void) {
    return this.contexts.withPushed(context, f)
  }

  /** Marks given token as a binding, i.e. a definition of a new variable. */
  bind(token: AstExtended<Token>) {
    const scope = this.scopes.top
    const id = token.astId()
    const identifier = token.repr()
    const range = token.span()
    log(() => `Binding ${identifier}@[${range}]`)
    scope.bindings.set(identifier, token)
    assert(!this.aliases.has(id), `Token ${id} at ${range} is already bound.`)
    this.aliases.set(id, new Set())
  }

  addConnection(source: AstExtended<Token>, target: AstExtended<Token>) {
    const sourceRange = source.span()
    const targetRange = target.span()
    const targets = this.aliases.get(source.astId())
    if (targets != null) {
      log(() => `Usage of ${target.repr()}@[${targetRange}] resolved to [${sourceRange}]`)
      targets.add(target.astId())
    } else {
      console.warn(`No targets found for source range ${sourceRange}`)
    }
  }

  /** Marks given token as a usage, i.e. a reference to an existing variable. */
  use(token: AstExtended<Token>) {
    const identifier = token.repr()
    const range = token.span()
    const binding = this.scopes.top.resolve(identifier, range)
    if (binding != null) {
      this.addConnection(binding, token)
    } else {
      log(() => `Usage of ${identifier}@[${range}] is unresolved.`)
      this.unresolvedSymbols.add(token.astId())
    }
  }

  /** Method that processes a single AST node. */
  public process() {
    this.processTree(this.ast)
  }

  processToken(token?: AstExtended<Token>): void {
    if (token == null) {
      return
    }

    const repr = token.repr()
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
  processNodeChildren(node?: AstExtended<Tree>): void {
    if (node == null) return
    for (const child of node.children()) {
      if (child.isTree()) {
        this.processTree(child)
      } else if (child.isToken()) {
        this.processToken(child)
      }
    }
  }

  /** Method that processes a single AST node. */
  processTree(node?: AstExtended<Tree>): void {
    if (node == null) {
      return
    }

    log(() => `\nprocessNode ${astPrettyPrintType(node)}\n====\n${node.repr()}\n====\n`)

    // We may replace this with switch(true) once
    // https://github.com/microsoft/TypeScript/pull/53681 will be released.
    // I think it may be more readable.
    if (node.isTree(Tree.Type.BodyBlock)) {
      this.withNewScopeOver(node.inner, () => {
        node.children
        this.processNodeChildren(node)
      })
    } else if (node.isTree(Tree.Type.NamedApp)) {
      this.processTree(node.map((app) => app.func))
      // Intentionally omit name, as it is not a variable usage.
      this.processTree(node.map((app) => app.arg))
    } else if (node.isTree(Tree.Type.DefaultApp)) {
      this.processTree(node.map((app) => app.func))
      // Intentionally omit `default` keyword, because it is a keyword, not a variable usage.
    } else if (node.isTree(Tree.Type.OprApp)) {
      const oprToken = node.tryMap((app) => (app.opr.ok ? app.opr.value : null))
      const opr = oprToken?.repr()
      switch (opr) {
        case LAMBDA_OPERATOR:
          // Lambda expression. Left-hand side is a pattern, right-hand side is an expression. Introduces a new scope.
          // Note that this is not a Tree.Type.Lambda, as that is for "new" lambdas syntax, like `\x -> x`.
          this.withNewScopeOver(node, () => {
            this.withContext(Context.Pattern, () => {
              this.processTree(node.tryMap((app) => app.lhs))
            })
            this.processTree(node.tryMap((app) => app.rhs))
          })
          break
        case ACCESSOR_OPERATOR:
          this.processTree(node.tryMap((app) => app.lhs))
          // Don't process the right-hand side, as it will be a method call, which is not a variable usage.
          break
        default:
          this.processNodeChildren(node)
          break
      }
    } else if (node.isTree(Tree.Type.MultiSegmentApp)) {
      // Intentionally omit segments' headers, as their are not a variable usage.
      const segments = node.visit((app) => app.segments)
      for (const segment of segments) {
        this.processTree(segment.tryMap((seg) => seg.body))
      }
    } else if (node.isTree(Tree.Type.Assignment)) {
      this.withContext(Context.Pattern, () => {
        this.processTree(node.map((assign) => assign.pattern))
      })
      this.processTree(node.map((assign) => assign.expr))
    } else if (node.isTree(Tree.Type.Function)) {
      // Function name goes to the current scope, unlike its arguments.
      this.withContext(Context.Pattern, () => {
        this.processTree(node.map((f) => f.name))
      })
      this.withNewScopeOver(node, () => {
        for (const argument of node.visit((f) => f.args)) {
          this.withContext(Context.Pattern, () => {
            this.processTree(argument.map((arg) => arg.pattern))
          })
          this.processTree(argument.tryMap((arg) => arg.default?.expression))
        }
        this.processTree(node.tryMap((f) => f.body))
      })
    } else if (node.isTree(Tree.Type.CaseOf)) {
      this.processTree(node.tryMap((caseOf) => caseOf.expression))
      for (const caseLine of node.visit((caseOf) => caseOf.cases)) {
        const pattern = caseLine.tryMap((line) => line.case?.pattern)
        const arrow = caseLine.tryMap((line) => line.case?.arrow)
        const expression = caseLine.tryMap((line) => line.case?.expression)
        if (pattern) {
          const armStart = pattern.span()[0]
          const armEnd = expression
            ? expression.span()[1]
            : arrow
            ? arrow.span()[1]
            : pattern.span()[1]

          const armRange: ContentRange = [armStart, armEnd]
          this.withNewScopeOver(armRange, () => {
            this.withContext(Context.Pattern, () => {
              this.processTree(pattern)
            })
            this.processTree(expression)
          })
        }
      }
    } else if (node.isTree(Tree.Type.Documented)) {
      // Intentionally omit documentation, as it is not a "real" code.
      this.processTree(node.tryMap((documented) => documented.expression))
    } else {
      log(() => `Catch-all for ${astPrettyPrintType(node)} at ${node.span()}:\n${node.repr()}\n`)
      this.processNodeChildren(node)
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
