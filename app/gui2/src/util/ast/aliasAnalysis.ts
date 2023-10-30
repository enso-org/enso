import { TextElement, Token, Tree } from '@/generated/ast'
import { assert } from '@/util/assert'
import {
  astPrettyPrintType,
  childrenAstNodes,
  parseEnso,
  parsedTreeOrTokenRange,
  readAstOrTokenSpan,
  readTokenSpan,
} from '@/util/ast'

import { NonEmptyStack, ObjectKeyedMap, ObjectKeyedSet } from '@/util/containers'
import type { ContentRange } from '../../../shared/yjsModel'

/** Whether the debug logs of the alias analyzer should be enabled.
 *
 * It is recommended to keep them disabled (unless debugging this module), as they are very noisy and can.
 */
const LOGGING_ENABLED = true

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
    public range?: ContentRange,
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

  public readonly aliases: ObjectKeyedMap<ContentRange, ObjectKeyedSet<ContentRange>> =
    new ObjectKeyedMap()

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
  withNewScopeOver(nodeOrRange: ContentRange | Tree | Token, f: () => undefined) {
    const range = parsedTreeOrTokenRange(nodeOrRange)
    const scope = new Scope(range, this.scopes.top)
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
    log(`Binding ${identifier}@[${range}]`)
    scope.bindings.set(identifier, token)
    assert(!this.aliases.has(range), `Token at ${range} is already bound.`)
    this.aliases.set(range, new ObjectKeyedSet())
  }

  addConnection(source: Token, target: Token) {
    const sourceRange = parsedTreeOrTokenRange(source)
    const targetRange = parsedTreeOrTokenRange(target)
    const targets = this.aliases.get(sourceRange)
    if (targets != null) {
      log(
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

    log(`Usage of ${identifier}@[${range}] is unresolved.`)
    this.unresolvedSymbols.add(range)
  }

  /** Method that processes a single AST node. */
  public process() {
    this.processNode(this.ast)
  }

  processToken(token?: Token): void {
    if (token == null) {
      return
    }

    const repr = readTokenSpan(token, this.code)

    // We require first character to be lowercase, to avoid working with type names.
    if (repr.length > 0 && repr.charAt(0) !== repr.charAt(0).toUpperCase()) {
      if (this.contexts.top === Context.Pattern) {
        this.bind(token)
      } else {
        this.use(token)
      }
    }
  }

  /** Method that processes a single AST node. */
  processNode(node?: Tree): void {
    if (node == null) {
      return
    }

    const range = parsedTreeOrTokenRange(node)
    const repr = readAstOrTokenSpan(node, this.code)

    log(
      '\nprocessNode',
      astPrettyPrintType(node),
      '\n====\n',
      readAstOrTokenSpan(node, this.code),
      '\n====\n',
    )

    if (node.type === Tree.Type.BodyBlock) {
      this.withNewScopeOver(node, () => {
        for (const child of childrenAstNodes(node)) {
          this.processNode(child)
        }
      })
    } else if (node.type === Tree.Type.Ident) {
      this.processToken(node.token)
    } else if (node.type === Tree.Type.TextLiteral) {
      for (const element of node.elements) {
        if (element.type === TextElement.Type.Splice) {
          this.processNode(element.expression)
        }
      }
    } else if (node.type === Tree.Type.NamedApp) {
      this.processNode(node.func)
      // Intentionally omit name, as it is not a variable usage.
      this.processNode(node.arg)
    } else if (node.type === Tree.Type.DefaultApp) {
      this.processNode(node.func)
      // Intentionally omit `default` keyword, because it is a keyword, not a variable usage.
    } else if (
      node.type === Tree.Type.OprApp &&
      node.opr.ok &&
      readAstOrTokenSpan(node.opr.value, this.code) === '->'
    ) {
      // Lambda expression.
      // Note that this is not a Tree.Type.Lambda, as that is for "new" lambdas syntax, like `\x -> x`.
      this.withNewScopeOver(node, () => {
        this.withContext(Context.Pattern, () => {
          this.processNode(node.lhs)
        })
        this.processNode(node.rhs)
      })
    } else if (
      node.type === Tree.Type.OprApp &&
      node.opr.ok &&
      readAstOrTokenSpan(node.opr.value, this.code) === '.'
    ) {
      // Accessor expression.
      this.processNode(node.lhs)
      // Don't process the right-hand side, as it will be a method call, which is not a variable usage.
    } else if (node.type === Tree.Type.MultiSegmentApp) {
      for (const segment of node.segments) {
        this.processNode(segment.body)
      }
    } else if (node.type === Tree.Type.Assignment) {
      this.withContext(Context.Pattern, () => {
        this.processNode(node.pattern)
      })
      this.processNode(node.expr)
    } else if (node.type === Tree.Type.Function) {
      this.withContext(Context.Pattern, () => {
        this.processNode(node.name)
      })
      this.withNewScopeOver(node, () => {
        for (const argument of node.args) {
          this.withContext(Context.Pattern, () => {
            this.processNode(argument.pattern)
          })
          this.processNode(argument.default?.expression)
        }
        this.processNode(node.body)
      })
    } else if (node.type === Tree.Type.CaseOf) {
      this.processNode(node.expression)
      for (const caseLine of node.cases) {
        if (caseLine.case?.pattern) {
          this.withNewScopeOver(caseLine.case?.expression ?? caseLine.case?.pattern, () => {
            this.withContext(Context.Pattern, () => {
              this.processNode(caseLine.case?.pattern)
            })
            this.processNode(caseLine.case?.expression)
          })
        }
      }
    } else if (node.type === Tree.Type.Documented) {
      this.processNode(node.expression)
    } else {
      node.visitChildren((child) => {
        if (Tree.isInstance(child)) {
          this.processNode(child)
        } else if (Token.isInstance(child)) {
          this.processToken(child)
        }
      })
      log(`Catch-all for ${astPrettyPrintType(node)} at ${range}:\n${repr}\n`)
    }
  }
}

// ===========
// === LOG ===
// ===========
function log(...args: any[]) {
  if (LOGGING_ENABLED ?? false) {
    console.log(...args)
  }
}
