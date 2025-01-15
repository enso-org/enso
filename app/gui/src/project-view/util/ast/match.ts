import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import * as iter from 'enso-common/src/utilities/data/iter'

/**
 * A pattern is an AST object with "placeholder" expressions.
 *
 * It can be used in two ways:
 * - It can be matched against an AST node, in which case each placeholder will match any expression, and the matches
 *   will be returned.
 * - It can be instantiated, by providing an expression to be substituted for each placeholder.
 */
export class Pattern<T extends Ast.Ast = Ast.Expression> {
  private readonly template: T
  private readonly placeholders: Ast.AstId[]
  private readonly placeholder: string

  private constructor(template: Ast.Owned<Ast.Mutable<T>>, placeholder: string) {
    this.template = Ast.dropMutability(template)
    this.placeholders = findPlaceholders(template, placeholder)
    this.placeholder = placeholder
  }

  /**
   * Parse an expression template in which a specified identifier (by default `__`)
   *  may match any arbitrary subtree.
   */
  static parseExpression(template: string, placeholder: string = '__'): Pattern {
    const ast = Ast.parseExpression(template)
    assertDefined(ast)
    return new Pattern(ast, placeholder)
  }

  /**
   * Given a function that constructs an AST value when provided an expression, creates a `Pattern` that constructs an
   * equivalent AST value when instantiated with an expression.
   */
  static new<T extends Ast.Ast>(
    f: (placeholder: Ast.Owned<Ast.MutableExpression>) => Ast.Owned<Ast.Mutable<T>>,
    placeholder: string = '__',
  ): Pattern<T> {
    assert(Ast.isIdentifier(placeholder))
    const module = Ast.MutableModule.Transient()
    return new Pattern(f(Ast.Ident.new(module, placeholder)), placeholder)
  }

  /** If the given expression matches the pattern, return the subtrees that matched the holes in the pattern. */
  match(target: Ast.Ast): Ast.AstId[] | undefined {
    const placeholders = this.placeholders.map((placeholder) => ({ placeholder, match: undefined }))
    if (!matchSubtree(this.template, target, placeholders)) return
    const matches: Ast.AstId[] = []
    for (const placeholder of placeholders) {
      if (!placeholder.match) return
      matches.push(placeholder.match)
    }
    return matches
  }

  /** Check if the given expression matches the pattern */
  test(target: Ast.Ast): boolean {
    return this.match(target) != null
  }

  /** Create a new concrete example of the pattern, with the placeholders replaced with the given subtrees. */
  instantiate(
    edit: Ast.MutableModule,
    subtrees: Ast.Owned<Ast.MutableExpression>[],
  ): Ast.Owned<Ast.Mutable<T>> {
    const template = edit.copy(this.template)
    const placeholders = findPlaceholders(template, this.placeholder).map((ast) => edit.tryGet(ast))
    for (const [placeholder, replacement] of iter.zipLongest(placeholders, subtrees)) {
      assertDefined(placeholder)
      assertDefined(replacement)
      placeholder.replace(replacement)
    }
    return template
  }

  /**
   * Helper that creates the AST described by the pattern, as {@link instantiate}, after first copying each of the
   * referenced subtrees into a different module.
   */
  instantiateCopied(
    subtrees: (Ast.Expression | Ast.MutableExpression)[],
    edit?: Ast.MutableModule,
  ): Ast.Owned<Ast.Mutable<T>> {
    const module = edit ?? Ast.MutableModule.Transient()
    return this.instantiate(
      module,
      subtrees.map((ast) => module.copy(ast)),
    )
  }
}

function findPlaceholders(ast: Ast.Ast, placeholder: string): Ast.AstId[] {
  const placeholders: Ast.AstId[] = []
  ast.visitRecursive((child) => {
    if (child instanceof Ast.Ident && child.code() === placeholder) placeholders.push(child.id)
  })
  return placeholders
}

type PlaceholderMatch = {
  placeholder: Ast.AstId
  match: Ast.AstId | undefined
}

function matchSubtree(
  pattern: Ast.Ast,
  target: Ast.Ast,
  placeholders: PlaceholderMatch[],
): boolean {
  if (pattern instanceof Ast.Ident) {
    for (const placeholder of placeholders) {
      if (pattern.id === placeholder.placeholder) {
        placeholder.match = target.id
        return true
      }
    }
  }
  for (const [patternNode, targetNode] of iter.zipLongest(pattern.children(), target.children())) {
    if (!patternNode || !targetNode) return false
    if (patternNode instanceof Ast.Token && targetNode instanceof Ast.Token) {
      if (patternNode.code() !== targetNode.code()) return false
    } else if (patternNode instanceof Ast.Ast && targetNode instanceof Ast.Ast) {
      if (!matchSubtree(patternNode, targetNode, placeholders)) return false
    } else {
      return false
    }
  }
  return true
}
