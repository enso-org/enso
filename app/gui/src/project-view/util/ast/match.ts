import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import { zipLongest } from '@/util/data/iterable'

/** TODO: Add docs */
export class Pattern {
  private readonly template: Ast.Ast
  private readonly placeholders: Ast.AstId[]
  private readonly placeholder: string

  private constructor(template: Ast.Owned, placeholder: string) {
    this.template = template
    this.placeholders = findPlaceholders(template, placeholder)
    this.placeholder = placeholder
  }

  /**
   * Parse an expression template in which a specified identifier (by default `__`)
   *  may match any arbitrary subtree.
   */
  static parse(template: string, placeholder: string = '__'): Pattern {
    const ast = Ast.parse(template)
    return new Pattern(ast, placeholder)
  }

  /** TODO: Add docs */
  static new(f: (placeholder: Ast.Owned) => Ast.Owned, placeholder: string = '__'): Pattern {
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
  instantiate(edit: Ast.MutableModule, subtrees: Ast.Owned[]): Ast.Owned {
    const template = edit.copy(this.template)
    const placeholders = findPlaceholders(template, this.placeholder).map((ast) => edit.tryGet(ast))
    for (const [placeholder, replacement] of zipLongest(placeholders, subtrees)) {
      assertDefined(placeholder)
      assertDefined(replacement)
      placeholder.replace(replacement)
    }
    return template
  }

  /** TODO: Add docs */
  instantiateCopied(subtrees: Ast.Ast[], edit?: Ast.MutableModule): Ast.Owned {
    const module = edit ?? Ast.MutableModule.Transient()
    return this.instantiate(
      module,
      subtrees.map((ast) => module.copy(ast)),
    )
  }

  /** TODO: Add docs */
  compose(f: (pattern: Ast.Owned) => Ast.Owned): Pattern {
    const module = Ast.MutableModule.Transient()
    return new Pattern(f(module.copy(this.template)), this.placeholder)
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
  for (const [patternNode, targetNode] of zipLongest(pattern.children(), target.children())) {
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
