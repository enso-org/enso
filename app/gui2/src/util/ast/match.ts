import { assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import { MutableModule } from '@/util/ast/abstract'
import { zipLongest } from '@/util/data/iterable'

export class Pattern {
  private readonly template: Ast.Ast
  private readonly placeholders: Ast.AstId[]
  private readonly placeholder: string

  constructor(template: string, placeholder: string) {
    const ast = Ast.parse(template)
    this.template = ast
    this.placeholders = findPlaceholders(ast, placeholder)
    this.placeholder = placeholder
  }

  /** Parse an expression template in which a specified identifier (by default `__`)
   *  may match any arbitrary subtree. */
  static parse(template: string, placeholder: string = '__'): Pattern {
    return new Pattern(template, placeholder)
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

  /** Create a new concrete example of the pattern, with the placeholders replaced with the given subtrees. */
  instantiate(edit: MutableModule, subtrees: Ast.Owned[]): Ast.Owned {
    const template = edit.copy(this.template)
    const placeholders = findPlaceholders(template, this.placeholder).map((ast) => edit.get(ast))
    for (const [placeholder, replacement] of zipLongest(placeholders, subtrees)) {
      assertDefined(placeholder)
      assertDefined(replacement)
      placeholder.replace(replacement)
    }
    return template
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
