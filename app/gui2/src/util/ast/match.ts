import { Ast } from '@/util/ast'

export class Pattern {
  private readonly tokenTree: Ast.TokenTree
  private readonly placeholder: string

  constructor(template: Ast.Ast, placeholder: string) {
    this.tokenTree = Ast.tokenTree(template)
    this.placeholder = placeholder
  }

  /** Parse an expression template in which a specified identifier (by default `__`)
   *  may match any arbitrary subtree. */
  static parse(template: string, placeholder: string = '__'): Pattern {
    return new Pattern(Ast.parse(template), placeholder)
  }

  /** If the given expression matches the pattern, return the subtrees that matched the holes in the pattern. */
  match(target: Ast.Ast): Ast.AstId[] | undefined {
    const extracted: Ast.AstId[] = []
    if (isMatch_(this.tokenTree, target.concreteChildren(), target.module, this.placeholder, extracted)) {
      return extracted
    }
  }
}

function isMatch_(pattern: Ast.TokenTree, target: Iterator<Ast.NodeChild>, module: Ast.Module, placeholder: string, extracted: Ast.AstId[]): boolean {
  for (const subpattern of pattern) {
    const next = target.next()
    if (next.done) return false
    const astOrToken = next.value.node
    if (typeof subpattern === 'string' && subpattern !== placeholder) {
      if (astOrToken instanceof Ast.Token) {
        return isMatch_(pattern.slice(1), target, module, placeholder, extracted)
      } else {
        return false
      }
    } else if (astOrToken instanceof Ast.Token) {
      return false
    } else if (typeof subpattern === 'string') {
      extracted.push(astOrToken)
    } else {
      const ast = module.get(astOrToken)
      if (!ast) return false
      if (!isMatch_(subpattern, ast.concreteChildren(), module, placeholder, extracted)) return false
    }
  }
  return true
}
