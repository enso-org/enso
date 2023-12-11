import { Ast } from '@/util/ast'
import { type MutableModule } from '@/util/ast/abstract'

class Pattern {
  private readonly tokenTree: Ast.TokenTree

  constructor(template: string) {
    this.tokenTree = Ast.tokenTree(Ast.parse(template))
  }

  match(target: Ast.Ast): Ast.AstId[] | null {
    const extracted: Ast.AstId[] = []
    if (isMatch_(this.tokenTree, target.concreteChildren(), target.module, extracted)) {
      return extracted
    } else {
      return null
    }
  }
}

const PLACEHOLDER = '__'

/** Determine whether an AST matches a specified pattern,
 * where a specified identifier (by default `__`) in the pattern may match any arbitrary subtree. */
function isMatch_(pattern: Ast.TokenTree, target: Iterator<Ast.NodeChild>, module: Ast.Module, extracted: Ast.AstId[]): boolean {
  for (const subpattern of pattern) {
    const next = target.next()
    if (next.done) return false
    const astOrToken = next.value.node
    if (typeof subpattern === 'string' && subpattern !== PLACEHOLDER) {
      if (astOrToken instanceof Ast.Token) {
        return isMatch_(pattern.slice(1), target, module, extracted)
      } else {
        return false
      }
    } else if (astOrToken instanceof Ast.Token) {
      return false
    }
    if (subpattern === PLACEHOLDER) {
      extracted.push(astOrToken)
    } else {
      const ast = module.get(astOrToken)
      if (!ast) return false
      if (!isMatch_(subpattern, ast.concreteChildren(), module, extracted)) return false
    }
  }
  return true
}

export function replaceMatches(
  pattern: Ast.Ast,
  replacements: (Ast.Ast | string)[],
  // This SHOULD NOT be `_`, as that would make `_` impossible to match.
  placeholder = '__',
) {
  const replaced = replaceMatchesInternal(
    pattern,
    replacements,
    pattern.module.edit(),
    placeholder,
  )
  if (replacements.length !== 0) {
    // throw new Error(`${replacements.length} unused replacements.`)
  }
  return replaced
}
