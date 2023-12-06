import { Ast } from '@/util/ast'
import { zipLongest } from '@/util/iterable'

/** Determine whether an AST matches a specified pattern,
 * where a specified identifier (by default `__`) in the pattern may match any arbitrary subtree. */
export function isMatch(
  target: Ast.Ast | Ast.Token,
  pattern: Ast.Ast | Ast.Token,
  // This SHOULD NOT be `_`, as that would make `_` impossible to match.
  placeholder = '__',
): boolean {
  if (pattern instanceof Ast.Token)
    return target instanceof Ast.Token && target.code() === pattern.code()
  else if (target instanceof Ast.Token) return false
  else if (pattern instanceof Ast.Ident && pattern.repr() === placeholder) return true
  else if (target.typeName() !== pattern.typeName()) return false
  else {
    for (const [nextChild, nextPatternChild] of zipLongest(target.children(), pattern.children())) {
      // By definition, `zipLongest` does not return `[undefined, undefined]`.
      if (!nextChild || !nextPatternChild) return false
      if (!isMatch(nextChild, nextPatternChild)) return false
    }
    return true
  }
}

/** Extract matches from an AST matching a pattern,
 * where a specified identifier (by default `__`) in the pattern may match any arbitrary subtree.
 * Returns an array of the arbitrary subtrees that were matched, if the entire AST matches the entire pattern.
 * Returns an empty array if the pattern contains no placeholder identifiers
 * (as specified above, defaulting to `__`).
 * Returns `undefined` if the AST does not match the pattern. */
export function extractMatches(
  target: Ast.Ast | Ast.Token,
  pattern: Ast.Ast | Ast.Token,
  // This SHOULD NOT be `_`, as that would make `_` impossible to match.
  placeholder = '__',
): Ast.Ast[] | undefined {
  if (pattern instanceof Ast.Token)
    return target instanceof Ast.Token && target.code() === pattern.code() ? [] : undefined
  else if (target instanceof Ast.Token) return undefined
  else if (pattern instanceof Ast.Ident && pattern.code() === placeholder) return [target]
  else if (target.typeName() !== pattern.typeName()) return undefined
  else {
    const matches: Ast.Ast[] = []
    for (const [nextChild, nextPatternChild] of zipLongest(target.children(), pattern.children())) {
      // By definition, `zipLongest` does not return `[undefined, undefined]`.
      if (!nextChild || !nextPatternChild) return undefined
      const childMatches = extractMatches(nextChild, nextPatternChild, placeholder)
      if (!childMatches) return undefined
      matches.push(...childMatches)
    }
    return matches
  }
}
