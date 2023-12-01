import { Tree, type Token } from '@/generated/ast'
import type { AstExtended } from '@/util/ast'

export function isMatch(
  target: AstExtended<Tree.Tree | Token.Token, boolean>,
  pattern: AstExtended<Tree.Tree | Token.Token, boolean>,
  // This SHOULD NOT be `_`, as that would make `_` impossible to match.
  patternPlaceholderIdentifier = '__',
): boolean {
  if (pattern.isToken()) return target.isToken() && target.repr() === pattern.repr()
  else if (pattern.isTree(Tree.Type.Ident) && pattern.repr() === patternPlaceholderIdentifier)
    return true
  else if (target.treeTypeName() !== pattern.treeTypeName()) return false
  else {
    const children = target.children()
    const patternChildren = pattern.children()
    return (
      children.length === patternChildren.length &&
      children.every((child, i) =>
        isMatch(child, patternChildren[i]!, patternPlaceholderIdentifier),
      )
    )
  }
}

export function extractMatches<HasIdMap extends boolean = true>(
  target: AstExtended<Tree.Tree | Token.Token, HasIdMap>,
  pattern: AstExtended<Tree.Tree | Token.Token, HasIdMap>,
  // This SHOULD NOT be `_`, as that would make `_` impossible to match.
  patternPlaceholderIdentifier = '__',
): AstExtended<Tree.Tree | Token.Token, HasIdMap>[] | undefined {
  if (pattern.isToken())
    return target.isToken() && target.repr() === pattern.repr() ? [] : undefined
  else if (pattern.isTree(Tree.Type.Ident) && pattern.repr() === patternPlaceholderIdentifier)
    return [target]
  else if (target.treeTypeName() !== pattern.treeTypeName()) return undefined
  else {
    const children = target.children()
    const patternChildren = pattern.children()
    const matches: AstExtended<Tree.Tree | Token.Token, HasIdMap>[] = []
    for (let i = 0; i < children.length; i += 1) {
      const childMatches = extractMatches(
        children[i]!,
        patternChildren[i]!,
        patternPlaceholderIdentifier,
      )
      if (!childMatches) return undefined
      matches.push(...childMatches)
    }
    return matches
  }
}
