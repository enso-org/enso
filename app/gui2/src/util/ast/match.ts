import { Tree, type Token } from '@/generated/ast'
import type { AstExtended } from '@/util/ast'

export function isMatch(
  target: AstExtended<Tree.Tree | Token.Token, boolean>,
  pattern: AstExtended<Tree.Tree | Token.Token, boolean>,
  // This MUST NOT be `_`, as that would make it impossible to match.
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
