import { Tree } from '@/generated/ast'
import { Ast } from '@/util/ast'
import { type TemporaryEdit } from '@/util/ast/abstract'
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

export function replaceMatchesInternal(
  pattern: Ast.Ast,
  replacements: (Ast.Ast | string)[],
  edit: TemporaryEdit,
  // This SHOULD NOT be `_`, as that would make `_` impossible to match.
  placeholder = '__',
): Ast.Ast {
  if (!replacements.length) return pattern
  if (pattern instanceof Ast.Ident && pattern.code() === placeholder) {
    let replacement = replacements.shift()!
    if (typeof replacement === 'string') {
      replacement = Ast.parseLine(replacement)
    }
    edit.splice(replacement)
    return replacement
  } else {
    function replaceNodeChild(node: Ast.NodeChild) {
      return node.node instanceof Ast.Token
        ? node
        : {
            ...node,
            node: replaceMatchesInternal(
              pattern.module.get(node.node)!,
              replacements,
              edit,
              placeholder,
            ).astId,
          }
    }
    switch (pattern.treeType) {
      case undefined: {
        if (!(pattern instanceof Ast.RawCode)) {
          throw new Error('AST node has unknown type.')
        }
        return pattern
      }
      case Tree.Type.App: {
        const typedAst = pattern as Ast.App
        const cloned = new Ast.App(
          edit,
          typedAst.astId,
          typedAst._func,
          typedAst._leftParen,
          typedAst._argumentName,
          typedAst._equals,
          typedAst._arg,
          typedAst._rightParen,
          typedAst.treeType!,
        )
        const function_ =
          typedAst.function &&
          replaceMatchesInternal(typedAst.function, replacements, edit, placeholder)
        if (function_) cloned._func = { ...cloned._func, node: function_.astId }
        const argument =
          typedAst.argument &&
          replaceMatchesInternal(typedAst.argument, replacements, edit, placeholder)
        if (argument) cloned._arg = { ...cloned._arg, node: argument.astId }
        return cloned
      }
      case Tree.Type.OprApp: {
        const typedAst = pattern as Ast.OprApp
        const cloned = new Ast.OprApp(
          edit,
          typedAst.astId,
          typedAst._lhs,
          typedAst._opr,
          typedAst._rhs,
        )
        const lhs =
          typedAst.lhs && replaceMatchesInternal(typedAst.lhs, replacements, edit, placeholder)
        if (lhs) cloned._lhs = { ...cloned._lhs, node: lhs.astId }
        const rhs =
          typedAst.rhs && replaceMatchesInternal(typedAst.rhs, replacements, edit, placeholder)
        if (rhs) cloned._rhs = { ...cloned._rhs, node: rhs.astId }
        return cloned
      }
      case Tree.Type.Number: {
        const typedAst = pattern as Ast.NumericLiteral
        const cloned = new Ast.NumericLiteral(edit, typedAst.astId, typedAst._tokens)
        typedAst._tokens = typedAst._tokens.map(replaceNodeChild)
        return cloned
      }
      case Tree.Type.Function: {
        const typedAst = pattern as Ast.Function
        const cloned = new Ast.Function(
          edit,
          typedAst.astId,
          typedAst._name,
          typedAst._args,
          typedAst._equals,
          typedAst._body,
        )
        const name =
          typedAst.name && replaceMatchesInternal(typedAst.name, replacements, edit, placeholder)
        if (name) cloned._name = { ...cloned._name, node: name.astId }
        typedAst._args = typedAst._args.map((arg) => arg.map(replaceNodeChild))
        return cloned
      }
      case Tree.Type.Assignment: {
        const typedAst = pattern as Ast.Assignment
        const cloned = new Ast.Assignment(
          edit,
          typedAst.astId,
          typedAst._pattern,
          typedAst._equals,
          typedAst._expression,
        )
        const childPattern =
          typedAst.pattern &&
          replaceMatchesInternal(typedAst.pattern, replacements, edit, placeholder)
        if (childPattern) cloned._pattern = { ...cloned._pattern, node: childPattern.astId }
        const expression =
          typedAst.expression &&
          replaceMatchesInternal(typedAst.expression, replacements, edit, placeholder)
        if (expression) cloned._expression = { ...cloned._expression, node: expression.astId }
        return cloned
      }
      case Tree.Type.Ident:
      case Tree.Type.Wildcard: {
        // These node types contain no subtrees.
        return pattern
      }
      case Tree.Type.BodyBlock: {
        // These node types contain no *exposed* subtrees.
        return pattern
      }
    }
  }
  // FIXME:
  console.warn('Unhandled node type:', pattern.typeName())
  return pattern
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
    pattern.module.temporaryEdit(),
    placeholder,
  )
  if (replacements.length !== 0) {
    // throw new Error(`${replacements.length} unused replacements.`)
  }
  return replaced
}
