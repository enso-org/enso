import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { unsafeKeys } from '@/util/record'

type Matches<T> = Record<keyof T, Ast.Ast[] | undefined>

interface MatchResult<T> {
  innerExpr: Ast.Ast
  matches: Record<keyof T, Ast.Ast[] | undefined>
}

export class Prefixes<T extends Record<keyof T, Pattern>> {
  constructor(
    /** Note that these are checked in order of definition. */
    public prefixes: T,
  ) {}

  /** Note that these are checked in order of definition. */
  static FromLines<T>(lines: Record<keyof T, string>) {
    return new Prefixes(
      Object.fromEntries(
        Object.entries<string>(lines).map(([name, line]) => [name, Pattern.parse(line)]),
      ) as Record<keyof T, Pattern>,
    )
  }

  extractMatches(expression: Ast.Ast): MatchResult<T> {
    const matches = Object.fromEntries(
      Object.entries<Pattern>(this.prefixes).map(([name, pattern]) => {
        const matches = pattern.match(expression)
        const lastMatch = matches != null ? matches[matches.length - 1] : undefined
        if (lastMatch) expression = lastMatch
        return [name, matches]
      }),
    ) as Matches<T>
    return { matches, innerExpr: expression }
  }

  modify(
    expression: Ast.MutableAst,
    replacements: Partial<Record<keyof T, Ast.Owned[] | undefined>>,
  ) {
    const edit = expression.module
    const matches = this.extractMatches(expression)
    let result = edit.get(matches.innerExpr)!.take().node
    for (const key of unsafeKeys(this.prefixes).reverse()) {
      if (key in replacements && !replacements[key]) continue
      const replacement: Ast.Owned[] | undefined =
        replacements[key] ?? matches.matches[key]?.map((match) => edit.get(match)!.take().node)
      if (!replacement) continue
      const pattern = this.prefixes[key]
      const parts = [...replacement, result]
      result = pattern.instantiate(edit, parts)
    }
    return result
  }
}
