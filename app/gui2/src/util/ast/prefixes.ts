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
        const matchIds = pattern.match(expression)
        const matches = matchIds
          ? Array.from(matchIds, (id) => expression.module.get(id)!)
          : undefined
        const lastMatch = matches != null ? matches[matches.length - 1] : undefined
        if (lastMatch) expression = lastMatch
        return [name, matches]
      }),
    ) as Matches<T>
    return { matches, innerExpr: expression }
  }

  modify(
    edit: Ast.MutableModule,
    expression: Ast.Ast,
    replacements: Partial<Record<keyof T, Ast.Ast[] | undefined>>,
  ) {
    const matches = this.extractMatches(expression)
    let result = matches.innerExpr
    for (const key of unsafeKeys(this.prefixes).reverse()) {
      if (key in replacements && !replacements[key]) continue
      const replacement: Ast.Ast[] | undefined = replacements[key] ?? matches.matches[key]
      if (!replacement) continue
      const pattern = this.prefixes[key]
      const parts = [...replacement, result]
      const partsIds = Array.from(parts, (ast) => ast.exprId)
      result = pattern.instantiate(edit, partsIds)
    }
    return result
  }
}
