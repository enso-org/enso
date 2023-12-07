import { Ast } from '@/util/ast'
import { extractMatches, replaceMatches } from '@/util/ast/match'
import { unsafeKeys } from '@/util/record'

type Matches<T> = Record<keyof T, Ast.Ast[] | undefined>

interface MatchResult<T> {
  innerExpr: Ast.Ast
  matches: Record<keyof T, Ast.Ast[] | undefined>
}

export class Prefixes<T extends Record<keyof T, Ast.Ast>> {
  constructor(
    /** Note that these are checked in order of definition. */
    public prefixes: T,
  ) {}

  /** Note that these are checked in order of definition. */
  static FromLines<T>(lines: Record<keyof T, string>) {
    return new Prefixes(
      Object.fromEntries(
        Object.entries<string>(lines).map(([name, line]) => [name, Ast.parseLine(line)]),
      ) as Record<keyof T, Ast.Ast>,
    )
  }

  extractMatches(expression: Ast.Ast): MatchResult<T> {
    const matches = Object.fromEntries(
      Object.entries<Ast.Ast>(this.prefixes).map(([name, pattern]) => {
        const matches = extractMatches(expression, pattern)
        const lastMatch = matches != null ? matches[matches.length - 1] : undefined
        if (lastMatch) expression = lastMatch
        return [name, matches]
      }),
    ) as Matches<T>
    return { matches, innerExpr: expression }
  }

  modify(
    expression: Ast.Ast,
    replacements: Partial<Record<keyof T, (Ast.Ast | string)[] | undefined>>,
  ) {
    const matches = this.extractMatches(expression)
    let result = matches.innerExpr
    for (const key of unsafeKeys(this.prefixes).reverse()) {
      if (key in replacements && !replacements[key]) continue
      const replacement: (Ast.Ast | string)[] | undefined =
        replacements[key] ?? matches.matches[key]
      if (!replacement) continue
      result = replaceMatches(this.prefixes[key], [...replacement, result])
    }
    return result
  }
}
