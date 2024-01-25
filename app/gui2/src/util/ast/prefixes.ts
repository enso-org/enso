import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { unsafeKeys } from '@/util/record'

type Matches<T> = Record<keyof T, Ast.AstId[] | undefined>

interface MatchResult<T> {
  innerExpr: Ast.AstId
  matches: Record<keyof T, Ast.AstId[] | undefined>
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
        if (lastMatch) expression = expression.module.checkedGet(lastMatch)
        return [name, matches]
      }),
    ) as Matches<T>
    return { matches, innerExpr: expression.id }
  }

  modify(expression: Ast.Mutable, replacements: Partial<Record<keyof T, Ast.Owned[] | undefined>>) {
    expression.updateValue((expression) => {
      const matches = this.extractMatches(expression)
      const edit = expression.module
      let result = edit.take(matches.innerExpr)
      for (const key of unsafeKeys(this.prefixes).reverse()) {
        if (key in replacements && !replacements[key]) continue
        const replacement: Ast.Owned[] | undefined =
          replacements[key] ?? matches.matches[key]?.map((match) => edit.take(match)!)
        if (!replacement) continue
        const pattern = this.prefixes[key]
        const parts = [...replacement, result]
        result = pattern.instantiate(edit, parts)
      }
      return result
    })
  }
}
