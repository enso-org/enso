import { Ast, astContainingChar, parseEnso, readAstSpan, readTokenSpan } from '@/util/ast'
import { GeneralOprApp } from '@/util/ast/opr'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { computed, ref, type ComputedRef, type Ref } from 'vue'
import type { Filter } from './filtering'

/** Input's editing context.
 *
 * It suggests what part of the input should be altered when accepting suggestion.
 *
 * Some variants has `accessOpr` field which contains a `.` application where the
 * right operand is edited. It may be a qualified name or self argument for
 * a suggestion.
 */
export type EditingContext =
  // Suggestion should be inserted at given position.
  | {
      type: 'insert'
      position: number
      accessOpr?: GeneralOprApp
    }
  // Suggestion should replace given identifier.
  | {
      type: 'changeIdentifier'
      identifier: Ast.Tree.Ident
      accessOpr?: GeneralOprApp
    }
  // Suggestion should replace given literal.
  | { type: 'changeLiteral'; literal: Ast.Tree.TextLiteral | Ast.Tree.Number }

/** Component Browser Input Data */
export class Input {
  /** The current input's text (code). */
  readonly code: Ref<string>
  /** The current selection (or cursor position if start is equal to end). */
  readonly selection: Ref<{ start: number; end: number }>
  /** The editing context deduced from code and selection */
  readonly context: ComputedRef<EditingContext>
  /** The filter deduced from code and selection. */
  readonly filter: ComputedRef<Filter>

  constructor() {
    this.code = ref('')
    this.selection = ref({ start: 0, end: 0 })

    this.context = computed(() => {
      const input = this.code.value
      const cursorPosition = this.selection.value.start
      if (cursorPosition === 0) return { type: 'insert', position: 0 }
      const editedPart = cursorPosition - 1
      const inputAst = parseEnso(input)
      const editedAst = astContainingChar(editedPart, inputAst)
      const leaf = editedAst.next()
      if (leaf.done) return { type: 'insert', position: cursorPosition }
      switch (leaf.value.type) {
        case Ast.Tree.Type.Ident:
          return {
            type: 'changeIdentifier',
            identifier: leaf.value,
            ...Input.readAccessOpr(editedAst.next(), input, leaf.value),
          }
        case Ast.Tree.Type.TextLiteral:
        case Ast.Tree.Type.Number:
          return { type: 'changeLiteral', literal: leaf.value }
        default:
          return {
            type: 'insert',
            position: cursorPosition,
            ...Input.readAccessOpr(leaf, input),
          }
      }
    })

    const qualifiedNameFilter: ComputedRef<Filter> = computed(() => {
      const code = this.code.value
      const ctx = this.context.value
      if (
        (ctx.type == 'changeIdentifier' || ctx.type == 'insert') &&
        ctx.accessOpr != null &&
        ctx.accessOpr.lhs != null
      ) {
        const qn = Input.asQualifiedName(ctx.accessOpr, code)
        if (qn != null) return { qualifiedNamePattern: qn }
      }
      return {}
    })

    this.filter = computed(() => {
      const code = this.code.value
      const ctx = this.context.value
      const filter = { ...qualifiedNameFilter.value }
      if (ctx.type === 'changeIdentifier') {
        const start =
          ctx.identifier.whitespaceStartInCodeParsed + ctx.identifier.whitespaceLengthInCodeParsed
        const end = this.selection.value.end
        filter.pattern = code.substring(start, end)
      } else if (ctx.type === 'changeLiteral') {
        filter.pattern = readAstSpan(ctx.literal, code)
      }
      return filter
    })
  }

  private static readAccessOpr(
    leafParent: IteratorResult<Ast.Tree>,
    code: string,
    editedAst?: Ast.Tree,
  ): {
    accessOpr?: GeneralOprApp
  } {
    if (leafParent.done) return {}
    switch (leafParent.value.type) {
      case Ast.Tree.Type.OprApp:
      case Ast.Tree.Type.OperatorBlockApplication: {
        const generalized = new GeneralOprApp(leafParent.value)
        const opr = generalized.lastOpr()
        if (opr == null || !opr.ok || readTokenSpan(opr.value, code) !== '.') return {}
        // The filtering should be affected only when we edit right part of '.' application.
        else if (
          editedAst != null &&
          opr.value.startInCodeBuffer > editedAst.whitespaceStartInCodeParsed
        )
          return {}
        else return { accessOpr: generalized }
      }
      default:
        return {}
    }
  }

  /**
   * Try to get a Qualified Name part from given accessor chain.
   * @param accessorChain The accessorChain. It's not validated, i.e. the user must ensure
   *   it's GeneralOprApp with `.` as leading operator.
   * @param code The code from which `accessorChain` was generated.
   * @returns If all segments except the last one are identifiers, returns QualifiedName with
   *   those. Otherwise returns null.
   */
  private static asQualifiedName(accessorChain: GeneralOprApp, code: string): QualifiedName | null {
    const operandsAsIdents = Array.from(
      accessorChain.operandsOfLeftAssocOprChain(code, '.'),
      (operand) =>
        operand?.type === 'ast' && operand.ast.type === Ast.Tree.Type.Ident ? operand.ast : null,
    ).slice(0, -1)
    if (operandsAsIdents.some((optIdent) => optIdent == null)) return null
    const segments = operandsAsIdents.map((ident) => readAstSpan(ident!, code))
    const rawQn = segments.join('.')
    const qn = tryQualifiedName(rawQn)
    return qn.ok ? qn.value : null
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test.each([
    ['', 0, { type: 'insert', position: 0 }, {}],
    [
      'Data.',
      5,
      { type: 'insert', position: 5, accessorChain: ['Data', '.', null] },
      { qualifiedNamePattern: 'Data' },
    ],
    ['Data.', 4, { type: 'changeIdentifier', identifier: 'Data' }, { pattern: 'Data' }],
    [
      'Data.read',
      5,
      { type: 'insert', position: 5, accessorChain: ['Data', '.', 'read'] },
      { qualifiedNamePattern: 'Data' },
    ],
    [
      'Data.read',
      7,
      { type: 'changeIdentifier', identifier: 'read', accessorChain: ['Data', '.', 'read'] },
      { pattern: 're', qualifiedNamePattern: 'Data' },
    ],
  ])(
    "Input context and filtering, when content is '%s' and cursor at %i",
    (
      code,
      cursorPosition,
      expContext: {
        type: string
        position?: number
        accessorChain?: (string | null)[]
        identifier?: string
        literal?: string
      },
      expFiltering: { pattern?: string; qualifiedNamePattern?: string },
    ) => {
      const input = new Input()
      input.code.value = code
      input.selection.value = { start: cursorPosition, end: cursorPosition }
      const context = input.context.value
      const filter = input.filter.value
      expect(context.type).toStrictEqual(expContext.type)
      switch (context.type) {
        case 'insert':
          expect(context.position).toStrictEqual(expContext.position)
          expect(
            context.accessOpr != null
              ? Array.from(context.accessOpr.componentsReprs(code))
              : undefined,
          ).toStrictEqual(expContext.accessorChain)
          break
        case 'changeIdentifier':
          expect(readAstSpan(context.identifier, code)).toStrictEqual(expContext.identifier)
          expect(
            context.accessOpr != null
              ? Array.from(context.accessOpr.componentsReprs(code))
              : undefined,
          ).toStrictEqual(expContext.accessorChain)
          break
        case 'changeLiteral':
          expect(readAstSpan(context.literal, code)).toStrictEqual(expContext.literal)
      }
      expect(filter.pattern).toStrictEqual(expFiltering.pattern)
      expect(filter.qualifiedNamePattern).toStrictEqual(expFiltering.qualifiedNamePattern)
    },
  )
}
