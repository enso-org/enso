import { Ast, astContainingChar, parseEnso, readAstSpan, readTokenSpan } from '@/util/ast'
import { GeneralOprApp, operandsOfLeftAssocOprChain } from '@/util/ast/opr'
import { debug } from '@/util/parserSupport'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { computed, ref, type ComputedRef, type Ref } from 'vue'
import type { Filter } from './filtering'

function asQualifiedName(accessorChain: GeneralOprApp, code: string): QualifiedName | null {
  const operandsAsIdents = Array.from(
    accessorChain.operandsOfLeftAssocOprChain(code, '.'),
    (operand) =>
      operand?.type === 'ast' && operand.ast.type === Ast.Tree.Type.Ident ? operand.ast : null,
  ).slice(0, -1)
  if (operandsAsIdents.some((optIdent) => optIdent == null)) return null
  const segments = operandsAsIdents.map((ident) => readAstSpan(ident!, code))
  const rawQn = segments.join('.')
  const qn = tryQualifiedName(rawQn)
  if (qn.ok) return qn.value
  else {
    console.error(
      `AST identifiers in dot chain does not represent valid qualified name: '${rawQn}'`,
    )
    return null
  }
}

type EditingContext =
  | {
      type: 'insert'
      position: number
      accessorChain?: GeneralOprApp
    }
  | {
      type: 'changeIdentifier'
      identifier: Ast.Tree.Ident
      accessorChain?: GeneralOprApp
    }
  | { type: 'changeLiteral'; literal: Ast.Tree.TextLiteral | Ast.Tree.Number }

export class Input {
  readonly code: Ref<string>
  readonly selection: Ref<{ start: number; end: number }>
  readonly context: ComputedRef<EditingContext>
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
            ...Input.readAccessorChain(editedAst.next(), input, leaf.value),
          }
        case Ast.Tree.Type.TextLiteral:
        case Ast.Tree.Type.Number:
          return { type: 'changeLiteral', literal: leaf.value }
        default:
          return {
            type: 'insert',
            position: cursorPosition,
            ...Input.readAccessorChain(leaf, input),
          }
      }
    })

    const qualifiedNameFilter: ComputedRef<Filter> = computed(() => {
      const code = this.code.value
      const ctx = this.context.value
      if (
        (ctx.type == 'changeIdentifier' || ctx.type == 'insert') &&
        ctx.accessorChain != null &&
        ctx.accessorChain.lhs != null
      ) {
        const qn = asQualifiedName(ctx.accessorChain, code)
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

  private static readAccessorChain(
    leafParent: IteratorResult<Ast.Tree>,
    code: string,
    editedAst?: Ast.Tree,
  ): {
    accessorChain?: GeneralOprApp
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
        else return { accessorChain: generalized }
      }
      default:
        return {}
    }
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
            context.accessorChain != null
              ? Array.from(context.accessorChain.readSpansOfCompnents(code))
              : undefined,
          ).toStrictEqual(expContext.accessorChain)
          break
        case 'changeIdentifier':
          expect(readAstSpan(context.identifier, code)).toStrictEqual(expContext.identifier)
          expect(
            context.accessorChain != null
              ? Array.from(context.accessorChain.readSpansOfCompnents(code))
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
