import * as Ast from '@/generated/ast'
import { astContainingChar, readAstSpan, readTokenSpan } from '@/util/ast'
import { GeneralOprApp, operandsOfLeftAssocOprChain } from '@/util/ast/opr'
import { parseEnso2 } from '@/util/ffi'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { computed, ref, type ComputedRef, type Ref } from 'vue'
import type { Filter } from './filtering'

function asQualifiedName(accessorChain: GeneralOprApp, code: string): QualifiedName | null {
  const operandsAsIdents = Array.from(operandsOfLeftAssocOprChain(astNode, code, '.'), (operand) =>
    operand?.type === Ast.Tree.Type.Ident ? operand : null,
  )
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
  readonly cursorPosition: Ref<number>
  readonly context: ComputedRef<EditingContext>
  readonly filter: ComputedRef<Filter>

  constructor() {
    this.code = ref('')
    this.cursorPosition = ref(0)

    this.context = computed(() => {
      const input = this.code.value
      const cursorPosition = this.cursorPosition.value
      if (cursorPosition === 0) return { type: 'insert', position: 0 }
      const editedPart = cursorPosition - 1
      const inputAst = parseEnso2(input)
      const editedAst = astContainingChar(editedPart, inputAst)
      const leaf = editedAst.next()
      if (leaf.done) return { type: 'insert', position: cursorPosition }
      switch (leaf.value.type) {
        case Ast.Tree.Type.Ident:
          return {
            type: 'changeIdentifier',
            identifier: leaf.value,
            ...Input.readAccessorChain(editedAst.next(), input),
          }
        case Ast.Tree.Type.TextLiteral:
        case Ast.Tree.Type.Number:
          return { type: 'changeLiteral', literal: leaf.value }
        default:
          return {
            type: 'insert',
            position: cursorPosition,
            ...Input.readAccessorChain(editedAst.next(), input),
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
        const qn = asQualifiedName(ctx.accessorChain.lhs, code)
        if (qn != null) return { qualifiedNamePattern: qn }
      }
      return {}
    })

    this.filter = computed(() => {
      const code = this.code.value
      const ctx = this.context.value
      const filter = { ...qualifiedNameFilter.value }
      if (ctx.type === 'changeIdentifier') {
        filter.pattern = readAstSpan(ctx.identifier, code)
      } else if (ctx.type === 'changeLiteral') {
        filter.pattern = readAstSpan(ctx.literal, code)
      }
      return filter
    })
  }

  private static readAccessorChain(
    leafParent: IteratorResult<Ast.Tree>,
    code: string,
  ): {
    accessorChain?: GeneralOprApp
  } {
    if (leafParent.done) return {}
    switch (leafParent.value.type) {
      case Ast.Tree.Type.OprApp:
      case Ast.Tree.Type.OperatorBlockApplication: {
        const generalized = new GeneralOprApp(leafParent.value)
        const opr = generalized.lastOpr()
        if (opr && opr.ok && readTokenSpan(opr.value, code) === '.')
          return { accessorChain: generalized }
        else return {}
      }
      default:
        return {}
    }
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test('childrenAstNodes', () => {
    // console.log(Array.from)
  })
}
