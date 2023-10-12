import {
  SuggestionKind,
  makeCon,
  makeLocal,
  makeMethod,
  makeModuleMethod,
  makeStaticMethod,
  type SuggestionEntry,
} from '@/stores/suggestionDatabase/entry'
import { Ast, astContainingChar, astSpan, parseEnso, readAstSpan, readTokenSpan } from '@/util/ast'
import { GeneralOprApp } from '@/util/ast/opr'
import {
  qnLastSegment,
  qnParent,
  qnSplit,
  tryQualifiedName,
  type QualifiedName,
} from '@/util/qualifiedName'
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
        const qn = Input.pathAsQualifiedName(ctx.accessOpr, code)
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

  private static pathAsQualifiedName(accessOpr: GeneralOprApp, code: string): QualifiedName | null {
    const operandsAsIdents = Input.qnIdentifiers(accessOpr, code)
    const segments = operandsAsIdents.map((ident) => readAstSpan(ident!, code))
    const rawQn = segments.join('.')
    const qn = tryQualifiedName(rawQn)
    return qn.ok ? qn.value : null
  }

  /**
   * Read path segments as idents. The 'path' means all access chain operands except the last.
   * If some of such operands is not and identifier, this returns `null`.
   * @param opr
   * @param code The code from which `opr` was generated.
   * @returns If all path segments are identifiers, return them
   */
  private static qnIdentifiers(opr: GeneralOprApp, code: string): Ast.Tree.Ident[] {
    const operandsAsIdents = Array.from(opr.operandsOfLeftAssocOprChain(code, '.'), (operand) =>
      operand?.type === 'ast' && operand.ast.type === Ast.Tree.Type.Ident ? operand.ast : null,
    ).slice(0, -1)
    if (operandsAsIdents.some((optIdent) => optIdent == null)) return []
    else return operandsAsIdents as Ast.Tree.Ident[]
  }

  /** Apply given suggested entry to the input. */
  applySuggestion(entry: SuggestionEntry) {
    const oldCode = this.code.value
    const changes = Array.from(this.inputChangesAfterApplying(entry)).reverse()
    const newCodeUpToLastChange = changes.reduce(
      (builder, change) => {
        const oldCodeFragment = oldCode.substring(builder.oldCodeIndex, change.start)
        return {
          code: builder.code + oldCodeFragment + change.str,
          oldCodeIndex: change.end,
        }
      },
      { code: '', oldCodeIndex: 0 },
    )
    const newCursorPos = newCodeUpToLastChange.code.length
    this.code.value =
      newCodeUpToLastChange.code + oldCode.substring(newCodeUpToLastChange.oldCodeIndex)
    this.selection.value = { start: newCursorPos, end: newCursorPos }
  }

  /** Return all input changes resulting from applying given suggestion.
   *
   * @returns The changes, starting from the rightmost. The `start` and `end` parameters refer
   * to indices of "old" input content.
   */
  private *inputChangesAfterApplying(
    entry: SuggestionEntry,
  ): Generator<{ start: number; end: number; str: string }> {
    const ctx = this.context.value
    const str = this.codeToBeInserted(entry)
    switch (ctx.type) {
      case 'insert': {
        yield { start: ctx.position, end: ctx.position, str }
        break
      }
      case 'changeIdentifier': {
        yield { ...astSpan(ctx.identifier), str }
        break
      }
      case 'changeLiteral': {
        yield { ...astSpan(ctx.literal), str }
        break
      }
    }
    yield* this.qnChangesAfterApplying(entry)
  }

  private codeToBeInserted(entry: SuggestionEntry): string {
    // Always return single name if accessOpr is already present.
    if (this.context.value.type !== 'changeLiteral' && this.context.value.accessOpr != null)
      return entry.name
    // Otherwise they may be cases we want to add type/module name, or self argument placeholder.
    if (entry.selfType != null) return `_.${entry.name}`
    if (entry.memberOf != null) {
      const parentName = qnLastSegment(entry.memberOf)
      return `${parentName}.${entry.name}`
    }
    return entry.name
  }

  /** All changes to the qualified name already written by the user.
   *
   * See `inputChangesAfterApplying`. */
  private *qnChangesAfterApplying(
    entry: SuggestionEntry,
  ): Generator<{ start: number; end: number; str: string }> {
    if (entry.selfType != null) return []
    if (entry.kind === SuggestionKind.Local || entry.kind === SuggestionKind.Function) return []
    if (this.context.value.type === 'changeLiteral') return []
    if (this.context.value.accessOpr == null) return []
    const writtenQn = Input.qnIdentifiers(this.context.value.accessOpr, this.code.value).reverse()

    let containingQn =
      entry.kind === SuggestionKind.Module
        ? qnParent(entry.definedIn)
        : entry.memberOf ?? entry.definedIn
    for (const ident of writtenQn) {
      if (containingQn == null) break
      const [parent, segment] = qnSplit(containingQn)
      yield { ...astSpan(ident), str: segment }
      containingQn = parent
    }
  }
}
