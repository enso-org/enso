import type { Filter } from '@/components/ComponentBrowser/filtering'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import {
  SuggestionKind,
  type SuggestionEntry,
  type Typename,
} from '@/stores/suggestionDatabase/entry'
import {
  Ast,
  astContainingChar,
  parseEnso,
  parsedTreeRange,
  readAstSpan,
  readTokenSpan,
} from '@/util/ast'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { GeneralOprApp } from '@/util/ast/opr'
import type { ExpressionInfo } from '@/util/computedValueRegistry'
import { MappedSet } from '@/util/containers'
import {
  qnLastSegment,
  qnParent,
  qnSplit,
  tryQualifiedName,
  type QualifiedName,
} from '@/util/qualifiedName'
import { IdMap, type ExprId } from 'shared/yjsModel'
import { computed, ref, type ComputedRef } from 'vue'

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
      oprApp?: GeneralOprApp
    }
  // Suggestion should replace given identifier.
  | {
      type: 'changeIdentifier'
      identifier: Ast.Tree.Ident
      oprApp?: GeneralOprApp
    }
  // Suggestion should replace given literal.
  | { type: 'changeLiteral'; literal: Ast.Tree.TextLiteral | Ast.Tree.Number }

/** Component Browser Input Data */
export function useComponentBrowserInput(
  graphStore: { identDefinitions: Map<string, ExprId> } = useGraphStore(),
  computedValueRegistry: {
    getExpressionInfo(id: ExprId): ExpressionInfo | undefined
  } = useProjectStore().computedValueRegistry,
) {
  const code = ref('')
  const selection = ref({ start: 0, end: 0 })
  const ast = computed(() => parseEnso(code.value))

  const context: ComputedRef<EditingContext> = computed(() => {
    const cursorPosition = selection.value.start
    if (cursorPosition === 0) return { type: 'insert', position: 0 }
    const editedPart = cursorPosition - 1
    const inputAst = ast.value
    const editedAst = astContainingChar(editedPart, inputAst).values()
    const leaf = editedAst.next()
    if (leaf.done) return { type: 'insert', position: cursorPosition }
    switch (leaf.value.type) {
      case Ast.Tree.Type.Ident:
        return {
          type: 'changeIdentifier',
          identifier: leaf.value,
          ...readOprApp(editedAst.next(), leaf.value),
        }
      case Ast.Tree.Type.TextLiteral:
      case Ast.Tree.Type.Number:
        return { type: 'changeLiteral', literal: leaf.value }
      default:
        return {
          type: 'insert',
          position: cursorPosition,
          ...readOprApp(leaf),
        }
    }
  })

  const internalUsages = computed(() => {
    const analyzer = new AliasAnalyzer(code.value, ast.value)
    analyzer.process()
    function* internalUsages() {
      for (const [_definition, usages] of analyzer.aliases) {
        yield* usages
      }
    }
    return new MappedSet(IdMap.keyForRange, internalUsages())
  })

  // Filter deduced from the access (`.` operator) chain written by user.
  const accessChainFilter: ComputedRef<Filter> = computed(() => {
    const ctx = context.value
    if (ctx.type === 'changeLiteral') return {}
    if (ctx.oprApp == null || ctx.oprApp.lhs == null) return {}
    const opr = ctx.oprApp.lastOpr()
    const input = code.value
    if (opr == null || !opr.ok || readTokenSpan(opr.value, input) !== '.') return {}
    const selfArg = pathAsSelfArgument(ctx.oprApp)
    if (selfArg != null) return { selfArg: selfArg }
    const qn = pathAsQualifiedName(ctx.oprApp)
    if (qn != null) return { qualifiedNamePattern: qn }
    return {}
  })

  const filter = computed(() => {
    const input = code.value
    const ctx = context.value
    const filter = { ...accessChainFilter.value }
    if (ctx.type === 'changeIdentifier') {
      const start =
        ctx.identifier.whitespaceStartInCodeParsed + ctx.identifier.whitespaceLengthInCodeParsed
      const end = selection.value.end
      filter.pattern = input.substring(start, end)
    } else if (ctx.type === 'changeLiteral') {
      filter.pattern = readAstSpan(ctx.literal, input)
    }
    return filter
  })

  function readOprApp(
    leafParent: IteratorResult<Ast.Tree>,
    editedAst?: Ast.Tree,
  ): {
    oprApp?: GeneralOprApp
  } {
    if (leafParent.done) return {}
    switch (leafParent.value.type) {
      case Ast.Tree.Type.OprApp:
      case Ast.Tree.Type.OperatorBlockApplication: {
        const generalized = new GeneralOprApp(leafParent.value)
        const opr = generalized.lastOpr()
        if (opr == null || !opr.ok) return {}
        // Opr application affects context only when we edit right part of operator.
        else if (
          editedAst != null &&
          opr.value.startInCodeBuffer > editedAst.whitespaceStartInCodeParsed
        )
          return {}
        else return { oprApp: generalized }
      }
      default:
        return {}
    }
  }

  function pathAsSelfArgument(
    accessOpr: GeneralOprApp,
  ): { type: 'known'; typename: Typename } | { type: 'unknown' } | null {
    if (accessOpr.lhs == null) return null
    if (accessOpr.lhs.type !== Ast.Tree.Type.Ident) return null
    if (accessOpr.apps.length > 1) return null
    if (internalUsages.value.has(parsedTreeRange(accessOpr.lhs))) return { type: 'unknown' }
    const ident = readAstSpan(accessOpr.lhs, code.value)
    const definition = graphStore.identDefinitions.get(ident)
    if (definition == null) return null
    const typename = computedValueRegistry.getExpressionInfo(definition)?.typename
    return typename != null ? { type: 'known', typename } : { type: 'unknown' }
  }

  function pathAsQualifiedName(accessOpr: GeneralOprApp): QualifiedName | null {
    const operandsAsIdents = qnIdentifiers(accessOpr)
    const segments = operandsAsIdents.map((ident) => readAstSpan(ident, code.value))
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
  function qnIdentifiers(opr: GeneralOprApp): Ast.Tree.Ident[] {
    const operandsAsIdents = Array.from(
      opr.operandsOfLeftAssocOprChain(code.value, '.'),
      (operand) =>
        operand?.type === 'ast' && operand.ast.type === Ast.Tree.Type.Ident ? operand.ast : null,
    ).slice(0, -1)
    if (operandsAsIdents.some((optIdent) => optIdent == null)) return []
    else return operandsAsIdents as Ast.Tree.Ident[]
  }

  /** Apply given suggested entry to the input. */
  function applySuggestion(entry: SuggestionEntry) {
    const oldCode = code.value
    const changes = Array.from(inputChangesAfterApplying(entry)).reverse()
    const newCodeUpToLastChange = changes.reduce(
      (builder, change) => {
        const oldCodeFragment = oldCode.substring(builder.oldCodeIndex, change.range[0])
        return {
          code: builder.code + oldCodeFragment + change.str,
          oldCodeIndex: change.range[1],
        }
      },
      { code: '', oldCodeIndex: 0 },
    )
    const isModule = entry.kind === SuggestionKind.Module
    const firstCharAfter = oldCode[newCodeUpToLastChange.oldCodeIndex]
    const shouldInsertSpace =
      !isModule && (firstCharAfter == null || /^[a-zA-Z0-9_]$/.test(firstCharAfter))
    const shouldMoveCursor = !isModule
    const newCursorPos = newCodeUpToLastChange.code.length + (shouldMoveCursor ? 1 : 0)
    code.value =
      newCodeUpToLastChange.code +
      (shouldInsertSpace ? ' ' : '') +
      oldCode.substring(newCodeUpToLastChange.oldCodeIndex)
    selection.value = { start: newCursorPos, end: newCursorPos }
  }

  /** Return all input changes resulting from applying given suggestion.
   *
   * @returns The changes, starting from the rightmost. The `start` and `end` parameters refer
   * to indices of "old" input content.
   */
  function* inputChangesAfterApplying(
    entry: SuggestionEntry,
  ): Generator<{ range: [number, number]; str: string }> {
    const ctx = context.value
    const str = codeToBeInserted(entry)
    switch (ctx.type) {
      case 'insert': {
        yield { range: [ctx.position, ctx.position], str }
        break
      }
      case 'changeIdentifier': {
        yield { range: parsedTreeRange(ctx.identifier), str }
        break
      }
      case 'changeLiteral': {
        yield { range: parsedTreeRange(ctx.literal), str }
        break
      }
    }
    yield* qnChangesAfterApplying(entry)
  }

  function codeToBeInserted(entry: SuggestionEntry): string {
    const ctx = context.value
    const opr = ctx.type !== 'changeLiteral' && ctx.oprApp != null ? ctx.oprApp.lastOpr() : null
    const oprAppSpacing =
      ctx.type === 'insert' && opr != null && opr.ok && opr.value.whitespaceLengthInCodeBuffer > 0
        ? ' '.repeat(opr.value.whitespaceLengthInCodeBuffer)
        : ''
    const extendingAccessOprChain =
      opr != null && opr.ok && readTokenSpan(opr.value, code.value) === '.'
    // Modules are special case, as we want to encourage user to continue writing path.
    if (entry.kind === SuggestionKind.Module) {
      if (extendingAccessOprChain) return `${oprAppSpacing}${entry.name}${oprAppSpacing}.`
      else return `${entry.definedIn}.`
    }
    // Always return single name if we're extending access opr chain.
    if (extendingAccessOprChain) return oprAppSpacing + entry.name
    // Otherwise they may be cases we want to add type/module name, or self argument placeholder.
    if (entry.selfType != null) return `${oprAppSpacing}_.${entry.name}`
    if (entry.memberOf != null) {
      const parentName = qnLastSegment(entry.memberOf)
      return `${oprAppSpacing}${parentName}.${entry.name}`
    }
    return oprAppSpacing + entry.name
  }

  /** All changes to the qualified name already written by the user.
   *
   * See `inputChangesAfterApplying`. */
  function* qnChangesAfterApplying(
    entry: SuggestionEntry,
  ): Generator<{ range: [number, number]; str: string }> {
    if (entry.selfType != null) return []
    if (entry.kind === SuggestionKind.Local || entry.kind === SuggestionKind.Function) return []
    if (context.value.type === 'changeLiteral') return []
    if (context.value.oprApp == null) return []
    const writtenQn = qnIdentifiers(context.value.oprApp).reverse()

    let containingQn =
      entry.kind === SuggestionKind.Module
        ? qnParent(entry.definedIn)
        : entry.memberOf ?? entry.definedIn
    for (const ident of writtenQn) {
      if (containingQn == null) break
      const [parent, segment] = qnSplit(containingQn)
      yield { range: parsedTreeRange(ident), str: segment }
      containingQn = parent
    }
  }

  return {
    /** The current input's text (code). */
    code,
    /** The current selection (or cursor position if start is equal to end). */
    selection,
    /** The editing context deduced from code and selection */
    context,
    /** The filter deduced from code and selection. */
    filter,
    /** Apply given suggested entry to the input. */
    applySuggestion,
  }
}
