import type { Filter } from '@/components/ComponentBrowser/filtering'
import { useGraphStore } from '@/stores/graph'
import type { GraphDb } from '@/stores/graph/graphDatabase'
import { covers, requiredImports, type RequiredImport } from '@/stores/imports'
import { useSuggestionDbStore, type SuggestionDb } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  entryQn,
  type SuggestionEntry,
  type Typename,
} from '@/stores/suggestionDatabase/entry'
import { Ast, AstExtended, astContainingChar } from '@/util/ast'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { GeneralOprApp, type OperatorChain } from '@/util/ast/opr'
import { MappedSet } from '@/util/containers'
import {
  qnFromSegments,
  qnLastSegment,
  qnSegments,
  tryQualifiedName,
  type QualifiedName,
} from '@/util/qualifiedName'
import { unwrap } from '@/util/result'
import { equalFlat } from 'lib0/array'
import { IdMap, type ContentRange } from 'shared/yjsModel'
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
      oprApp?: GeneralOprApp<false>
    }
  // Suggestion should replace given identifier.
  | {
      type: 'changeIdentifier'
      identifier: AstExtended<Ast.Tree.Ident, false>
      oprApp?: GeneralOprApp<false>
    }
  // Suggestion should replace given literal.
  | { type: 'changeLiteral'; literal: AstExtended<Ast.Tree.TextLiteral | Ast.Tree.Number, false> }

/** Component Browser Input Data */
export function useComponentBrowserInput(
  graphDb: GraphDb = useGraphStore().db,
  suggestionDb: SuggestionDb = useSuggestionDbStore().entries,
) {
  const code = ref('')
  const selection = ref({ start: 0, end: 0 })
  const ast = computed(() => AstExtended.parse(code.value))

  const context: ComputedRef<EditingContext> = computed(() => {
    const cursorPosition = selection.value.start
    if (cursorPosition === 0) return { type: 'insert', position: 0 }
    const editedPart = cursorPosition - 1
    const inputAst = ast.value
    const editedAst = inputAst
      .mapIter((ast) => astContainingChar(editedPart, ast))
      [Symbol.iterator]()
    const leaf = editedAst.next()
    if (leaf.done) return { type: 'insert', position: cursorPosition }
    switch (leaf.value.inner.type) {
      case Ast.Tree.Type.Ident:
        return {
          type: 'changeIdentifier',
          identifier: leaf.value as AstExtended<Ast.Tree.Ident, false>,
          ...readOprApp(editedAst.next(), leaf.value),
        }
      case Ast.Tree.Type.TextLiteral:
      case Ast.Tree.Type.Number:
        return {
          type: 'changeLiteral',
          literal: leaf.value as AstExtended<Ast.Tree.TextLiteral | Ast.Tree.Number, false>,
        }
      default:
        return {
          type: 'insert',
          position: cursorPosition,
          ...readOprApp(leaf),
        }
    }
  })

  const internalUsages = computed(() => {
    const analyzer = new AliasAnalyzer(code.value, ast.value.inner)
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
    if (opr == null || opr.repr() !== '.') return {}
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
        ctx.identifier.inner.whitespaceStartInCodeParsed +
        ctx.identifier.inner.whitespaceLengthInCodeParsed
      const end = selection.value.end
      filter.pattern = input.substring(start, end)
    } else if (ctx.type === 'changeLiteral') {
      filter.pattern = ctx.literal.repr()
    }
    return filter
  })

  const imports = ref<RequiredImport[]>([])

  function readOprApp(
    leafParent: IteratorResult<AstExtended<Ast.Tree, false>>,
    editedAst?: AstExtended<Ast.Tree, false>,
  ): {
    oprApp?: GeneralOprApp<false>
  } {
    if (leafParent.done) return {}
    switch (leafParent.value.inner.type) {
      case Ast.Tree.Type.OprApp:
      case Ast.Tree.Type.OperatorBlockApplication: {
        const generalized = new GeneralOprApp(leafParent.value as OperatorChain<false>)
        const opr = generalized.lastOpr()
        if (opr == null) return {}
        // Opr application affects context only when we edit right part of operator.
        else if (
          editedAst != null &&
          opr.inner.startInCodeBuffer > editedAst.inner.whitespaceStartInCodeParsed
        )
          return {}
        else return { oprApp: generalized }
      }
      default:
        return {}
    }
  }

  function pathAsSelfArgument(
    accessOpr: GeneralOprApp<false>,
  ): { type: 'known'; typename: Typename } | { type: 'unknown' } | null {
    if (accessOpr.lhs == null) return null
    if (!accessOpr.lhs.isTree(Ast.Tree.Type.Ident)) return null
    if (accessOpr.apps.length > 1) return null
    if (internalUsages.value.has(accessOpr.lhs.span())) return { type: 'unknown' }
    const ident = accessOpr.lhs.repr()
    const definition = graphDb.getIdentDefiningNode(ident)
    if (definition == null) return null
    const typename = graphDb.getExpressionInfo(definition)?.typename
    return typename != null ? { type: 'known', typename } : { type: 'unknown' }
  }

  function pathAsQualifiedName(accessOpr: GeneralOprApp<false>): QualifiedName | null {
    const operandsAsIdents = qnIdentifiers(accessOpr)
    const segments = operandsAsIdents.map((ident) => ident.repr())
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
  function qnIdentifiers(opr: GeneralOprApp<false>): AstExtended<Ast.Tree.Ident, false>[] {
    const operandsAsIdents = Array.from(opr.operandsOfLeftAssocOprChain('.'), (operand) =>
      operand?.type === 'ast' && operand.ast.isTree(Ast.Tree.Type.Ident) ? operand.ast : null,
    ).slice(0, -1)
    if (operandsAsIdents.some((optIdent) => optIdent == null)) return []
    else return operandsAsIdents as AstExtended<Ast.Tree.Ident, false>[]
  }

  /** Apply given suggested entry to the input. */
  function applySuggestion(entry: SuggestionEntry) {
    const oldCode = code.value
    const { changes, requiredImport } = inputChangesAfterApplying(entry)
    changes.reverse()
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
    if (requiredImport) {
      const [id] = suggestionDb.nameToId.lookup(requiredImport)
      if (id) {
        const requiredEntry = suggestionDb.get(id)
        if (requiredEntry) {
          imports.value = imports.value.concat(requiredImports(suggestionDb, requiredEntry))
        }
      }
    } else {
      imports.value = imports.value.concat(requiredImports(suggestionDb, entry))
    }
  }

  function importsToAdd(): Set<RequiredImport> {
    const existingImports = graphDb.imports.value
    const finalImports = new Set<RequiredImport>()
    for (const required of imports.value) {
      if (!existingImports.some((existing) => covers(existing.import, required))) {
        finalImports.add(required)
      }
    }
    return finalImports
  }

  /** Return all input changes resulting from applying given suggestion.
   *
   * @returns The changes, starting from the rightmost. The `start` and `end` parameters refer
   * to indices of "old" input content.
   */
  function inputChangesAfterApplying(entry: SuggestionEntry): {
    changes: { range: ContentRange; str: string }[]
    requiredImport: QualifiedName | null
  } {
    const ctx = context.value
    const str = codeToBeInserted(entry)
    let mainChange = undefined
    switch (ctx.type) {
      case 'insert': {
        mainChange = { range: [ctx.position, ctx.position] as ContentRange, str }
        break
      }
      case 'changeIdentifier': {
        mainChange = { range: ctx.identifier.span(), str }
        break
      }
      case 'changeLiteral': {
        mainChange = { range: ctx.literal.span(), str }
        break
      }
    }
    const qnChange = qnChanges(entry)
    return { changes: [mainChange, ...qnChange.changes], requiredImport: qnChange.requiredImport }
  }

  function codeToBeInserted(entry: SuggestionEntry): string {
    const ctx = context.value
    const opr = ctx.type !== 'changeLiteral' && ctx.oprApp != null ? ctx.oprApp.lastOpr() : null
    const oprAppSpacing =
      ctx.type === 'insert' && opr != null && opr.inner.whitespaceLengthInCodeBuffer > 0
        ? ' '.repeat(opr.inner.whitespaceLengthInCodeBuffer)
        : ''
    const extendingAccessOprChain = opr != null && opr.repr() === '.'
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

  /** All changes to the qualified name already written by the user. */
  function qnChanges(entry: SuggestionEntry): {
    changes: { range: ContentRange; str: string }[]
    requiredImport: QualifiedName | null
  } {
    if (entry.selfType != null) return { changes: [], requiredImport: null }
    if (entry.kind === SuggestionKind.Local || entry.kind === SuggestionKind.Function)
      return { changes: [], requiredImport: null }
    if (context.value.type !== 'changeLiteral' && context.value.oprApp != null) {
      const qn = entryQn(entry)
      const identifiers = qnIdentifiers(context.value.oprApp)
      const writtenSegments = identifiers.map((ident) => ident.repr())
      const allSegments = qnSegments(qn)
      const windowSize = writtenSegments.length
      let position = undefined
      for (let right = allSegments.length; right >= windowSize; right--) {
        const left = right - windowSize
        if (equalFlat(allSegments.slice(left, right), writtenSegments)) {
          position = left
          break
        }
      }
      if (position == null) position = allSegments.length - 1 - writtenSegments.length
      const nameSegments = allSegments.slice(position, -1)
      const importSegments = allSegments.slice(0, position + 1)
      const minimalNumberOfSegments = 2
      const requiredImport =
        importSegments.length < minimalNumberOfSegments
          ? null
          : unwrap(qnFromSegments(importSegments))
      let last = 0
      const result = []
      for (let i = 0; i < nameSegments.length; i++) {
        const segment = nameSegments[i]
        if (i < identifiers.length) {
          const span = identifiers[i]!.span()
          last = span[1]
          result.push({ range: span, str: segment as string })
        } else {
          result.push({ range: [last, last] as ContentRange, str: ('.' + segment) as string })
        }
      }
      return { changes: result.reverse(), requiredImport }
    } else {
      return { changes: [], requiredImport: null }
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
    /** A list of imports to add when the suggestion is accepted */
    importsToAdd,
  }
}
