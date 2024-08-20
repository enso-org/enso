import type { Filter } from '@/components/ComponentBrowser/filtering'
import { useGraphStore, type NodeId } from '@/stores/graph'
import type { GraphDb } from '@/stores/graph/graphDatabase'
import { requiredImportEquals, requiredImports, type RequiredImport } from '@/stores/graph/imports'
import { useSuggestionDbStore, type SuggestionDb } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  entryQn,
  type SuggestionEntry,
  type SuggestionId,
  type Typename,
} from '@/stores/suggestionDatabase/entry'
import { isIdentifier, isOperator, type AstId, type Identifier } from '@/util/ast/abstract'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { RawAstExtended } from '@/util/ast/extended'
import { GeneralOprApp, type OperatorChain } from '@/util/ast/opr'
import { RawAst, astContainingChar } from '@/util/ast/raw'
import { MappedSet } from '@/util/containers'
import type { Result } from '@/util/data/result'
import {
  qnFromSegments,
  qnLastSegment,
  qnSegments,
  tryQualifiedName,
  type QualifiedName,
} from '@/util/qualifiedName'
import { useToast } from '@/util/toast'
import { equalFlat } from 'lib0/array'
import { computed, readonly, ref, type ComputedRef } from 'vue'
import { sourceRangeKey, type SourceRange } from 'ydoc-shared/yjsModel'
import { useAI } from './ai'

/** Information how the component browser is used, needed for proper input initializing. */
export type Usage =
  | { type: 'newNode'; sourcePort?: AstId | undefined }
  | { type: 'editNode'; node: NodeId; cursorPos: number }

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
      identifier: RawAstExtended<RawAst.Tree.Ident, false>
      oprApp?: GeneralOprApp<false>
    }
  // Suggestion should replace given literal.
  | {
      type: 'changeLiteral'
      literal: RawAstExtended<RawAst.Tree.TextLiteral | RawAst.Tree.Number, false>
    }
  // In process of writing AI prompt: no suggestion should be displayed.
  | {
      type: 'aiPrompt'
      prompt: string
    }

/** An atomic change to the user input. */
interface Change {
  str: string
  /** Range in the original code to be replaced with `str`. */
  range: SourceRange
}

/** Component Browser Input Data */
export function useComponentBrowserInput(
  graphDb: GraphDb = useGraphStore().db,
  suggestionDb: SuggestionDb = useSuggestionDbStore().entries,
  ai: { query(query: string, sourcePort: string): Promise<Result<string>> } = useAI(),
) {
  const text = ref('')
  const cbUsage = ref<Usage>()
  const anyChange = ref(false)
  const selection = ref({ start: 0, end: 0 })
  const ast = computed(() => RawAstExtended.parse(text.value))
  const imports = ref<RequiredImport[]>([])
  const processingAIPrompt = ref(false)
  const toastError = useToast.error()
  const sourceNodeIdentifier = ref<Identifier>()
  const firstAppliedReturnType = ref<Typename>()

  // Text Model to being edited externally (by user).
  //
  // Some user actions (like typing operator right after input) may handled differently than
  // internal changes (like applying suggestion).
  const contentModel = computed({
    get: () => ({
      text: text.value,
      selection: selection.value,
    }),
    set: ({ text: newText, selection: newSelection }) => {
      if (newSelection) {
        selection.value = newSelection
      }
      if (newText !== text.value) {
        anyChange.value = true
        const parsed = extractSourceNode(newText)
        if (
          sourceNodeIdentifier.value &&
          cbUsage.value?.type !== 'editNode' &&
          isOperator(newText.trimStart())
        ) {
          // When user, right after opening CB with source node types operator, we should
          // re-initialize input with it instead of dot at the end.
          alterInput(
            `${sourceNodeIdentifier.value} ${newText}`,
            sourceNodeIdentifier.value.length + 1,
          )
          sourceNodeIdentifier.value = undefined
        } else if (!sourceNodeIdentifier.value && parsed.sourceNodeIdentifier) {
          alterInput(parsed.text, -(parsed.sourceNodeIdentifier.length + 1))
          sourceNodeIdentifier.value = parsed.sourceNodeIdentifier
        } else {
          text.value = newText
        }
      }
    },
  })

  function alterInput(newText: string, prefixLengthChange: number) {
    text.value = newText
    const adjustPoint = (point: number) =>
      Math.min(newText.length, Math.max(0, point + prefixLengthChange))
    selection.value = {
      start: adjustPoint(selection.value.start),
      end: adjustPoint(selection.value.end),
    }
  }

  const context: ComputedRef<EditingContext> = computed(() => {
    const cursorPosition = selection.value.start
    if (sourceNodeIdentifier.value) {
      const aiPromptMatch = /^AI:(.*)$/.exec(text.value)
      if (aiPromptMatch) {
        return {
          type: 'aiPrompt',
          prompt: aiPromptMatch[1] ?? '',
        }
      }
    }
    if (cursorPosition === 0) return { type: 'insert', position: 0 }
    const editedPart = cursorPosition - 1
    const inputAst = ast.value
    const editedAst = inputAst
      .mapIter((ast) => astContainingChar(editedPart, ast))
      [Symbol.iterator]()
    const leaf = editedAst.next()
    if (leaf.done) return { type: 'insert', position: cursorPosition }
    switch (leaf.value.inner.type) {
      case RawAst.Tree.Type.Ident:
        return {
          type: 'changeIdentifier',
          identifier: leaf.value as RawAstExtended<RawAst.Tree.Ident, false>,
          ...readOprApp(editedAst.next(), leaf.value),
        }
      case RawAst.Tree.Type.TextLiteral:
      case RawAst.Tree.Type.Number:
        return {
          type: 'changeLiteral',
          literal: leaf.value as RawAstExtended<
            RawAst.Tree.TextLiteral | RawAst.Tree.Number,
            false
          >,
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
    const analyzer = new AliasAnalyzer(text.value, ast.value.inner)
    analyzer.process()
    function* internalUsages() {
      for (const [_definition, usages] of analyzer.aliases) {
        yield* usages
      }
    }
    return new MappedSet(sourceRangeKey, internalUsages())
  })

  // Filter deduced from the access (`.` operator) chain written by user.
  const accessChainFilter: ComputedRef<Filter> = computed(() => {
    const ctx = context.value
    if (ctx.type === 'changeLiteral' || ctx.type === 'aiPrompt') return {}
    const insertingAtBeginning = ctx.type === 'insert' && ctx.position === 0
    const editingAtBeginning =
      ctx.type === 'changeIdentifier' && ctx.identifier.inner.whitespaceStartInCodeParsed === 0
    if (sourceNodeIdentifier.value && (insertingAtBeginning || editingAtBeginning)) {
      const selfArg = nodeType(sourceNodeIdentifier.value)
      return selfArg ? { selfArg } : {}
    }
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
    const input = text.value
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

  const autoSelectFirstComponent = computed(() => {
    // We want to autoselect first component only when we may safely assume user want's to continue
    // editing - they want to immediately see preview of best component and rather won't press
    // enter (and if press, they won't be surprised by the results).
    const ctx = context.value
    // If no input, we're sure user want's to add something.
    if (!text.value) return true
    // When changing identifier, it is unfinished. Or, the best match should be exactly what
    // the user wants
    if (ctx.type === 'changeIdentifier') return true
    // With partially written `.` chain we ssume user want's to add something.
    if (ctx.type === 'insert' && ctx.oprApp?.lastOpr()?.repr() === '.') return true
    return false
  })

  function readOprApp(
    leafParent: IteratorResult<RawAstExtended<RawAst.Tree, false>>,
    editedAst?: RawAstExtended<RawAst.Tree, false>,
  ): {
    oprApp?: GeneralOprApp<false>
  } {
    if (leafParent.done) return {}
    switch (leafParent.value.inner.type) {
      case RawAst.Tree.Type.OprApp:
      case RawAst.Tree.Type.OperatorBlockApplication: {
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
    if (!accessOpr.lhs.isTree(RawAst.Tree.Type.Ident)) return null
    if (accessOpr.apps.length > 1) return null
    if (internalUsages.value.has(accessOpr.lhs.span())) return { type: 'unknown' }
    return nodeType(accessOpr.lhs.repr())
  }

  function nodeType(
    ident: string,
  ): { type: 'known'; typename: Typename } | { type: 'unknown' } | null {
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
   * @returns If all path segments are identifiers, return them
   */
  function qnIdentifiers(opr: GeneralOprApp<false>): RawAstExtended<RawAst.Tree.Ident, false>[] {
    const operandsAsIdents = Array.from(opr.operandsOfLeftAssocOprChain('.'), (operand) =>
      operand?.type === 'ast' && operand.ast.isTree(RawAst.Tree.Type.Ident) ? operand.ast : null,
    ).slice(0, -1)
    if (operandsAsIdents.some((optIdent) => optIdent == null)) return []
    else return operandsAsIdents as RawAstExtended<RawAst.Tree.Ident, false>[]
  }

  /** Apply given suggested entry to the input. */
  function applySuggestion(id: SuggestionId) {
    const entry = suggestionDb.get(id)
    if (!entry) return
    if (firstAppliedReturnType.value == null) firstAppliedReturnType.value = entry.returnType
    const { newText, newCursorPos, requiredImport } = inputAfterApplyingSuggestion(entry)
    text.value = newText
    selection.value = { start: newCursorPos, end: newCursorPos }
    if (requiredImport) {
      const [importId] = suggestionDb.nameToId.lookup(requiredImport)
      if (importId) {
        const requiredEntry = suggestionDb.get(importId)
        if (requiredEntry) {
          imports.value = imports.value.concat(requiredImports(suggestionDb, requiredEntry))
        }
      }
    } else {
      imports.value = imports.value.concat(requiredImports(suggestionDb, entry))
    }
  }

  function inputAfterApplyingSuggestion(entry: SuggestionEntry): {
    newText: string
    newCode: string
    newCursorPos: number
    requiredImport: QualifiedName | null
  } {
    const { changes, requiredImport } = inputChangesAfterApplying(entry)
    changes.reverse()
    const newCodeUpToLastChange = changes.reduce(
      (builder, change) => {
        const oldCodeFragment = text.value.substring(builder.oldCodeIndex, change.range[0])
        return {
          text: builder.text + oldCodeFragment + change.str,
          oldCodeIndex: change.range[1],
        }
      },
      { text: '', oldCodeIndex: 0 },
    )
    const isModule = entry.kind === SuggestionKind.Module
    const firstCharAfter = text.value[newCodeUpToLastChange.oldCodeIndex]
    const shouldInsertSpace =
      !isModule && (firstCharAfter == null || /^[a-zA-Z0-9_]$/.test(firstCharAfter))
    const shouldMoveCursor = !isModule
    const newCursorPos = newCodeUpToLastChange.text.length + (shouldMoveCursor ? 1 : 0)
    const newText =
      newCodeUpToLastChange.text +
      (shouldInsertSpace ? ' ' : '') +
      text.value.substring(newCodeUpToLastChange.oldCodeIndex)
    return {
      newText,
      newCode: applySourceNode(newText),
      newCursorPos,
      requiredImport,
    }
  }

  /** List of imports required for applied suggestions.
   *
   * If suggestion was manually edited by the user after accepting, it is not included.
   */
  function importsToAdd(): RequiredImport[] {
    const finalImports: RequiredImport[] = []
    for (const anImport of imports.value) {
      const alreadyAdded = finalImports.some((existing) => requiredImportEquals(existing, anImport))
      const importedIdent =
        anImport.kind == 'Qualified' ? qnLastSegment(anImport.module) : anImport.import
      const noLongerNeeded = !text.value.includes(importedIdent)
      if (!noLongerNeeded && !alreadyAdded) {
        finalImports.push(anImport)
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
    changes: Change[]
    requiredImport: QualifiedName | null
  } {
    const ctx = context.value
    const str = codeToBeInserted(entry)
    let mainChange: Change | undefined = undefined
    switch (ctx.type) {
      case 'insert': {
        mainChange = { range: [ctx.position, ctx.position], str }
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
      case 'aiPrompt': {
        mainChange = { range: [0, text.value.length], str }
        break
      }
    }
    const qnChange = qnChanges(entry)
    return { changes: [mainChange!, ...qnChange.changes], requiredImport: qnChange.requiredImport }
  }

  function codeToBeInserted(entry: SuggestionEntry): string {
    const ctx = context.value
    if (ctx.type === 'insert' && ctx.position === 0 && sourceNodeIdentifier.value) return entry.name
    if (
      ctx.type === 'changeIdentifier' &&
      ctx.identifier.inner.whitespaceStartInCodeParsed === 0 &&
      sourceNodeIdentifier.value
    )
      return entry.name
    const opr = 'oprApp' in ctx && ctx.oprApp != null ? ctx.oprApp.lastOpr() : null
    const oprAppSpacing =
      ctx.type === 'insert' && opr != null && opr.inner.whitespaceLengthInCodeBuffer > 0 ?
        ' '.repeat(opr.inner.whitespaceLengthInCodeBuffer)
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
    changes: Change[]
    requiredImport: QualifiedName | null
  } {
    if (entry.selfType != null) return { changes: [], requiredImport: null }
    if (entry.kind === SuggestionKind.Local || entry.kind === SuggestionKind.Function)
      return { changes: [], requiredImport: null }
    if ('oprApp' in context.value && context.value.oprApp != null) {
      // 1. Find the index of last identifier from the original qualified name that is already written by the user.
      const qn = entryQn(entry)
      const identifiers = qnIdentifiers(context.value.oprApp)
      const writtenSegments = identifiers.map((ident) => ident.repr())
      const allSegments = qnSegments(qn)
      const windowSize = writtenSegments.length
      let indexOfAlreadyWrittenSegment = undefined
      for (let right = allSegments.length; right >= windowSize; right--) {
        const left = right - windowSize
        if (equalFlat(allSegments.slice(left, right), writtenSegments)) {
          indexOfAlreadyWrittenSegment = left
          break
        }
      }
      if (indexOfAlreadyWrittenSegment == null) {
        // We didn’t find the exact match, which probably means the user has written
        // partial names, like `Dat.V.` instead of `Data.Vector`.
        // In this case we will replace each written segment with the most recent
        // segments of qualified name, and import what’s left.
        indexOfAlreadyWrittenSegment = allSegments.length - 1 - writtenSegments.length
      }
      // `nameSegments` and `importSegments` overlap by one, because we need to import the first written identifier.
      const nameSegments = allSegments.slice(indexOfAlreadyWrittenSegment, -1)
      const importSegments = allSegments.slice(0, indexOfAlreadyWrittenSegment + 1)
      // A correct qualified name contains more than 2 segments (namespace and project name).
      const minimalNumberOfSegments = 2
      const requiredImport =
        importSegments.length < minimalNumberOfSegments ? null : qnFromSegments(importSegments)
      // 2. We replace each written identifier with a correct one, and then append the rest of needed qualified name.
      let lastEditedCharIndex = 0
      const result = []
      for (let i = 0; i < nameSegments.length; i++) {
        const segment = nameSegments[i]
        if (i < identifiers.length) {
          // Identifier was already written by the user, we replace it with the correct one.
          const span = identifiers[i]!.span()
          lastEditedCharIndex = span[1]
          result.push({ range: span, str: segment as string })
        } else {
          // The rest of qualified name needs to be added at the end.
          const range: SourceRange = [lastEditedCharIndex, lastEditedCharIndex]
          result.push({ range, str: ('.' + segment) as string })
        }
      }
      return { changes: result.reverse(), requiredImport }
    } else {
      return { changes: [], requiredImport: null }
    }
  }

  function reset(usage: Usage) {
    switch (usage.type) {
      case 'newNode':
        if (usage.sourcePort) {
          const ident = graphDb.getOutputPortIdentifier(usage.sourcePort)
          sourceNodeIdentifier.value = ident != null && isIdentifier(ident) ? ident : undefined
        } else {
          sourceNodeIdentifier.value = undefined
        }
        text.value = ''
        selection.value = { start: 0, end: 0 }
        break
      case 'editNode': {
        const parsed = extractSourceNode(
          graphDb.nodeIdToNode.get(usage.node)?.innerExpr.code() ?? '',
        )
        text.value = parsed.text
        sourceNodeIdentifier.value = parsed.sourceNodeIdentifier
        selection.value = { start: usage.cursorPos, end: usage.cursorPos }
        break
      }
    }
    imports.value = []
    cbUsage.value = usage
    anyChange.value = false
  }

  function extractSourceNode(expression: string) {
    const sourceNodeMatch = /^([^.]+)\.(.*)$/.exec(expression)
    const matchedSource = sourceNodeMatch?.[1]
    const matchedCode = sourceNodeMatch?.[2]
    if (
      matchedSource != null &&
      isIdentifier(matchedSource) &&
      matchedCode != null &&
      graphDb.getIdentDefiningNode(matchedSource)
    )
      return { text: matchedCode, sourceNodeIdentifier: matchedSource }
    return { text: expression, sourceNodeIdentifier: undefined }
  }

  function applyAIPrompt() {
    const ctx = context.value
    if (ctx.type !== 'aiPrompt') {
      console.error('Cannot apply AI prompt in non-AI context')
      return
    }
    if (sourceNodeIdentifier.value == null) {
      sourceNodeIdentifier.value
      console.error('Cannot apply AI prompt without a source node.')
      return
    }
    processingAIPrompt.value = true
    ai.query(ctx.prompt, sourceNodeIdentifier.value).then(
      (result) => {
        if (result.ok) {
          text.value = result.value
        } else {
          toastError.reportError(result.error, 'Applying AI prompt failed')
        }
        processingAIPrompt.value = false
      },
      (err) => {
        const msg = `Applying AI prompt failed: ${err}`
        console.error(msg)
        toastError.show(msg)
        processingAIPrompt.value = false
      },
    )
  }

  function applySourceNode(text: string) {
    return sourceNodeIdentifier.value ? `${sourceNodeIdentifier.value}.${text}` : text
  }

  return {
    /** The current input's displayed text (code, excluding any initial self argument). */
    text: readonly(text),
    /** The current input's displayed text and selection. */
    content: contentModel,
    /** The current input's full code. */
    code: computed(() => applySourceNode(text.value)),
    /** Initial self argument to place before the displayed text in the inserted code. */
    selfArgument: sourceNodeIdentifier,
    /** A flag indicating that input was changed after last reset. */
    anyChange,
    /** The current selection (or cursor position if start is equal to end). */
    selection,
    /** @internal The editing context deduced from code and selection. Exposed for testing. */
    context,
    /** Whether the input is an AI prompt. */
    isAiPrompt: computed(() => context.value.type === 'aiPrompt'),
    /** The filter deduced from code and selection. */
    filter,
    /** Flag indicating that we should autoselect first component after last update. */
    autoSelectFirstComponent,
    /** Flag indincating that we're waiting for AI's answer for user's prompt. */
    processingAIPrompt,
    /** Re-initializes the input for given usage. */
    reset,
    /** Apply given suggested entry to the input. */
    applySuggestion,
    /** Apply the currently written AI prompt. */
    applyAIPrompt,
    /** Return input after applying given suggestion, without changing state. */
    inputAfterApplyingSuggestion,
    /** A list of imports to add when the suggestion is accepted. */
    importsToAdd,
    /** The return type of the first applied suggestion. */
    firstAppliedReturnType,
  }
}
