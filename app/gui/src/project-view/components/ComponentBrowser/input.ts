import { useAI } from '@/components/ComponentBrowser/ai'
import type { Filter, SelfArg } from '@/components/ComponentBrowser/filtering'
import { useGraphStore, type NodeId } from '@/stores/graph'
import type { GraphDb } from '@/stores/graph/graphDatabase'
import { requiredImportEquals, requiredImports, type RequiredImport } from '@/stores/graph/imports'
import { useSuggestionDbStore, type SuggestionDb } from '@/stores/suggestionDatabase'
import { type SuggestionEntry, type SuggestionId } from '@/stores/suggestionDatabase/entry'
import { isIdentifier, type AstId, type Identifier } from '@/util/ast/abstract'
import { Err, Ok, type Result } from '@/util/data/result'
import { qnLastSegment, type QualifiedName } from '@/util/qualifiedName'
import { useToast } from '@/util/toast'
import { computed, proxyRefs, readonly, ref, type ComputedRef } from 'vue'

/** Information how the component browser is used, needed for proper input initializing. */
export type Usage =
  | { type: 'newNode'; sourcePort?: AstId | undefined }
  | { type: 'editNode'; node: NodeId; cursorPos: number }

/**
 * One of the modes of the component browser:
 * "component browsing" when user wants to add new component
 * "code editing" for editing existing, or just added nodes
 * See https://github.com/enso-org/enso/issues/10598 for design details.
 */
export type ComponentBrowserMode =
  | {
      mode: 'componentBrowsing'
      filter: Filter
    }
  | {
      mode: 'codeEditing'
      code: string
      appliedSuggestion?: SuggestionId
    }
  | {
      mode: 'aiPrompt'
      prompt: string
    }

/** Component Browser Input Data */
export function useComponentBrowserInput(
  graphDb: GraphDb = useGraphStore().db,
  suggestionDb: SuggestionDb = useSuggestionDbStore().entries,
  ai: { query(query: string, sourcePort: string): Promise<Result<string>> } = useAI(),
) {
  const text = ref('')
  const cbUsage = ref<Usage>()
  const selection = ref({ start: 0, end: 0 })
  const imports = ref<RequiredImport[]>([])
  const processingAIPrompt = ref(false)
  const toastError = useToast.error()
  const sourceNodeIdentifier = ref<Identifier>()
  const switchedToCodeMode = ref<{ appliedSuggestion?: SuggestionId }>()

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
        const parsed = extractSourceNode(newText)
        if (
          switchedToCodeMode.value &&
          !sourceNodeIdentifier.value &&
          parsed.sourceNodeIdentifier
        ) {
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

  const mode: ComputedRef<ComponentBrowserMode> = computed(() => {
    if (sourceNodeIdentifier.value) {
      const aiPromptMatch = /^AI:(.*)$/.exec(text.value)
      if (aiPromptMatch) {
        return { mode: 'aiPrompt', prompt: aiPromptMatch[1] ?? ' ' }
      }
    }
    if (switchedToCodeMode.value || cbUsage.value?.type === 'editNode') {
      return {
        mode: 'codeEditing',
        code: applySourceNode(text.value),
        ...(switchedToCodeMode.value?.appliedSuggestion ?
          { appliedSuggestion: switchedToCodeMode.value.appliedSuggestion }
        : {}),
      }
    } else {
      return {
        mode: 'componentBrowsing',
        filter: {
          pattern: text.value,
          ...(sourceNodeType.value != null ? { selfArg: sourceNodeType.value } : {}),
        },
      }
    }
  })

  const sourceNodeType = computed<SelfArg | null>(() => {
    if (!sourceNodeIdentifier.value) return null
    const definition = graphDb.getIdentDefiningNode(sourceNodeIdentifier.value)
    if (definition == null) return null
    const typename = graphDb.getExpressionInfo(definition)?.typename
    return typename != null ? { type: 'known', typename } : { type: 'unknown' }
  })

  /** Apply given suggested entry to the input. */
  function applySuggestion(id: SuggestionId): Result {
    const entry = suggestionDb.get(id)
    if (!entry) return Err(`No entry with id ${id}`)
    switchedToCodeMode.value = { appliedSuggestion: id }
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
    return Ok()
  }

  function switchToCodeEditMode() {
    switchedToCodeMode.value = {}
  }

  function inputAfterApplyingSuggestion(entry: SuggestionEntry): {
    newText: string
    newCode: string
    newCursorPos: number
    requiredImport: QualifiedName | null
  } {
    const newText =
      !sourceNodeIdentifier.value && entry.memberOf ?
        `${qnLastSegment(entry.memberOf)}.${entry.name} `
      : `${entry.name} `
    const newCode =
      sourceNodeIdentifier.value ? `${sourceNodeIdentifier.value}.${entry.name} ` : `${newText} `
    const newCursorPos = newText.length

    return {
      newText,
      newCode,
      newCursorPos,
      requiredImport:
        sourceNodeIdentifier.value ? null
        : entry.memberOf ? entry.memberOf
          // Perhaps we will add cases for Type/Con imports, but they are not displayed as
          // suggestion ATM.
        : null,
    }
  }

  /**
   * List of imports required for applied suggestions.
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
    if (mode.value.mode !== 'aiPrompt') {
      console.error('Cannot apply AI prompt in non-AI context')
      return
    }
    if (sourceNodeIdentifier.value == null) {
      console.error('Cannot apply AI prompt without a source node.')
      return
    }
    processingAIPrompt.value = true
    ai.query(mode.value.prompt, sourceNodeIdentifier.value).then(
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

  return proxyRefs({
    /** The current input's displayed text (code, excluding any initial self argument). */
    text: readonly(text),
    /** The current input's displayed text and selection. */
    content: contentModel,
    /** The current input's full code. */
    code: computed(() => applySourceNode(text.value)),
    /** The component browser mode. See {@link ComponentBrowserMode} */
    mode,
    /** Initial self argument to place before the displayed text in the inserted code. */
    selfArgument: sourceNodeIdentifier,
    /** The current selection (or cursor position if start is equal to end). */
    selection,
    /** Flag indincating that we're waiting for AI's answer for user's prompt. */
    processingAIPrompt,
    /** Re-initializes the input for given usage. */
    reset,
    /** Apply given suggested entry to the input. It will switch mode to code editing. */
    applySuggestion,
    /** Switch to code edit mode with input as-is */
    switchToCodeEditMode,
    /** Apply the currently written AI prompt. */
    applyAIPrompt,
    /** A list of imports to add when the suggestion is accepted. */
    importsToAdd,
  })
}
