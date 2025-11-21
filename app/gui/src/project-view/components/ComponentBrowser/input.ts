import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { type NodeId } from '$/providers/openedProjects/graph'
import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import {
  requiredImportEquals,
  requiredImports,
  type RequiredImport,
} from '$/providers/openedProjects/module/imports'
import { type SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import {
  entryDisplayOwner,
  entryDisplayPath,
  entryHasOwner,
  entryIsStatic,
  type SuggestionEntry,
  type SuggestionId,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import { useAI } from '@/components/ComponentBrowser/ai'
import type { Filter, SelfArg } from '@/components/ComponentBrowser/filtering'
import { Ast } from '@/util/ast'
import { selfArgSeparator } from '@/util/ast/abstract'
import { ANY_TYPE } from '@/util/ensoTypes'
import type { ProjectPath } from '@/util/projectPath'
import { qnLastSegment } from '@/util/qualifiedName'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { useToast } from '@/util/toast'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { computed, readonly, ref, shallowRef, toRef, toValue, type ComputedRef } from 'vue'
import { Range } from 'ydoc-shared/util/data/range'

/** Information how the component browser is used, needed for proper input initializing. */
export type Usage =
  | { type: 'newNode'; sourcePort?: Ast.AstId | undefined }
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
      literal?: Ast.TextLiteral | Ast.NumericLiteral | Ast.NegationApp | undefined
    }
  | {
      mode: 'codeEditing'
      code: string
      appliedSuggestion?: SuggestionEntry
    }
  | {
      mode: 'aiPrompt'
      prompt: string
    }

/** Component Browser Input Data */
export function useComponentBrowserInput(
  graphDb: ToValue<GraphDb> = toRef(useCurrentProject().graph.value, 'db'),
  suggestionDb: ToValue<SuggestionDb> = toRef(useCurrentProject().suggestionDb.value, 'entries'),
  ai: { query(query: string, sourcePort: string): Promise<Result<string>> } = useAI(),
) {
  const text = ref('')
  const cbUsage = ref<Usage>()
  const selection = ref(Range.empty)
  const imports = shallowRef<RequiredImport[]>([])
  const processingAIPrompt = ref(false)
  const toastError = useToast.error()
  const sourceNodeIdentifier = ref<Ast.Identifier>()
  const switchedToCodeMode = ref<{ appliedSuggestion?: SuggestionEntry }>()

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
    selection.value = selection.value
      .shift(prefixLengthChange)
      .clip(Range.fromStartAndLength(0, newText.length))
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
      let literal: Ast.MutableTextLiteral | Ast.NumericLiteral | Ast.NegationApp | undefined =
        Ast.TextLiteral.tryParse(text.value)
      if (literal == null) {
        literal = Ast.NumericLiteral.tryParseWithSign(text.value)
      } else {
        literal.fixBoundaries()
      }
      return {
        mode: 'componentBrowsing',
        filter: {
          pattern: text.value,
          ...(sourceNodeType.value != null ? { selfArg: sourceNodeType.value } : {}),
        },
        literal,
      }
    }
  })

  const sourceNodeType = computed<SelfArg | null>(() => {
    if (!sourceNodeIdentifier.value) return null
    const graphDbValue = toValue(graphDb)
    const definition = graphDbValue.getIdentDefiningNode(sourceNodeIdentifier.value)
    if (definition == null) return null
    const info = graphDbValue.getExpressionInfo(definition)
    if (info == null || info.typeInfo == null) return { type: 'unknown' }
    const ancestors = [...info.typeInfo.ancestors(toValue(suggestionDb))]
    return { type: 'known', typeInfo: info.typeInfo, ancestors }
  })

  /** Apply given suggested entry to the input. */
  function applySuggestion(id: SuggestionId, suffix: string | undefined): Result {
    const suggestionDbValue = toValue(suggestionDb)
    const entry = suggestionDbValue.get(id)
    if (!entry) return Err(`No entry with id ${id}`)
    switchedToCodeMode.value = { appliedSuggestion: entry }
    const { newText, requiredImport } = inputAfterApplyingSuggestion(entry)
    const newTextWithSuffix = suffix ? `${newText}${suffix}` : newText
    text.value = newTextWithSuffix
    selection.value = Range.emptyAt(newTextWithSuffix.length)
    if (requiredImport) {
      const importId = suggestionDbValue.findByProjectPath(requiredImport)
      if (importId) {
        const requiredEntry = suggestionDbValue.get(importId)
        if (requiredEntry) {
          imports.value = imports.value.concat(requiredImports(suggestionDbValue, requiredEntry))
        }
      }
    } else {
      imports.value = imports.value.concat(requiredImports(suggestionDbValue, entry))
    }
    return Ok()
  }

  function switchToCodeEditMode() {
    switchedToCodeMode.value = {}
  }

  function inputAfterApplyingSuggestion(entry: SuggestionEntry): {
    newText: string
    requiredImport: ProjectPath | undefined
  } {
    if (sourceNodeIdentifier.value && sourceNodeType.value?.type === 'known') {
      const sourceTypes = sourceNodeType.value.typeInfo.visibleTypes
      if (
        entryHasOwner(entry) &&
        !sourceTypes.find((type) => type.equals(entry.memberOf)) &&
        !sourceNodeType.value.ancestors.find((ancestor) => ancestor.equals(entry.memberOf)) &&
        !entry.memberOf.equals(ANY_TYPE)
      ) {
        return {
          newText: ':' + entryDisplayOwner(entry) + ' . ' + entry.name + ' ',
          requiredImport: entry.memberOf,
        }
      }
      return {
        newText: entry.name + ' ',
        requiredImport: undefined,
      }
    } else if (entryIsStatic(entry)) {
      return {
        newText: entryDisplayPath(entry) + ' ',
        requiredImport: entry.memberOf.normalized(),
      }
    } else {
      // Perhaps we will add cases for Type/Con imports, but they are not displayed as suggestion ATM.
      return {
        newText: entry.name + ' ',
        requiredImport: undefined,
      }
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
        anImport.kind == 'Qualified' ?
          qnLastSegment(
            anImport.module.path ?? anImport.module.project ?? ('Main' as Ast.Identifier),
          )
        : anImport.import
      const noLongerNeeded = !text.value.includes(importedIdent)
      if (!noLongerNeeded && !alreadyAdded) {
        finalImports.push(anImport)
      }
    }
    return finalImports
  }

  function reset(usage: Usage) {
    const graphDbValue = toValue(graphDb)
    switch (usage.type) {
      case 'newNode':
        if (usage.sourcePort) {
          const ident = graphDbValue.getOutputPortIdentifier(usage.sourcePort)
          sourceNodeIdentifier.value = ident != null && Ast.isIdentifier(ident) ? ident : undefined
        } else {
          sourceNodeIdentifier.value = undefined
        }
        text.value = ''
        selection.value = Range.empty
        break
      case 'editNode': {
        const parsed = extractSourceNode(
          graphDbValue.nodeIdToNode.get(usage.node)?.innerExpr.code() ?? '',
        )
        text.value = parsed.text
        sourceNodeIdentifier.value = parsed.sourceNodeIdentifier
        selection.value = Range.emptyAt(usage.cursorPos - parsed.textOffset)
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
      Ast.isIdentifier(matchedSource) &&
      matchedCode != null &&
      toValue(graphDb).getIdentDefiningNode(matchedSource)
    )
      return {
        text: matchedCode,
        textOffset: matchedSource.length + 1,
        sourceNodeIdentifier: matchedSource,
      }
    return { text: expression, textOffset: 0, sourceNodeIdentifier: undefined }
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
          selection.value = Range.emptyAt(result.value.length)
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
    return sourceNodeIdentifier.value ?
        `${sourceNodeIdentifier.value}${selfArgSeparator(text)}${text}`
      : text
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
    /** Flag indicating that we're waiting for AI's answer for user's prompt. */
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
