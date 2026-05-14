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
import { proxyRefs, type ToValue } from '$/utils/reactivity'
import type { Filter, SelfArg } from '@/components/ComponentBrowser/filtering'
import { isAiAssignment, readAiPrompt } from '@/components/GraphEditor/aiNode'
import { Ast } from '@/util/ast'
import { selfArgSeparator } from '@/util/ast/abstract'
import { nodeDocumentationText } from '@/util/ast/node'
import { ANY_TYPE } from '@/util/ensoTypes'
import type { ProjectPath } from '@/util/projectPath'
import { qnLastSegment } from '@/util/qualifiedName'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { computed, readonly, ref, shallowRef, toRef, toValue, type ComputedRef } from 'vue'
import { Range } from 'ydoc-shared/util/data/range'

/** Information how the component browser is used, needed for proper input initializing. */
export type Usage =
  | { type: 'newNode'; sourcePort?: Ast.AstId | undefined }
  | { type: 'editNode'; node: NodeId; cursorPos: number }

/**
 * The current effective interpretation of the component browser's input, derived from the
 * selected mode and the user-typed text. The `mode` tag identifies which of the three
 * operating modes the CB is in; the remaining fields carry mode-specific data the
 * surrounding view needs to render the panel. Computed, not stored — the underlying state
 * is `selectedMode` plus the text/selection model.
 *
 * See https://github.com/enso-org/enso/issues/10598 for design details.
 */
export type ComponentBrowserInterpretation =
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

/**
 * The user-selectable mode tag — the `mode` discriminator of {@link ComponentBrowserInterpretation}:
 * - `componentBrowsing` when the user is searching the suggestion list to add a new component,
 * - `codeEditing` for free-form code editing on a new or existing node,
 * - `aiPrompt` for typing a natural-language prompt that the local Claude agent expands into a
 *   User Defined Component.
 */
export type ComponentBrowserMode = ComponentBrowserInterpretation['mode']

/** Component Browser Input Data */
export function useComponentBrowserInput(
  aiAvailable: ToValue<boolean> = () => false,
  graphDb: ToValue<GraphDb> = toRef(useCurrentProject().graph.value, 'db'),
  suggestionDb: ToValue<SuggestionDb> = toRef(useCurrentProject().suggestionDb.value, 'entries'),
) {
  const text = ref('')
  const cbUsage = ref<Usage>()
  const selection = ref(Range.empty)
  const imports = shallowRef<RequiredImport[]>([])
  const sourceNodeIdentifier = ref<Ast.Identifier>()
  /**
   * The suggestion the user just accepted, if any. Surfaced through {@link interpretation} so the
   * editor can show the suggestion's icon while the user is fine-tuning its arguments.
   * Cleared on `reset`, on `setSelectedMode` away from `codeEditing`, and on
   * `switchToCodeEditMode` (which only carries text, no suggestion).
   */
  const appliedSuggestion = ref<SuggestionEntry>()
  const selectedMode = ref<ComponentBrowserMode>('componentBrowsing')
  /**
   * `true` when the CB was opened on an existing node — the mode is determined by the node
   * type and cannot be changed by the user. Derived from `cbUsage`, not separately tracked.
   */
  const modeLocked = computed(() => cbUsage.value?.type === 'editNode')

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
        // Auto-extract `<source>.<rest>` into the source-port slot when the user is composing
        // a brand-new node in code-editing mode and hasn't anchored a source yet. Skipped when
        // editing an existing node — the user already settled on that node's code shape and
        // shouldn't have it silently chopped up under their cursor.
        if (
          cbUsage.value?.type === 'newNode' &&
          selectedMode.value === 'codeEditing' &&
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

  const interpretation: ComputedRef<ComponentBrowserInterpretation> = computed(() => {
    if (selectedMode.value === 'aiPrompt') {
      return { mode: 'aiPrompt', prompt: text.value }
    }
    if (selectedMode.value === 'codeEditing') {
      return {
        mode: 'codeEditing',
        code: applySourceNode(text.value),
        ...(appliedSuggestion.value ? { appliedSuggestion: appliedSuggestion.value } : {}),
      }
    }
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
  })

  const sourceNodeType = computed<SelfArg | null>(() => {
    if (!sourceNodeIdentifier.value) return null
    const graphDbValue = toValue(graphDb)
    if (graphDbValue.getIdentDefiningNode(sourceNodeIdentifier.value) == null) return null
    const typeInfo = graphDbValue.getTypeOfIdentifier(sourceNodeIdentifier.value)
    if (typeInfo == null) return { type: 'unknown' }
    const ancestors = [...typeInfo.ancestors(toValue(suggestionDb))]
    return { type: 'known', typeInfo, ancestors }
  })

  /** Apply given suggested entry to the input. */
  function applySuggestion(id: SuggestionId, suffix: string | undefined): Result {
    const suggestionDbValue = toValue(suggestionDb)
    const entry = suggestionDbValue.get(id)
    if (!entry) return Err(`No entry with id ${id}`)
    appliedSuggestion.value = entry
    selectedMode.value = 'codeEditing'
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
    if (modeLocked.value) return
    appliedSuggestion.value = undefined
    selectedMode.value = 'codeEditing'
  }

  /**
   * User-driven mode change (from the mode menu or the Shift+Enter shortcut). Refuses when the
   * input is mode-locked (i.e. we're editing an existing node and the mode is determined by
   * the node type). When leaving `codeEditing`, the `appliedSuggestion` tracker is cleared so
   * the next entry to `codeEditing` re-derives it from scratch.
   */
  function setSelectedMode(mode: ComponentBrowserMode): void {
    if (modeLocked.value) return
    if (selectedMode.value === mode) return
    if (selectedMode.value === 'codeEditing') {
      appliedSuggestion.value = undefined
    }
    selectedMode.value = mode
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
    appliedSuggestion.value = undefined
    cbUsage.value = usage
    switch (usage.type) {
      case 'newNode':
        selectedMode.value = toValue(aiAvailable) ? 'aiPrompt' : 'componentBrowsing'
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
        const editedNode = graphDbValue.nodeIdToNode.get(usage.node)
        // An AI assignment opens in AI-prompt mode only if AI is currently available; without
        // a live agent there is no way to submit a new prompt, so fall through to code
        // editing instead of leaving the user staring at a disabled AI mode.
        if (editedNode && isAiAssignment(editedNode.outerAst) && toValue(aiAvailable)) {
          const prompt = readAiPrompt(nodeDocumentationText(editedNode)) ?? ''
          selectedMode.value = 'aiPrompt'
          sourceNodeIdentifier.value = undefined
          text.value = prompt
          selection.value = Range.emptyAt(prompt.length)
        } else {
          selectedMode.value = 'codeEditing'
          const parsed = extractSourceNode(editedNode?.innerExpr.code() ?? '')
          text.value = parsed.text
          sourceNodeIdentifier.value = parsed.sourceNodeIdentifier
          selection.value = Range.emptyAt(usage.cursorPos - parsed.textOffset)
        }
        break
      }
    }
    imports.value = []
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
    /** The current interpretation of the input under the selected mode. See {@link ComponentBrowserInterpretation}. */
    interpretation,
    /** The user-selected mode tag (drives {@link interpretation}). */
    selectedMode: readonly(selectedMode),
    /** When `true`, the mode is determined by `usage` (an existing node's type) and cannot be changed. */
    modeLocked,
    /** Initial self argument to place before the displayed text in the inserted code. */
    selfArgument: sourceNodeIdentifier,
    /** The current selection (or cursor position if start is equal to end). */
    selection,
    /** Re-initializes the input for given usage. */
    reset,
    /** Apply given suggested entry to the input. It will switch mode to code editing. */
    applySuggestion,
    /** Switch to code edit mode with input as-is */
    switchToCodeEditMode,
    /** Change the selected mode; refuses when {@link modeLocked} is `true`. */
    setSelectedMode,
    /** A list of imports to add when the suggestion is accepted. */
    importsToAdd,
  })
}
