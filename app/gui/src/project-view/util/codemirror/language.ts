import type { SuggestionDb } from '@/stores/suggestionDatabase'
import type { MethodSuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { ProjectPath } from '@/util/projectPath'
import type { QualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import { acceptCompletion, autocompletion, startCompletion } from '@codemirror/autocomplete'
import { Prec, type Extension } from '@codemirror/state'
import { keymap, ViewPlugin, type PluginValue, type ViewUpdate } from '@codemirror/view'
import { tableExpression, type MethodCompletionInfo } from 'lezer-enso-table-expr'
import { computed, toValue, type Ref } from 'vue'
import { mapOr, type Opt } from 'ydoc-shared/util/data/opt'

export interface LanguageSupportOptions {
  suggestionDb: ToValue<Opt<SuggestionDb>>
}

const NULL_EXTENSION: Extension = []

/**
 * An extension that starts autocomplete (if the extension is installed) when the editor is focused
 * and its contents are empty.
 */
export function startCompletionWhenEmptyDocumentFocused(): Extension {
  return startOnFocus
}
const startOnFocus = ViewPlugin.fromClass(
  class implements PluginValue {
    update(update: ViewUpdate) {
      if (
        update.state.doc.length === 0 &&
        update.view.hasFocus &&
        (update.focusChanged || update.docChanged)
      ) {
        // Execute the command asynchronously because a transaction may not be dispatched directly
        // from an update handler.
        setTimeout(() => {
          if (update.view.hasFocus) startCompletion(update.view)
        })
      }
    }
  },
)

const completionBindings = keymap.of([
  {
    // Currently, various CM usages bind the `Enter` event on an element wrapping the editor. For
    // compatibility with this behaviour, the autocomplete binding for `Enter` must stop the event
    // if and only if handled; CM doesn't support this except through the `any` interface.
    any: (view, event) => {
      if (event.key !== 'Enter') return false
      const handled = acceptCompletion(view)
      if (handled) {
        event.stopPropagation()
        event.preventDefault()
      }
      return handled
    },
  },
])

/** @returns a reactive syntax support extension for the specified language. */
export function useLanguageSupport(
  syntax: ToValue<Opt<string>>,
  { suggestionDb }: LanguageSupportOptions,
) {
  const extensions: Record<string, Readonly<Ref<Extension>>> = Object.assign(Object.create(null), {
    'enso-table-expression': useTableExpressionExtension(suggestionDb),
  })
  function languageExtension(languageName: string): Extension | undefined {
    const extension = extensions[languageName]
    DEV: if (!extension) console.warn(`Unknown WidgetText syntax: ${languageName}`)
    return extension?.value
  }

  /** Language support for a known syntax. */
  const languageExt = computed((): Extension | undefined =>
    mapOr(toValue(syntax), undefined, languageExtension),
  )
  /** Extensions added when any language support is available. */
  const anyLanguageExt = computed((): Extension[] => [
    Prec.highest(completionBindings),
    autocompletion({ filterStrict: true }),
    startCompletionWhenEmptyDocumentFocused(),
  ])
  return computed(
    (): Extension =>
      languageExt.value ? [languageExt.value, ...anyLanguageExt.value] : NULL_EXTENSION,
  )
}

const COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Column.Column' as QualifiedName,
)
/** @returns a lazily initialized extension for the table expression language. */
function useTableExpressionExtension(
  suggestionDb: ToValue<Opt<SuggestionDb>>,
): Readonly<Ref<Extension>> {
  const methodInfos = computed(() =>
    [...(toValue(suggestionDb)?.selectableMethods(COLUMN_TYPE) ?? [])]
      .filter((method) => !EXCLUDED_METHODS.has(method.name))
      .map(methodInfoFromEntry),
  )
  return computed(() => tableExpression({ methods: () => methodInfos.value }))
}
function methodInfoFromEntry(entry: MethodSuggestionEntry): MethodCompletionInfo {
  return {
    name: entry.name,
    description: entry.documentationSummary,
  }
}
const EXCLUDED_METHODS = new Set([
  ///// Syntactic methods /////
  // These methods are used to implement special syntaxes in the expression language. Some cannot
  // syntactically be used as methods; others could legally be used, but the dedicated syntax is
  // preferred.
  'between',
  'iif',
  'is_in',
  'is_nothing',
  'like',
  'not',
  ///// Semantically excluded methods /////
  // These methods are available for use on Column values but may not make sense in an Expression.
  'info', // Technically works, probably not useful.
  'rename', // When used with `Column.set`, this is redundant and doesn't work.
  'to_table', // Does nothing, successfully but inefficiently.
  'to_vector', // Error
])
