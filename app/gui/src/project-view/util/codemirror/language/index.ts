import { useLanguageSupportExtensions } from '@/providers/languageSupportExtensions'
import type { ToValue } from '@/util/reactivity'
import { acceptCompletion, autocompletion, startCompletion } from '@codemirror/autocomplete'
import { Prec, type Extension } from '@codemirror/state'
import { keymap, ViewPlugin, type PluginValue, type ViewUpdate } from '@codemirror/view'
import { mapOr, type Opt } from 'enso-common/src/utilities/data/opt'
import { computed, effectScope, shallowRef, toValue, type Ref } from 'vue'

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
export function useLanguageSupport(syntax: ToValue<Opt<string>>): Readonly<Ref<Extension>> {
  const initLanguageExtension = useLanguageSupportExtensions(true)
  if (!initLanguageExtension) return shallowRef(NULL_EXTENSION)
  const scope = effectScope()
  /** Language support for a known syntax. */
  const languageExt = computed((): Extension | undefined =>
    mapOr(toValue(syntax), undefined, (syntax) => scope.run(() => initLanguageExtension(syntax))),
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
