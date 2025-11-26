import { proxyRefs, type MaybeRefOrGetterArray } from '@/util/reactivity'
import { createGlobalState } from '@vueuse/core'
import * as text from 'enso-common/src/text'
import { computed, ref, toValue } from 'vue'

export type TextStore = ReturnType<typeof createTextStore>

/**
 * A composable for getting localized text and setting the language.
 *
 * The composable is used in tests only; the application should use
 * `injectText` instead.
 */
function createTextStore() {
  const language = ref(text.resolveUserLanguage())
  const locale = computed(() => text.LANGUAGE_TO_LOCALE[language.value])
  const localizedText = computed(() => text.getDictionary(language.value))

  const getText: GetText = (key, ...replacements) =>
    text.getText(localizedText.value, key, ...replacements)

  function textRef<K extends text.TextId>(
    key: K,
    ...replacements: MaybeRefOrGetterArray<text.Replacements[K]>
  ) {
    return computed(() =>
      getText(toValue(key), ...(replacements.map((x) => toValue(x)) as text.Replacements[K])),
    )
  }

  function setLanguage(lang: text.Language) {
    language.value = lang
  }

  return proxyRefs({ language, locale, getText, textRef, setLanguage })
}

/**
 * A function that gets localized text for a given key, with optional replacements.
 * @param key - The key of the text to get.
 * @param replacements - The replacements to insert into the text.
 * If the text contains placeholders like `$0`, `$1`, etc.,
 * they will be replaced with the corresponding replacement.
 */
export type GetText = <K extends text.TextId>(
  key: K,
  ...replacements: text.Replacements[K]
) => string

export const useText = createGlobalState(createTextStore)
