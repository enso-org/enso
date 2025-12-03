import { proxyRefs, type MaybeRefOrGetterArray } from '@/util/reactivity'
import { createGlobalState } from '@vueuse/core'
import {
  getDictionary,
  LANGUAGE_TO_LOCALE,
  getText as originalGetText,
  resolveUserLanguage,
  type DefaultGetText,
  type Language,
  type Replacements,
  type TextId,
} from 'enso-common/src/text'
import { computed, ref, toValue } from 'vue'
export type { DefaultGetText as GetText } from 'enso-common/src/text'

export type TextStore = ReturnType<typeof createTextStore>

/**
 * A composable for getting localized text and setting the language.
 *
 * The composable is used in tests only; the application should use
 * `injectText` instead.
 */
function createTextStore() {
  const language = ref(resolveUserLanguage())
  const locale = computed(() => LANGUAGE_TO_LOCALE[language.value])
  const localizedText = computed(() => getDictionary(language.value))

  const getText: DefaultGetText = (key, ...replacements) =>
    originalGetText(localizedText.value, key, ...replacements)

  function textRef<K extends TextId>(
    key: K,
    ...replacements: MaybeRefOrGetterArray<Replacements[K]>
  ) {
    return computed(() =>
      getText(toValue(key), ...(replacements.map((x) => toValue(x)) as Replacements[K])),
    )
  }

  function setLanguage(lang: Language) {
    language.value = lang
  }

  return proxyRefs({ language, locale, getText, textRef, setLanguage })
}

export const useText = createGlobalState(createTextStore)
