/** @file Hooks related to `TexrProvider`. */
import { getDictionary, getText as originalGetText } from 'enso-common/src/text'
import { useCallback, useContext } from 'react'
import { TextContext, type GetText } from './constants'

/** Exposes a property to get localized text, and get and set the current language. */
export function useText() {
  const { language, setLanguage, locale } = useContext(TextContext)

  const localizedText = getDictionary(language)

  const getText = useCallback<GetText>(
    (key, ...replacements) => {
      return originalGetText(localizedText, key, ...replacements)
    },
    [localizedText],
  )

  return { language, setLanguage, getText, locale } as const
}
