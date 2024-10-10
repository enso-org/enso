/**
 * @file The React provider for localized, along with hooks to use the provider via the shared
 * React context.
 */
import * as React from 'react'

import * as text from 'enso-common/src/text'

import * as object from '#/utilities/object'

// ===================
// === TextContext ===
// ===================

/** State contained in a `TextContext`. */
export interface TextContextType {
  readonly language: text.Language
  readonly locale: string
  readonly setLanguage: (newLanguage: text.Language) => void
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

const DEFAULT_LANGUAGE = text.Language.english

const TextContext = React.createContext<TextContextType>({
  language: DEFAULT_LANGUAGE,
  locale: text.LANGUAGE_TO_LOCALE[DEFAULT_LANGUAGE],
  /**
   * Set `this.language`. It is NOT RECOMMENDED to use the default value, as this does not trigger
   * reactive updates.
   */
  setLanguage(language) {
    object.unsafeMutable(this).language = language
  },
})

/** Props for a {@link TextProvider}. */
export type TextProviderProps = Readonly<React.PropsWithChildren>

// ====================
// === TextProvider ===
// ====================

/** A React Provider that lets components get the current language. */
export default function TextProvider(props: TextProviderProps) {
  const { children } = props

  const [language, setLanguage] = React.useState(text.Language.english)
  const locale = text.LANGUAGE_TO_LOCALE[language]

  return (
    <TextContext.Provider value={{ language, setLanguage, locale }}>
      {children}
    </TextContext.Provider>
  )
}

/** Exposes a property to get localized text, and get and set the current language. */
export function useText() {
  const { language, setLanguage, locale } = React.useContext(TextContext)
  const localizedText = text.TEXTS[language]

  const getText = React.useCallback<GetText>(
    (key, ...replacements) => {
      const template = localizedText[key]
      return replacements.length === 0 ?
          template
        : template.replace(/[$]([$]|\d+)/g, (_match, placeholder: string) =>
            placeholder === '$' ? '$' : (
              String(replacements[Number(placeholder)] ?? `$${placeholder}`)
            ),
          )
    },
    [localizedText],
  )

  return { language, setLanguage, getText, locale } as const
}
