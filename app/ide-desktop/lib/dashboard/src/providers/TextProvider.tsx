/** @file The React provider for localized, along with hooks to use the provider via the shared
 * React context. */
import * as React from 'react'

import * as text from '#/text'

import * as object from '#/utilities/object'

// ===================
// === TextContext ===
// ===================

/** State contained in a `TextContext`. */
export interface TextContextType {
  readonly language: text.Language
  readonly setLanguage: (newLanguage: text.Language) => void
}

/**
 *
 */
export type GetText = <K extends text.TextId>(
  key: K,
  ...replacements: text.Replacements[K]
) => string

const TextContext = React.createContext<TextContextType>({
  language: text.Language.english,
  /** Set `this.language`. It is NOT RECOMMENDED to use the default value, as this does not trigger
   * reactive updates. */
  setLanguage(language) {
    object.unsafeMutable(this).language = language
  },
})

/** Props for a {@link TextProvider}. */
export interface TextProviderProps extends Readonly<React.PropsWithChildren> {}

// ====================
// === TextProvider ===
// ====================

/** A React Provider that lets components get the current language. */
export default function TextProvider(props: TextProviderProps) {
  const { children } = props
  const [language, setLanguage] = React.useState(text.Language.english)

  return <TextContext.Provider value={{ language, setLanguage }}>{children}</TextContext.Provider>
}

/** Exposes a property to get localized text, and get and set the current language. */
export function useText() {
  const { language, setLanguage } = React.useContext(TextContext)
  const localizedText = text.TEXTS[language]

  const getText = React.useCallback<GetText>(
    (key, ...replacements) => {
      const template = localizedText[key]
      return replacements.length === 0
        ? template
        : template.replace(/[$]([$]|\d+)/g, (_match, placeholder: string) =>
            placeholder === '$'
              ? '$'
              : String(replacements[Number(placeholder)] ?? `$${placeholder}`)
          )
    },
    [localizedText]
  )

  return { language, setLanguage, getText } as const
}
