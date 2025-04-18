/**
 * @file The React provider for localized, along with hooks to use the provider via the shared
 * React context.
 */
import { LANGUAGE_TO_LOCALE, resolveUserLanguage } from 'enso-common/src/text'
import { useState, type PropsWithChildren } from 'react'
import { TextContext } from './constants'

/** Props for a {@link TextProvider}. */
export type TextProviderProps = Readonly<PropsWithChildren>

/** A React Provider that lets components get the current language. */
export function TextProvider(props: TextProviderProps) {
  const { children } = props

  const [language, setLanguage] = useState(() => resolveUserLanguage())
  const locale = LANGUAGE_TO_LOCALE[language]

  return (
    <TextContext.Provider value={{ language, setLanguage, locale }}>
      {children}
    </TextContext.Provider>
  )
}
