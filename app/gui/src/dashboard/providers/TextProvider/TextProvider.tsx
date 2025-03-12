/**
 * @file The React provider for localized, along with hooks to use the provider via the shared
 * React context.
 */
import { LANGUAGE_TO_LOCALE, resolveUserLanguage } from 'enso-common/src/text'
import { useMemo, useState, type PropsWithChildren } from 'react'
import { TextContext, type TextContextType } from './constants'

/** Props for a {@link TextProvider}. */
export type TextProviderProps = Readonly<PropsWithChildren>

/** A React Provider that lets components get the current language. */
export function TextProvider(props: TextProviderProps) {
  const { children } = props

  const [language, setLanguage] = useState(() => resolveUserLanguage())
  const locale = LANGUAGE_TO_LOCALE[language]

  const contextValue = useMemo<TextContextType>(
    () => ({ language, setLanguage, locale }),
    [language, locale],
  )

  return <TextContext.Provider value={contextValue}>{children}</TextContext.Provider>
}
