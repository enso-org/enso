/** @file The React provider for localized, along with hooks to use the provider via the shared
 * React context. */
import * as React from 'react'

import * as text from '#/text'

// ===================
// === TextContext ===
// ===================

/** State contained in a `TextContext`. */
export interface TextContextType {
    language: text.Language
    setLanguage: (newLanguage: text.Language) => void
}

const TextContext = React.createContext<TextContextType>({
    language: text.Language.english,
    /** Set `this.language`. */
    setLanguage(language) {
        this.language = language
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

    const getText = React.useCallback((key: text.TextId) => localizedText[key], [localizedText])

    return { language, setLanguage, getText }
}
