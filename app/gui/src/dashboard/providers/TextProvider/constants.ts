/** @file Constants related to the `TextProvider`. */
import {
  resolveUserLanguage,
  type Language,
  type Replacements,
  type TextId,
} from 'enso-common/src/text'
import { unsafeMutable } from 'enso-common/src/utilities/data/object'
import { createContext } from 'react'

/**
 * A function that gets localized text for a given key, with optional replacements.
 * @param key - The key of the text to get.
 * @param replacements - The replacements to insert into the text.
 * If the text contains placeholders like `$0`, `$1`, etc.,
 * they will be replaced with the corresponding replacement.
 */
export type GetText = <K extends TextId>(key: K, ...replacements: Replacements[K]) => string

/** State contained in a `TextContext`. */
export interface TextContextType {
  readonly language: Language
  readonly locale: string
  readonly setLanguage: (newLanguage: Language) => void
}

export const TextContext = createContext<TextContextType>({
  language: resolveUserLanguage(),
  locale: navigator.language,
  /**
   * Set `this.language`. It is NOT RECOMMENDED to use the default value, as this does not trigger
   * reactive updates.
   */
  setLanguage(language) {
    unsafeMutable(this).language = language
  },
})
