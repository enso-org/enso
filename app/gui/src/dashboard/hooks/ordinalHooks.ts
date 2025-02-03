/** @file A hook returning a function to get the ordinal string of a number. */
import { useText } from '#/providers/TextProvider'
import { useLocale } from 'react-aria'

/** A hook returning a function to get the ordinal string of a number. */
export function useGetOrdinal() {
  const { getText } = useText()
  const { locale } = useLocale()

  const pluralRules = new Intl.PluralRules(locale, { type: 'ordinal' })

  const suffixes = new Map([
    ['one', getText('pluralOne')],
    ['two', getText('pluralTwo')],
    ['few', getText('pluralFew')],
    ['other', getText('pluralOther')],
  ])

  return (n: number) => {
    const pluralRule = pluralRules.select(n)
    const suffix = suffixes.get(pluralRule)
    return `${n}${suffix}`
  }
}
