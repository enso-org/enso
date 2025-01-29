import { LINKABLE_URL_REGEX } from '@/util/link'

function uriEscapeChar(char: string) {
  return `%${char.codePointAt(0)!.toString(16).toUpperCase().padStart(2, '0')}`
}

function toAutoLink(text: string) {
  return `<${text.replaceAll(/[\][<>*`]/g, uriEscapeChar)}>`
}

/** Convert the input to Markdown. This includes converting any likely URLs to <autolink>s. */
export function transformPastedText(text: string): string {
  return text.replaceAll(LINKABLE_URL_REGEX, toAutoLink)
}
