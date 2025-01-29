/**
 * Convert the given HTML text to a Markdown approximation. The supporting libraries will be loaded the first time this
 * is called.
 */
export async function htmlToMarkdown(html: string): Promise<string> {
  const { htmlToMarkdownImpl } = await import(
    '@/components/MarkdownEditor/htmlToMarkdown/htmlToMarkdownImpl'
  )
  return htmlToMarkdownImpl(html)
}
