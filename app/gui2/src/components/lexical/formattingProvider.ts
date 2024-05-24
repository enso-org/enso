import { useFormatting } from '@/components/lexical/formatting'
import { createContextStore } from '@/providers'
import type { LexicalEditor } from 'lexical'

export { injectFn as injectFormatting, provideFn as provideFormatting }
const { provideFn, injectFn } = createContextStore('Lexical formatting', (editor: LexicalEditor) =>
  useFormatting(editor),
)
