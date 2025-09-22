import { cursorDecoratorExt } from '@/components/MarkdownEditor/codemirror/decoration/editingAtCursor'
import {
  decorateImageWithClass,
  decorateImageWithRendered,
  linkDecoratorExt,
  markdownLinkEditPopup,
} from '@/components/MarkdownEditor/codemirror/decoration/linksAndImages'
import { listDecoratorExt } from '@/components/MarkdownEditor/codemirror/decoration/lists'
import { decorateTable } from '@/components/MarkdownEditor/codemirror/decoration/table'
import { treeStateDecorator } from '@/components/MarkdownEditor/codemirror/decoration/treeStateDecorator'
import { linkDecoratorStateExt } from '@/util/codemirror/links'
import { vueHostExt } from '@/util/codemirror/vueHostExt'
import type { Extension } from '@codemirror/state'
import { decorateFrontMatter } from './frontmatter'

/** Extension applying decorators for Markdown. */
export function markdownDecorators(): Extension {
  return [
    linkDecoratorStateExt,
    vueHostExt,
    treeStateDecorator([
      decorateImageWithClass,
      decorateImageWithRendered,
      decorateTable,
      decorateFrontMatter,
    ]),
    linkDecoratorExt(),
    listDecoratorExt(),
    cursorDecoratorExt(),
    markdownLinkEditPopup(),
  ]
}
