import { cursorDecoratorExt } from '@/components/MarkdownEditor/markdown/decoration/editingAtCursor'
import {
  decorateImageWithClass,
  decorateImageWithRendered,
  linkDecoratorExt,
  markdownLinkEditPopup,
} from '@/components/MarkdownEditor/markdown/decoration/linksAndImages'
import { listDecoratorExt } from '@/components/MarkdownEditor/markdown/decoration/lists'
import { decorateTable } from '@/components/MarkdownEditor/markdown/decoration/table'
import { treeStateDecorator } from '@/components/MarkdownEditor/markdown/decoration/treeStateDecorator'
import { linkDecoratorStateExt } from '@/util/codemirror/links'
import { vueHostExt } from '@/util/codemirror/vueHostExt'
import { type Extension } from '@codemirror/state'

/** Extension applying decorators for Markdown. */
export function markdownDecorators(): Extension {
  return [
    linkDecoratorStateExt,
    vueHostExt,
    treeStateDecorator([decorateImageWithClass, decorateImageWithRendered, decorateTable]),
    linkDecoratorExt(),
    listDecoratorExt(),
    cursorDecoratorExt(),
    markdownLinkEditPopup(),
  ]
}
