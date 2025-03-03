/** @file Provides a Vue reactive API for Markdown formatting in CodeMirror. */
import {
  HeaderLevel,
  ListType,
  toggleHeader,
  toggleList,
  toggleQuote,
} from '@/components/MarkdownEditor/codemirror/formatting/block'
import {
  canInsertLink,
  getInlineFormatting,
  type InlineFormattingNode,
  insertLink,
  setInlineFormatting,
} from '@/components/MarkdownEditor/codemirror/formatting/inline'
import { type Extension, Facet, Prec } from '@codemirror/state'
import { type EditorView, ViewPlugin, type ViewUpdate } from '@codemirror/view'
import * as objects from 'enso-common/src/utilities/data/object'
import { computed, proxyRefs, readonly, type Ref, ref } from 'vue'

type ReactiveFormatting = Record<InlineFormattingNode, Ref<boolean | undefined>>
const reactiveFormattingFacet = Facet.define<ReactiveFormatting, ReactiveFormatting>({
  combine: (values) => values[values.length - 1]!,
})

/** Supports watching and modifying the formatting of the selected text. */
export function useMarkdownFormatting(view: EditorView) {
  const reactiveFormatting = view.state.facet(reactiveFormattingFacet)
  function inlineFormat(type: InlineFormattingNode) {
    return proxyRefs({
      value: readonly(reactiveFormatting[type]),
      set: (value: boolean) => view.dispatch(setInlineFormatting(view.state, type, value)),
    })
  }
  return {
    toggleHeader: (level: HeaderLevel) => toggleHeader(view, level),
    toggleQuote: () => toggleQuote(view),
    toggleList: (type: ListType) => toggleList(view, type),
    italic: inlineFormat('Emphasis'),
    bold: inlineFormat('StrongEmphasis'),
    strikethrough: inlineFormat('Strikethrough'),
    insertLink: computed(
      () => canInsertLink(view.state) && (() => view.dispatch(insertLink(view.state))),
    ),
  }
}

/** Returns an extension that supports reactively watch the formatting of the selected text. */
export function markdownFormatting(): Extension {
  const reactiveFormatting = {
    Emphasis: ref<boolean>(),
    StrongEmphasis: ref<boolean>(),
    Strikethrough: ref<boolean>(),
  }
  const reactiveFormattingFacetExt = reactiveFormattingFacet.of(reactiveFormatting)
  return [
    reactiveFormattingFacetExt,
    viewObserverExt((update) => {
      if (!update.docChanged && !update.selectionSet) return
      const formatting = getInlineFormatting(update.view.state)
      for (const key of objects.unsafeKeys(reactiveFormatting))
        reactiveFormatting[key].value = formatting?.[key]
    }),
  ]
}

/** Returns an extension that calls the given callback when the view is updated. */
function viewObserverExt(onUpdate: (update: ViewUpdate) => void): Extension {
  return Prec.lowest(
    ViewPlugin.fromClass(
      class {
        update(update: ViewUpdate) {
          onUpdate(update)
        }
      },
    ),
  )
}
