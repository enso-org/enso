/** @file Provides a Vue reactive API for Markdown formatting in CodeMirror. */
import {
  getBlockType,
  insertCodeBlock,
  removeCodeBlock,
  setBlockType,
} from '@/components/MarkdownEditor/codemirror/formatting/block'
import {
  canInsertLink,
  getInlineFormatting,
  insertLink,
  setInlineFormatting,
  type InlineFormattingNode,
} from '@/components/MarkdownEditor/codemirror/formatting/inline'
import type { SupportedBlockType as BlockType } from '@/components/MarkdownEditor/markdown/types'
import { assert } from '@/util/assert'
import { proxyRefs } from '@/util/reactivity'
import { Facet, type EditorState, type Extension, type TransactionSpec } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import * as objects from 'enso-common/src/utilities/data/object'
import { computed, readonly, ref, type Ref } from 'vue'
export type { BlockType }

interface ReactiveFormatting {
  inline: Record<InlineFormattingNode, Ref<boolean | undefined>>
  blockType: Ref<BlockType | undefined>
  unformattable: Ref<boolean>
}

const reactiveFormattingFacet = Facet.define<ReactiveFormatting, ReactiveFormatting>({
  combine: (values) => values[values.length - 1]!,
})

/** Supports watching and modifying the formatting of the selected text. */
export function useMarkdownFormatting(view: EditorView) {
  const reactiveFormatting = view.state.facet(reactiveFormattingFacet)
  function doEdit(edit: (state: EditorState) => TransactionSpec) {
    view.dispatch(edit(view.state))
    view.focus()
  }
  function inlineFormat(type: InlineFormattingNode) {
    const setter = (value: boolean) => doEdit((state) => setInlineFormatting(state, type, value))
    return proxyRefs({
      value: computed(() => !!reactiveFormatting.inline[type].value),
      set: computed(() => (reactiveFormatting.inline[type] === undefined ? undefined : setter)),
    })
  }
  return {
    italic: inlineFormat('Emphasis'),
    bold: inlineFormat('StrongEmphasis'),
    strikethrough: inlineFormat('Strikethrough'),
    insertLink: computed(() => (canInsertLink(view.state) ? () => doEdit(insertLink) : undefined)),
    insertCodeBlock: computed(() =>
      reactiveFormatting.unformattable.value ? undefined : () => doEdit(insertCodeBlock),
    ),
    blockType: proxyRefs({
      value: readonly(reactiveFormatting.blockType),
      set: (type: BlockType) => {
        const currentType = getBlockType(view.state)
        if (type === currentType) return
        assert(type !== 'FencedCode')
        doEdit((state) =>
          currentType === 'FencedCode' ? removeCodeBlock(state) : setBlockType(state, type),
        )
      },
    }),
  }
}

/** Returns an extension that supports reactively watch the formatting of the selected text. */
export function markdownFormatting(): Extension {
  const reactiveFormatting: ReactiveFormatting = {
    inline: {
      Emphasis: ref(),
      StrongEmphasis: ref(),
      Strikethrough: ref(),
    },
    blockType: ref(),
    unformattable: ref(false),
  }
  const reactiveFormattingFacetExt = reactiveFormattingFacet.of(reactiveFormatting)
  return [
    reactiveFormattingFacetExt,
    EditorView.updateListener.of((update) => {
      if (!update.docChanged && !update.selectionSet) return
      const formatting = getInlineFormatting(update.view.state)
      for (const key of objects.unsafeKeys(reactiveFormatting.inline))
        reactiveFormatting.inline[key].value = formatting?.[key]
      reactiveFormatting.blockType.value = getBlockType(update.view.state)
      reactiveFormatting.unformattable.value = formatting === undefined
    }),
  ]
}
