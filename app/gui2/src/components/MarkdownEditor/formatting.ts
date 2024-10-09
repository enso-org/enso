import { lexicalTheme } from '@/components/lexical'
import { useBufferedWritable } from '@/util/reactivity'
import { $createCodeNode } from '@lexical/code'
import {
  $isListNode,
  INSERT_ORDERED_LIST_COMMAND,
  INSERT_UNORDERED_LIST_COMMAND,
  ListNode,
} from '@lexical/list'
import {
  $createHeadingNode,
  $createQuoteNode,
  $isHeadingNode,
  $isQuoteNode,
  type HeadingTagType,
} from '@lexical/rich-text'
import { $setBlocksType } from '@lexical/selection'
import { $isTableSelection } from '@lexical/table'
import {
  $findMatchingParent,
  $getNearestBlockElementAncestorOrThrow,
  $getNearestNodeOfType,
} from '@lexical/utils'
import type { EditorThemeClasses, LexicalEditor, RangeSelection, TextFormatType } from 'lexical'
import {
  $createParagraphNode,
  $getSelection,
  $isRangeSelection,
  $isRootOrShadowRoot,
  $isTextNode,
  COMMAND_PRIORITY_LOW,
  FORMAT_TEXT_COMMAND,
  SELECTION_CHANGE_COMMAND,
} from 'lexical'
import { ref } from 'vue'

/** TODO: Add docs */
export function useFormatting(editor: LexicalEditor) {
  const selectionReaders = new Array<(selection: RangeSelection) => void>()
  function onReadSelection(reader: (selection: RangeSelection) => void) {
    selectionReaders.push(reader)
  }
  function $readState() {
    const selection = $getSelection()
    if ($isRangeSelection(selection)) {
      for (const reader of selectionReaders) {
        reader(selection)
      }
    }
  }
  editor.registerUpdateListener(({ editorState }) => {
    editorState.read($readState)
  })
  editor.registerCommand(
    SELECTION_CHANGE_COMMAND,
    (_payload, _newEditor) => {
      $readState()
      return false
    },
    COMMAND_PRIORITY_LOW,
  )
  return {
    bold: useFormatProperty(editor, 'bold', onReadSelection),
    italic: useFormatProperty(editor, 'italic', onReadSelection),
    strikethrough: useFormatProperty(editor, 'strikethrough', onReadSelection),
    subscript: useFormatProperty(editor, 'subscript', onReadSelection),
    superscript: useFormatProperty(editor, 'superscript', onReadSelection),
    blockType: useBlockType(editor, onReadSelection),
    clearFormatting: () => editor.update($clearFormatting),
  }
}
export type UseFormatting = ReturnType<typeof useFormatting>

function useFormatProperty(
  editor: LexicalEditor,
  property: TextFormatType,
  onReadSelection: ($readSelection: (selection: RangeSelection) => void) => void,
) {
  const state = ref(false)

  onReadSelection((selection) => (state.value = selection.hasFormat(property)))

  // The editor only exposes a toggle interface, so we need to model its state to ensure the setter is only called when
  // the desired value is different from its current value.
  const writable = useBufferedWritable({
    get: state,
    set: (_value: boolean) => editor.dispatchCommand(FORMAT_TEXT_COMMAND, property),
  })

  return { state, set: (value: boolean) => (writable.value = value) }
}

function $clearFormatting() {
  const selection = $getSelection()
  if ($isRangeSelection(selection) || $isTableSelection(selection)) {
    const anchor = selection.anchor
    const focus = selection.focus
    const nodes = selection.getNodes()
    const extractedNodes = selection.extract()

    if (anchor.key === focus.key && anchor.offset === focus.offset) {
      return
    }

    nodes.forEach((node, idx) => {
      // We split the first and last node by the selection
      // So that we don't format unselected text inside those nodes
      if ($isTextNode(node)) {
        // Use a separate variable to ensure TS does not lose the refinement
        let textNode = node
        if (idx === 0 && anchor.offset !== 0) {
          textNode = textNode.splitText(anchor.offset)[1] || textNode
        }
        if (idx === nodes.length - 1) {
          textNode = textNode.splitText(focus.offset)[0] || textNode
        }
        /**
         * If the selected text has one format applied
         * selecting a portion of the text, could
         * clear the format to the wrong portion of the text.
         *
         * The cleared text is based on the length of the selected text.
         */
        // We need this in case the selected text only has one format
        const extractedTextNode = extractedNodes[0]
        if (nodes.length === 1 && $isTextNode(extractedTextNode)) {
          textNode = extractedTextNode
        }

        if (textNode.__style !== '') {
          textNode.setStyle('')
        }
        if (textNode.__format !== 0) {
          textNode.setFormat(0)
          $getNearestBlockElementAncestorOrThrow(textNode).setFormat('')
        }
        node = textNode
      } else if ($isHeadingNode(node) || $isQuoteNode(node)) {
        node.replace($createParagraphNode(), true)
      }
    })
  }
}

export const blockTypeToBlockName = {
  bullet: 'Bulleted List',
  code: 'Code Block',
  h1: 'Heading 1',
  h2: 'Heading 2',
  h3: 'Heading 3',
  number: 'Numbered List',
  paragraph: 'Normal',
  quote: 'Quote',
}
export type BlockType = keyof typeof blockTypeToBlockName
export const blockTypes = Object.keys(blockTypeToBlockName) as BlockType[]

const smallestEnabledHeading = ['h6', 'h5', 'h4', 'h3', 'h2', 'h1'].find(
  isBlockType,
) as HeadingTagType & BlockType

function isBlockType(value: string): value is BlockType {
  return value in blockTypeToBlockName
}

/** TODO: Add docs */
export function normalizeHeadingLevel(heading: HeadingTagType): HeadingTagType & BlockType {
  return isBlockType(heading) ? heading : smallestEnabledHeading
}

function useBlockType(
  editor: LexicalEditor,
  onReadSelection: ($readSelection: (selection: RangeSelection) => void) => void,
) {
  const state = ref<BlockType>('paragraph')

  onReadSelection((selection) => (state.value = $getBlockType(selection) ?? 'paragraph'))

  function $getBlockType(selection: RangeSelection): BlockType | undefined {
    const anchorNode = selection.anchor.getNode()
    const element =
      anchorNode.getKey() === 'root' ?
        anchorNode
      : $findMatchingParent(anchorNode, (e) => {
          const parent = e.getParent()
          return parent !== null && $isRootOrShadowRoot(parent)
        }) ?? anchorNode.getTopLevelElementOrThrow()

    if ($isListNode(element)) {
      const parentList = $getNearestNodeOfType<ListNode>(anchorNode, ListNode)
      const type = parentList ? parentList.getListType() : element.getListType()
      if (type in blockTypeToBlockName) {
        return type as keyof typeof blockTypeToBlockName
      }
    } else if ($isHeadingNode(element)) {
      return normalizeHeadingLevel(element.getTag())
    } else {
      const type = element.getType()
      if (type in blockTypeToBlockName) {
        return type as keyof typeof blockTypeToBlockName
      }
    }
  }

  const $setBlockType: Record<BlockType, () => void> = {
    bullet: () => editor.dispatchCommand(INSERT_UNORDERED_LIST_COMMAND, undefined),
    number: () => editor.dispatchCommand(INSERT_ORDERED_LIST_COMMAND, undefined),
    paragraph: () => {
      const selection = $getSelection()
      if ($isRangeSelection(selection)) {
        $setBlocksType(selection, () => $createParagraphNode())
      }
    },
    quote: () => $setBlocksType($getSelection(), () => $createQuoteNode()),
    code: () => {
      let selection = $getSelection()
      if (selection !== null) {
        if (selection.isCollapsed()) {
          $setBlocksType(selection, () => $createCodeNode())
        } else {
          const textContent = selection.getTextContent()
          const codeNode = $createCodeNode()
          selection.insertNodes([codeNode])
          selection = $getSelection()
          if ($isRangeSelection(selection)) {
            selection.insertRawText(textContent)
          }
        }
      }
    },
    h1: () => $setBlocksType($getSelection(), () => $createHeadingNode('h1')),
    h2: () => $setBlocksType($getSelection(), () => $createHeadingNode('h2')),
    h3: () => $setBlocksType($getSelection(), () => $createHeadingNode('h3')),
  }

  return {
    state,
    set: (value: BlockType) => editor.update($setBlockType[value]),
  }
}

/** TODO: Add docs */
export function lexicalRichTextTheme(themeCss: Record<string, string>): EditorThemeClasses {
  const theme = lexicalTheme(themeCss)
  if (theme.heading) {
    for (const level of Object.keys(theme.heading)) {
      const levelTag = level as keyof typeof theme.heading
      const normalized = normalizeHeadingLevel(levelTag)
      theme.heading[levelTag] = theme.heading[normalized] ?? 'lexical-unsupported-heading-level'
    }
  }
  return theme
}
