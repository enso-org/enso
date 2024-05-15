import {
  $createParagraphNode,
  $createTextNode,
  $getRoot,
  $setSelection,
  COMMAND_PRIORITY_NORMAL,
  type LexicalCommand,
  type LexicalEditor,
} from 'lexical'

export function $setRootText(text: string) {
  if (text === $getRoot().getTextContent()) return
  const root = $getRoot()
  root.clear()
  const paragraph = $createParagraphNode()
  paragraph.append($createTextNode(text))
  root.append(paragraph)
  $setSelection(null)
}

export function useBindKeyCommand(
  editor: LexicalEditor,
  keyCommand: LexicalCommand<KeyboardEvent | null>,
  onPress?: () => void,
) {
  editor.registerCommand(
    keyCommand,
    (event) => {
      if (event) {
        event.preventDefault()
        event.stopImmediatePropagation()
      }
      onPress?.()
      return true
    },
    COMMAND_PRIORITY_NORMAL,
  )
}
