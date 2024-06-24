import { $getSelectedLinkNode } from '@/components/MarkdownEditor/LinkPlugin'
import { COMMAND_PRIORITY_LOW, SELECTION_CHANGE_COMMAND, type LexicalEditor } from 'lexical'
import { shallowRef } from 'vue'

export function useLinkNode(editor: LexicalEditor) {
  const urlUnderCursor = shallowRef<string>()
  editor.registerCommand(
    SELECTION_CHANGE_COMMAND,
    () => {
      urlUnderCursor.value = $getSelectedLinkNode()?.getURL()
      return true
    },
    COMMAND_PRIORITY_LOW,
  )
  return { urlUnderCursor }
}
