import type { LexicalPlugin } from '@/components/lexical'
import {
  $handleListInsertParagraph,
  INSERT_ORDERED_LIST_COMMAND,
  INSERT_UNORDERED_LIST_COMMAND,
  insertList,
  ListItemNode,
  ListNode,
  REMOVE_LIST_COMMAND,
  removeList,
} from '@lexical/list'
import { COMMAND_PRIORITY_LOW, INSERT_PARAGRAPH_COMMAND } from 'lexical'

export const listPlugin: LexicalPlugin = {
  nodes: [ListItemNode, ListNode],
  register: (editor) => {
    editor.registerCommand(
      INSERT_ORDERED_LIST_COMMAND,
      () => {
        insertList(editor, 'number')
        return true
      },
      COMMAND_PRIORITY_LOW,
    )
    editor.registerCommand(
      INSERT_UNORDERED_LIST_COMMAND,
      () => {
        insertList(editor, 'bullet')
        return true
      },
      COMMAND_PRIORITY_LOW,
    )
    editor.registerCommand(
      REMOVE_LIST_COMMAND,
      () => {
        removeList(editor)
        return true
      },
      COMMAND_PRIORITY_LOW,
    )
    editor.registerCommand(
      INSERT_PARAGRAPH_COMMAND,
      $handleListInsertParagraph,
      COMMAND_PRIORITY_LOW,
    )
  },
}
