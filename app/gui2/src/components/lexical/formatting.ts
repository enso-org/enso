import type { LexicalEditor, RangeSelection, TextFormatType } from 'lexical'
import {
  $getSelection,
  $isRangeSelection,
  COMMAND_PRIORITY_LOW,
  FORMAT_TEXT_COMMAND,
  SELECTION_CHANGE_COMMAND,
} from 'lexical'
import { computed, ref } from 'vue'

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
  }
}

function useFormatProperty(
  editor: LexicalEditor,
  property: TextFormatType,
  onReadSelection: ($readSelection: (selection: RangeSelection) => void) => void,
) {
  const state = ref(false)

  onReadSelection((selection) => (state.value = selection.hasFormat(property)))

  return computed({
    get: () => state.value,
    set: (value) => {
      if (value !== state.value) editor.dispatchCommand(FORMAT_TEXT_COMMAND, property)
    },
  })
}
