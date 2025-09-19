import { documentationEditorFormatBindings } from '@/bindings'
import { actionBlockType, type BlockTypeAction } from '@/components/MarkdownEditor/blockTypeActions'
import type { useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror'
import { registerHandlers, type Action, type ActionHandler } from '@/providers/action'
import { handlerToKeyBinding } from '@/util/codemirror/keymap'
import type { ToValue } from '@/util/reactivity'
import { useToast } from '@/util/toast'
import { keymap } from '@codemirror/view'
import * as objects from 'enso-common/src/utilities/data/object'
import { computed, toValue } from 'vue'

interface FormatActionsOptions {
  formatting: ReturnType<typeof useMarkdownFormatting>
  readonly: ToValue<boolean>
  editing: ToValue<boolean>
  uploadImage: (() => void) | undefined
}

/** Registers actions for the given editor's format state. */
export function useFormatActions({
  formatting: { italic, bold, insertLink, insertCodeBlock, blockType },
  readonly,
  editing,
  uploadImage,
}: FormatActionsOptions) {
  const toastError = useToast.error()

  const notReadonly = computed(() => !toValue(readonly))

  function reportUnformattable() {
    toastError.show('The selected text cannot be formated')
  }

  function toggleFormatAction(format: {
    set: ((value: boolean) => void) | undefined
    value: boolean
  }) {
    return {
      available: notReadonly,
      enabled: () => !!toValue(editing) && !!format.set,
      toggled: () => format.value,
      action: () => {
        if (!format.set) {
          reportUnformattable()
          return
        }
        format.set(!format.value)
      },
    } satisfies Action
  }

  function doFormatAction(action: ToValue<(() => void) | undefined>) {
    return {
      available: notReadonly,
      enabled: () => toValue(action) != null,
      action: () => {
        const actionValue = toValue(action)
        if (!actionValue) {
          reportUnformattable()
          return
        }
        actionValue()
      },
    } satisfies Action
  }

  function setBlockTypeActions<T extends BlockTypeAction>(
    actions: T[],
  ): Record<T, ActionHandler & { action: () => void }> {
    const setBlockTypeAction = (actionName: BlockTypeAction) =>
      ({
        available: notReadonly,
        action: () => blockType.set(actionBlockType[actionName]),
      }) satisfies Action
    return objects.unsafeFromEntries(
      actions.map((actionName) => [actionName, setBlockTypeAction(actionName)]),
    )
  }

  const actionHandlers = registerHandlers({
    'documentationEditor.italic': toggleFormatAction(italic),
    'documentationEditor.bold': toggleFormatAction(bold),
    'documentationEditor.link': doFormatAction(insertLink),
    'documentationEditor.code': doFormatAction(insertCodeBlock),
    'documentationEditor.image': {
      available: notReadonly,
      enabled: () => toValue(editing) && uploadImage != null,
      action: () => uploadImage?.(),
    },
    ...setBlockTypeActions([
      'documentationEditor.header1',
      'documentationEditor.header2',
      'documentationEditor.header3',
      'documentationEditor.paragraph',
    ]),
  })

  const formatBindings = keymap.of([
    handlerToKeyBinding(
      documentationEditorFormatBindings.handler(
        objects.mapEntries(
          documentationEditorFormatBindings.bindings,
          (actionName) => actionHandlers[actionName].action,
        ),
      ),
      true,
    ),
  ])

  return { actions: actionHandlers, formatBindings }
}
