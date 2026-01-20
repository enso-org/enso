import type { EditorState, Extension } from '@codemirror/state'
import { StateField } from '@codemirror/state'
import { type Tooltip, showTooltip } from '@codemirror/view'

/**
 * Creates an extension that uses the given function to provide a tooltip based on the cursor position. If there are
 * multiple cursors, no tooltip is shown.
 */
export function singleCursorTooltipExtension(
  getTooltip: (state: EditorState) => Tooltip | null,
): Extension {
  return StateField.define<Tooltip | null>({
    create: getTooltip,

    update(prev, tr) {
      if (!tr.docChanged && !tr.selection) return prev
      if (tr.state.selection.ranges.length !== 1) return null
      return getTooltip(tr.state)
    },

    provide: (f) => showTooltip.from(f),
  })
}
