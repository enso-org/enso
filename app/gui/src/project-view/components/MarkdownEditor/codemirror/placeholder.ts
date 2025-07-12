import { Range, RangeSet, RangeValue, StateEffect, StateField } from '@codemirror/state'
import { EditorView } from '@codemirror/view'

/**
 * A placeholder instance created by `insertPlaceholder` and consumed by `replacePlaceholder`.
 * Each created placeholder can only be replaced once.
 */
export class ReplaceablePlaceholder extends RangeValue {}

/**
 * Insert a text placeholder that can be replaced at later time.
 *
 * Requires `uploadPlaceholders` extension to be present on the editor view.
 *
 */
export function insertPlaceholder(
  editorView: EditorView,
  from: number,
  to: number,
  insert: string,
): ReplaceablePlaceholder {
  const p = new ReplaceablePlaceholder()
  editorView.dispatch({
    changes: [{ from, to, insert }],
    effects: [insertPlaceholderEffect.of(p.range(from, from + insert.length))],
  })
  return p
}

/**
 * Replace a previously prepared placeholder with specified text.
 * Once `cleanupPlaceholder` is passed as `true`, the placeholder metadata will be removed
 * and it will not be usable anymore.
 *
 * Requires `uploadPlaceholders` extension to be present on the editor view.
 */
export function replacePlaceholder(
  editorView: EditorView,
  p: ReplaceablePlaceholder,
  insert: string,
  cleanupPlaceholder = true,
) {
  const cursor = editorView.state.field(replaceablePlaceholders).iter()
  while (cursor.value) {
    if (cursor.value === p) {
      editorView.dispatch({
        changes: [{ from: cursor.from, to: cursor.to, insert }],
        effects: cleanupPlaceholder ? [removePlaceholderEffect.of(p)] : [],
      })
      break
    }
    cursor.next()
  }
}

const insertPlaceholderEffect = StateEffect.define<Range<ReplaceablePlaceholder>>()
const removePlaceholderEffect = StateEffect.define<ReplaceablePlaceholder>()

/**
 * The placeholder state extension for `codemirror`.
 */
export const replaceablePlaceholders = StateField.define<RangeSet<ReplaceablePlaceholder>>({
  create: () => RangeSet.empty,
  update: (value, tx) => {
    const inserts = tx.effects.filter((e) => e.is(insertPlaceholderEffect)).map((e) => e.value)
    const removes = tx.effects.filter((e) => e.is(removePlaceholderEffect)).map((e) => e.value)
    const removed = removes.length > 0 ? new Set(removes) : null
    return value.map(tx.changes).update({
      add: inserts,
      ...(removed == null ? {} : { filter: (_f, _t, value) => !removed.has(value) }),
    })
  },
})
