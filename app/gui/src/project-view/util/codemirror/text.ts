import type { ChangeDesc, ChangeSet } from '@codemirror/state'
import { SourceRange, SourceRangeEdit, SourceRangeEditDesc } from 'ydoc-shared/util/data/text'

/** Collect the changes in a {@link ChangeSet} as {@link SourceRangeEdit}s. */
export function changeSetToTextEdits(changes: ChangeSet): SourceRangeEdit[] {
  const textEdits = new Array<SourceRangeEdit>()
  changes.iterChanges((from, to, _fromB, _toB, insert) =>
    textEdits.push(
      SourceRangeEdit.replace(SourceRange.unsafeFromBounds(from, to), insert.toString()),
    ),
  )
  return textEdits
}

/** Collect the change descriptions in a {@link ChangeDesc} as {@link SourceRangeEditDesc}s. */
export function changeDescToSourceRangeEditDesc(changeDesc: ChangeDesc): SourceRangeEditDesc[] {
  const textEdits = new Array<SourceRangeEditDesc>()
  changeDesc.iterChangedRanges((fromA, toA, fromB, toB) => {
    textEdits.push(
      SourceRangeEditDesc.replace(SourceRange.unsafeFromBounds(fromA, toA), {
        length: toB - fromB,
      }),
    )
  })
  return textEdits
}
