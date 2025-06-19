/** @file Widths for Assets Table columns. */
import type { ColumnProps } from '#/components/aria'
import { useStore } from '#/hooks/storeHooks'
import { Column } from '#/pages/dashboard/components/column/columnUtils'
import { mapEntries, unsafeEntries } from '#/utilities/object'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

/** The size of a column for a React Aria table. */
type ColumnSize = NonNullable<ColumnProps['width']>

/** Assets table column widths store. */
export interface AssetsTableColumnWidthsStore {
  readonly widths: Readonly<Record<Column, ColumnSize>>
  readonly setWidths: (widths: Readonly<Record<Column, ColumnSize>>) => void
}

export const assetsTableColumnWidthsStore = createStore<AssetsTableColumnWidthsStore>()(
  persist(
    (set, get) => ({
      widths: {
        /* eslint-disable @typescript-eslint/no-magic-numbers */
        [Column.name]: 320,
        [Column.modified]: 133,
        [Column.sharedWith]: 160,
        [Column.labels]: 320,
        [Column.accessedByProjects]: 384,
        [Column.accessedData]: 384,
        [Column.path]: 320,
        /* eslint-enable @typescript-eslint/no-magic-numbers */
      },
      setWidths: (widths) => {
        const currentWidths = get().widths
        if (unsafeEntries(currentWidths).some(([column, width]) => widths[column] !== width)) {
          set({ widths })
        }
      },
    }),
    {
      name: 'enso-column-widths',
      version: 1,
      merge: (persisted, current) => {
        if (
          typeof persisted !== 'object' ||
          persisted == null ||
          !('widths' in persisted) ||
          typeof persisted.widths !== 'object' ||
          persisted.widths == null
        ) {
          return current
        }
        const persistedWidths: Record<string, unknown> = { ...persisted.widths }
        return {
          ...current,
          widths: mapEntries(current.widths, (column, width) => {
            const persistedEntry = persistedWidths[column]
            if (typeof persistedEntry !== 'string' && typeof persistedEntry !== 'number') {
              return width
            }
            // eslint-disable-next-line no-restricted-syntax
            return persistedEntry as ColumnSize
          }),
        }
      },
    },
  ),
)

/** Widths for assets table columns. */
export function useAssetsTableColumnWidths() {
  return useStore(assetsTableColumnWidthsStore, ({ widths }) => widths, {
    unsafeEnableTransition: true,
  })
}

/** A function to set widths for assets table columns. */
export function useSetAssetsTableColumnWidths() {
  return useStore(assetsTableColumnWidthsStore, ({ setWidths }) => setWidths, {
    unsafeEnableTransition: true,
  })
}
