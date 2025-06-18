/** @file Widths for Assets Table columns. */
import type { ResizableTableContainer } from '#/components/aria'
import { useStore } from '#/hooks/storeHooks'
import { Column } from '#/pages/dashboard/components/column/columnUtils'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

/** The size of a column for a React Aria table. */
type ColumnSize = NonNullable<
  ReturnType<
    Parameters<NonNullable<Parameters<typeof ResizableTableContainer>[0]['onResize']>>[0]['get']
  >
>

/** Assets table column widths store. */
export interface AssetsTableColumnWidthsStore {
  readonly widths: Readonly<Record<Column, ColumnSize>>
  readonly setWidths: (widths: Readonly<Record<Column, ColumnSize>>) => void
}

export const assetsTableColumnWidthsStore = createStore<AssetsTableColumnWidthsStore>()(
  persist(
    (set) => ({
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
        set({ widths })
      },
    }),
    {
      name: 'enso-column-widths',
      version: 1,
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
