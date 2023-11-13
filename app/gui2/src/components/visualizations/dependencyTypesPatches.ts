// Fixes and extensions for dependencies' type definitions.

import type { BaseType, Selection } from 'd3'

declare module 'd3' {
  function select<GElement extends BaseType, OldDatum>(
    node: GElement | null | undefined,
  ): Selection<GElement, OldDatum, null, undefined>

  // These type parameters are defined on the original interface.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface ScaleSequential<Output, Unknown = never> {
    ticks(): number[]
  }
}

declare module '@ag-grid-community/core' {
  // These type parameters are defined on the original interface.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface ColDef<TData, TValue> {
    /** Custom user-defined value. */
    manuallySized: boolean
  }

  // These type parameters are defined on the original interface.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface AbstractColDef<TData, TValue> {
    field: string
  }
}
