// Fixes and extensions for dependencies' type definitions.

declare module 'd3' {
  function select<GElement extends import('d3').BaseType, OldDatum>(
    node: GElement | null | undefined,
  ): import('d3').Selection<GElement, OldDatum, null, undefined>

  // These type parameters are defined on the original interface.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface ScaleSequential<Output, Unknown = never> {
    ticks(): number[]
  }
}

import {} from 'ag-grid-community'

declare module 'ag-grid-community' {
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
