// Fixes and extensions for dependencies' type definitions.

declare module 'd3' {
  // d3 treats `null` and `undefined` as a selection of 0 elements, so they are a valid selection
  // for any element type.
  function select<GElement extends import('d3').BaseType, OldDatum>(
    node: GElement | null | undefined,
  ): import('d3').Selection<GElement, OldDatum, null, undefined>

  // These type parameters are present on the original type.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface ScaleSequential<Output, Unknown = never> {
    // This field exists in the code but not in the typings.
    ticks(): number[]
  }
}

import {} from 'ag-grid-community'

declare module 'ag-grid-community' {
  // These type parameters are present on the original type.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface ColDef<TData, TValue> {
    /** Custom user-defined value. */
    manuallySized: boolean
  }

  // These type parameters are present on the original type.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface AbstractColDef<TData, TValue> {
    // This field exists in the code but not in the typings.
    field: string
  }
}
