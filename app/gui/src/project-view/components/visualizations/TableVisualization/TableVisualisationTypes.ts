// IMPORTANT - The types here must be kept in sync with those in Table_Viz_Data.enso
// AND the mocks in tableVisualisation.spec.ts

/** Table Viz for errors. */
export interface Error {
  type: 'Error'
  error: string
  all_rows_count?: undefined
}

/** Is this a Table Viz for errors? */
export function isError(data: unknown): data is Error {
  return (
    typeof data === 'object' && data !== null && 'type' in data && (data as any).type === 'Error'
  )
}

export interface GenericGrid {
  type: 'Generic_Grid'
  headers: Header[]
  data: unknown[][]

  // TODO: Remove all_rows_count – not used, but we need to fix the type logic
  all_rows_count: number
}
export interface Header {
  visualization_header: string
  get_child_node_action?: string
  child_label?: string
}

/** Is this a Table Viz for a generic grid? */
export function isGenericGrid(data: unknown): data is GenericGrid {
  return (
    typeof data === 'object' &&
    data !== null &&
    'type' in data &&
    (data as any).type === 'Generic_Grid'
  )
}
