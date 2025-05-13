// IMPORTANT - The types here must be kept in sync with those in Table_Viz_Data.enso
// AND the mocks in tableVisualisation.spec.ts

export interface Error {
    type: 'Error'
    error: string
    all_rows_count?: undefined
  }
  
  export function isError(data: unknown): data is Error {
    return (
      typeof data === 'object' &&
      data !== null &&
      'type' in data &&
      (data as any).type === 'Error'
    )
  }
  
  export interface SingleColumnOfActions {
    type: 'Single_Column_Of_Actions'
    column_count: number
    all_rows_count: number
    data: string[]
    json: unknown[][]
    get_child_node_action: string
    child_label: string
    visualization_header: string
  }
  
  export function isSingleColumnOfActions(data: unknown): data is SingleColumnOfActions {
    return (
      typeof data === 'object' &&
      data !== null &&
      'type' in data &&
      (data as any).type === 'Single_Column_Of_Actions' &&
      'data' in data &&
      Array.isArray((data as any).data)
    )
  }
  