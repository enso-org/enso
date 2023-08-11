/** @file Types for columns in a `Table`. */

// =============
// === Types ===
// =============

/** Props for a {@link Column}. */
export interface TableColumnProps<T, State = never, RowState = never, Key extends string = string> {
    keyProp: Key
    item: T
    setItem: React.Dispatch<React.SetStateAction<T>>
    selected: boolean
    setSelected: (selected: boolean) => void
    state: State
    rowState: RowState
    setRowState: React.Dispatch<React.SetStateAction<RowState>>
}

/** Props for a {@link Column}. */
export interface TableColumnHeadingProps<State = never> {
    state: State
}

/** Metadata describing how to render a column of the table. */
export interface TableColumn<T, State = never, RowState = never, Key extends string = string> {
    id: string
    className?: string
    heading: (props: TableColumnHeadingProps<State>) => JSX.Element
    render: (props: TableColumnProps<T, State, RowState, Key>) => JSX.Element
}
