/** @file A row in a `Table`. */
import * as React from 'react'

import * as modalProvider from '../../providers/modal'

import * as tableColumn from './tableColumn'

// =============================
// === Partial `Props` types ===
// =============================

/** `state: State`. */
interface StateProp<State> {
    state: State
}

/** `tablerowState` and `setTableRowState` */
interface InternalTableRowStateProps<TableRowState> {
    rowState: TableRowState
    setRowState: React.Dispatch<React.SetStateAction<TableRowState>>
}

/** `initialRowState: RowState`. */
interface InitialRowStateProp<RowState> {
    initialRowState: RowState
}

// ================
// === TableRow ===
// ================

/** Common properties for state and setters passed to event handlers on a {@link TableRow}. */
interface InternalTableRowInnerProps<T> {
    item: T
    setItem: (newItem: T) => void
    setNewKey: (newKey: string) => void
}

/** State and setters passed to event handlers on a {@link TableRow}. */
export type TableRowInnerProps<T, TableRowState = never> = InternalTableRowInnerProps<T> &
    ([TableRowState] extends never ? unknown : InternalTableRowStateProps<TableRowState>)

/** Props for a {@link TableRow}. */
interface InternalBaseTableRowProps<T, State = never, TableRowState = never>
    extends Omit<JSX.IntrinsicElements['tr'], 'onClick' | 'onContextMenu'> {
    item: T
    state?: State
    initialRowState?: TableRowState
    columns: tableColumn.TableColumn<T, State, TableRowState>[]
    selected: boolean
    allowContextMenu: boolean
    setNewKey: (newKey: string) => void
    onClick: (props: TableRowInnerProps<T, TableRowState>, event: React.MouseEvent) => void
    onContextMenu: (
        props: TableRowInnerProps<T, TableRowState>,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

/** Props for a {@link TableRow}. */
export type TableRowProps<T, State = never, TableRowState = never> = InternalBaseTableRowProps<
    T,
    State,
    TableRowState
> &
    ([State] extends [never] ? unknown : StateProp<State>) &
    ([TableRowState] extends [never] ? unknown : InitialRowStateProp<TableRowState>)

/** A row of a table. This is required because each row may store its own state. */
function TableRow<T, State = never, TableRowState = never>(
    props: TableRowProps<T, State, TableRowState>
) {
    const {
        item: rawItem,
        state,
        initialRowState,
        columns,
        selected,
        allowContextMenu,
        setNewKey,
        onClick,
        onContextMenu,
        className,
        ...passthrough
    } = props
    const { unsetModal } = modalProvider.useSetModal()

    /** The internal state for this row. This may change as backend requests are sent. */
    const [item, setItem] = React.useState(rawItem)
    /** This is SAFE, as the type is defined such that they MUST be
     * present if it is specified as a generic parameter.
     * See the type definitions of {@link TableRowProps} and `TableProps`. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const [rowState, setRowState] = React.useState<TableRowState>(initialRowState!)

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])

    const innerProps = {
        item,
        setItem,
        setNewKey,
        rowState,
        setRowState,
    }

    return (
        <tr
            tabIndex={-1}
            onClick={event => {
                unsetModal()
                onClick(innerProps, event)
            }}
            onContextMenu={event => {
                if (allowContextMenu) {
                    onContextMenu(innerProps, event)
                }
            }}
            className={`h-10 transition duration-300 ease-in-out hover:bg-gray-100 ${
                className ?? ''
            } ${selected ? 'bg-gray-200' : ''}`}
            {...passthrough}
        >
            {columns.map(column => {
                // This is a React component even though it does not contain JSX.
                // eslint-disable-next-line no-restricted-syntax
                const Render = column.render
                return (
                    <td
                        key={column.id}
                        className={`px-4 border-0 border-r ${column.className ?? ''}`}
                    >
                        <Render
                            item={item}
                            setItem={setItem}
                            selected={selected}
                            /** This is SAFE, as the type is defined such that they MUST be
                             * present if it is specified as a generic parameter.
                             * See the type definitions of {@link TableRowProps} and {@link TableProps}.
                             */
                            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                            state={state!}
                            rowState={rowState}
                            setRowState={setRowState}
                        />
                    </td>
                )
            })}
        </tr>
    )
}

export default TableRow
