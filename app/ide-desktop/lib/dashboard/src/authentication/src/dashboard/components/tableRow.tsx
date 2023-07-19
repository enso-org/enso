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
interface InternalTableRowInnerProps<T, Key extends string = string> {
    key: Key
    item: T
    setItem: (newItem: T) => void
}

/** State and setters passed to event handlers on a {@link TableRow}. */
export type TableRowInnerProps<
    T,
    Key extends string = string,
    TableRowState = never
> = InternalTableRowInnerProps<T, Key> &
    ([TableRowState] extends never ? unknown : InternalTableRowStateProps<TableRowState>)

/** Props for a {@link TableRow}. */
interface InternalBaseTableRowProps<
    T,
    Key extends string = string,
    State = never,
    TableRowState = never
> extends Omit<JSX.IntrinsicElements['tr'], 'onClick' | 'onContextMenu'> {
    keyProp: Key
    item: T
    state?: State
    initialRowState?: TableRowState
    columns: tableColumn.TableColumn<T, State, TableRowState>[]
    selected: boolean
    allowContextMenu: boolean
    onClick: (props: TableRowInnerProps<T, Key, TableRowState>, event: React.MouseEvent) => void
    onContextMenu?: (
        props: TableRowInnerProps<T, Key, TableRowState>,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

/** Props for a {@link TableRow}. */
export type TableRowProps<
    T,
    Key extends string = string,
    State = never,
    TableRowState = never
> = InternalBaseTableRowProps<T, Key, State, TableRowState> &
    ([State] extends [never] ? unknown : StateProp<State>) &
    ([TableRowState] extends [never] ? unknown : InitialRowStateProp<TableRowState>)

/** A row of a table. This is required because each row may store its own state. */
function TableRow<T, Key extends string = string, State = never, RowState = never>(
    props: TableRowProps<T, Key, State, RowState>
) {
    const {
        keyProp: key,
        item: rawItem,
        state,
        initialRowState,
        columns,
        selected,
        allowContextMenu,
        onClick,
        onContextMenu,
        className,
        ...passthrough
    } = props
    const { unsetModal } = modalProvider.useSetModal()

    /** The internal state for this row. This may change as backend requests are sent. */
    const [item, setItem] = React.useState(rawItem)
    /** This is SAFE, as the type is defined such that they MUST be present when `RowState` is not
     * `never`.
     * See the type definitions of {@link TableRowProps} and `TableProps`. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const [rowState, setRowState] = React.useState<RowState>(initialRowState!)

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])

    const innerProps: TableRowInnerProps<T, Key, RowState> = {
        key,
        item,
        setItem,
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
                    onContextMenu?.(innerProps, event)
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
                        className={`px-4 border-0 border-r vertical-align-middle ${
                            column.className ?? ''
                        }`}
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
