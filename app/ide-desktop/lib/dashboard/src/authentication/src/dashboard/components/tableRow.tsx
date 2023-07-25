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
    TableRowState = never,
    Key extends string = string
> = InternalTableRowInnerProps<T, Key> &
    ([TableRowState] extends never ? unknown : InternalTableRowStateProps<TableRowState>)

/** Props for a {@link TableRow}. */
interface InternalBaseTableRowProps<
    T,
    State = never,
    TableRowState = never,
    Key extends string = string
> extends Omit<JSX.IntrinsicElements['tr'], 'onClick' | 'onContextMenu'> {
    keyProp: Key
    item: T
    setItem?: React.Dispatch<React.SetStateAction<T>>
    state?: State
    initialRowState?: TableRowState
    columns: tableColumn.TableColumn<T, State, TableRowState, Key>[]
    selected: boolean
    setSelected: (selected: boolean) => void
    allowContextMenu: boolean
    onClick: (props: TableRowInnerProps<T, TableRowState, Key>, event: React.MouseEvent) => void
    onContextMenu?: (
        props: TableRowInnerProps<T, TableRowState, Key>,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

/** Props for a {@link TableRow}. */
export type TableRowProps<
    T,
    State = never,
    TableRowState = never,
    Key extends string = string
> = InternalBaseTableRowProps<T, State, TableRowState, Key> &
    ([State] extends [never] ? unknown : StateProp<State>) &
    ([TableRowState] extends [never] ? unknown : InitialRowStateProp<TableRowState>)

/** A row of a table. This is required because each row may store its own state. */
export default function TableRow<T, State = never, RowState = never, Key extends string = string>(
    props: TableRowProps<T, State, RowState, Key>
) {
    const {
        keyProp: key,
        item: rawItem,
        setItem: rawSetItem,
        state,
        initialRowState,
        columns,
        selected,
        setSelected,
        allowContextMenu,
        onClick,
        onContextMenu,
        className,
        ...passthrough
    } = props
    const { unsetModal } = modalProvider.useSetModal()

    /** The internal state for this row. This may change as backend requests are sent. */
    // This hook is not called conditionally. `setItem` either always exists, or never exists.
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const [item, setItem] = rawSetItem != null ? [rawItem, rawSetItem] : React.useState(rawItem)
    /** This is SAFE, as the type is defined such that they MUST be present when `RowState` is not
     * `never`.
     * See the type definitions of {@link TableRowProps} and `TableProps`. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const [rowState, setRowState] = React.useState<RowState>(initialRowState!)

    React.useEffect(() => {
        if (rawSetItem != null) {
            setItem(rawItem)
        }
    }, [rawItem, /* should never change */ setItem, /* should never change */ rawSetItem])

    const innerProps: TableRowInnerProps<T, RowState, Key> = {
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
            className={`h-10 transition duration-300 ease-in-out ${className ?? ''} ${
                selected ? 'selected' : ''
            }`}
            {...passthrough}
        >
            {columns.map(column => {
                // This is a React component even though it does not contain JSX.
                // eslint-disable-next-line no-restricted-syntax
                const Render = column.render
                return (
                    <td key={column.id} className={column.className ?? ''}>
                        <Render
                            keyProp={key}
                            item={item}
                            setItem={setItem}
                            selected={selected}
                            setSelected={setSelected}
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
