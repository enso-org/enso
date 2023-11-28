/** @file A row in a `Table`. */
import * as React from 'react'

import * as modalProvider from '../../providers/modal'

import type * as tableColumn from './tableColumn'

// =============================
// === Partial `Props` types ===
// =============================

/** `state: State`. */
interface StateProp<State> {
    state: State
}

/** `tablerowState` and `setTableRowState`. */
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
    setItem: React.Dispatch<React.SetStateAction<T>>
}

/** State and setters passed to event handlers on a {@link TableRow}. */
export type TableRowInnerProps<
    T,
    State = never,
    RowState = never,
    Key extends string = string,
> = InternalTableRowInnerProps<T, Key> &
    ([RowState] extends never ? unknown : InternalTableRowStateProps<RowState>) &
    ([State] extends never ? unknown : StateProp<State>)

/** Props for a {@link TableRow}. */
interface InternalBaseTableRowProps<T, State = never, RowState = never, Key extends string = string>
    extends Omit<JSX.IntrinsicElements['tr'], 'onClick' | 'onContextMenu'> {
    keyProp: Key
    tableRowRef?: React.RefObject<HTMLTableRowElement>
    item: T
    /** Pass this in only if `item` also needs to be updated in the parent component. */
    setItem?: React.Dispatch<React.SetStateAction<T>>
    state?: State
    hidden: boolean
    initialRowState?: RowState
    /** Pass this in only if `rowState` also needs to be updated in the parent component. */
    setRowState?: React.Dispatch<React.SetStateAction<RowState>>
    columns: tableColumn.TableColumn<T, State, RowState, Key>[]
    selected: boolean
    setSelected: (selected: boolean) => void
    isSoleSelectedItem: boolean
    allowContextMenu: boolean
    onClick: (props: TableRowInnerProps<T, State, RowState, Key>, event: React.MouseEvent) => void
    onContextMenu?: (
        props: TableRowInnerProps<T, State, RowState, Key>,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

/** Props for a {@link TableRow}. */
export type TableRowProps<
    T,
    State = never,
    TableRowState = never,
    Key extends string = string,
> = InternalBaseTableRowProps<T, State, TableRowState, Key> &
    ([State] extends [never] ? unknown : StateProp<State>) &
    ([TableRowState] extends [never] ? unknown : InitialRowStateProp<TableRowState>)

/** A row of a table. This is required because each row may store its own state. */
export default function TableRow<T, State = never, RowState = never, Key extends string = string>(
    props: TableRowProps<T, State, RowState, Key>
) {
    const {
        keyProp: key,
        tableRowRef,
        item: rawItem,
        setItem: rawSetItem,
        state,
        initialRowState,
        setRowState: rawSetRowState,
        columns,
        hidden,
        selected,
        setSelected,
        isSoleSelectedItem,
        // This prop is unused here, but is useful for components wrapping this component.
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        allowContextMenu,
        onClick,
        onContextMenu,
        className,
        ...passthrough
    } = props
    const { unsetModal } = modalProvider.useSetModal()

    const [fallbackItem, fallbackSetItem] = React.useState(rawItem)
    /** The item represented by this row. This may change as backend requests are sent. */
    const [item, setItem] =
        rawSetItem != null ? [rawItem, rawSetItem] : [fallbackItem, fallbackSetItem]
    /** This is SAFE, as the type is defined such that they MUST be present when `RowState` is not
     * `never`.
     * See the type definitions of {@link TableRowProps} and `TableProps`. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const [fallbackRowState, fallbackSetRowState] = React.useState<RowState>(initialRowState!)
    /** The internal state for this row. This may change as backend requests are sent. */
    const [rowState, setRowState] =
        initialRowState != null && rawSetRowState != null
            ? [initialRowState, rawSetRowState]
            : [fallbackRowState, fallbackSetRowState]

    React.useEffect(() => {
        if (rawSetItem == null) {
            setItem(rawItem)
        }
    }, [rawItem, /* should never change */ setItem, /* should never change */ rawSetItem])

    const innerProps: TableRowInnerProps<T, State, RowState, Key> = {
        key,
        item,
        setItem,
        // This is SAFE, as the type is defined such that they MUST be present when `State` is not
        //`never`.
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        state: state!,
        rowState,
        setRowState,
    }

    return hidden ? null : (
        <tr
            ref={tableRowRef}
            tabIndex={-1}
            onClick={event => {
                unsetModal()
                onClick(innerProps, event)
            }}
            onContextMenu={event => {
                onContextMenu?.(innerProps, event)
            }}
            className={`h-8 transition duration-300 ease-in-out ${className ?? ''} ${
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
                            isSoleSelectedItem={isSoleSelectedItem}
                            /** This is SAFE, as the type is defined such that they MUST be
                             * present if it is specified as a generic parameter.
                             * See the type definitions of {@link TableRowProps} and
                             * `TableProps`. */
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
