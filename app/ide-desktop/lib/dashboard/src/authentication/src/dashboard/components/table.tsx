/** @file Table that projects an object into each column. */
import * as React from 'react'

import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'
import * as svg from '../../components/svg'

// =================
// === Constants ===
// =================

/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE = 36
/** The classes for the initial state of the spinner. */
const SPINNER_INITIAL_CLASSES = 'grow dasharray-5 ease-linear'
/** The classes for the final state of the spinner. */
const SPINNER_LOADING_CLASSES = 'grow dasharray-75 duration-1000 ease-linear'

// =============
// === Types ===
// =============

/** Props for a {@link Column}. */
export interface ColumnProps<T, State = never, RowState = never> {
    item: T
    selected: boolean
    state: State
    rowState: RowState
    setRowState: (rowState: RowState) => void
}

/** Metadata describing how to render a column of the table. */
export interface Column<T, State = never, RowState = never> {
    id: string
    className?: string
    heading: JSX.Element
    render: (props: ColumnProps<T, State, RowState>) => JSX.Element
}

// =================
// === StateProp ===
// =================

/** `state: State`. */
interface StateProp<State> {
    state: State
}

// ===========================
// === InitialRowStateProp ===
// ===========================

/** `initialRowState: RowState`. */
interface InitialRowStateProp<RowState> {
    initialRowState: RowState
}

// ===========
// === Row ===
// ===========

/** Props for a {@link Row}. */
interface InternalBaseRowProps<T, State = never, RowState = never> {
    item: T
    state?: State
    initialRowState?: RowState
    columns: Column<T, State, RowState>[]
    selected: boolean
    allowContextMenu: boolean
    onClick: (item: T, event: React.MouseEvent) => void
    onContextMenu: (
        item: T,
        event: React.MouseEvent<HTMLTableRowElement>,
        rowState: RowState,
        setRowState: (newRowState: RowState) => void
    ) => void
}

/** Props for a {@link Row}. */
export type RowProps<T, State = never, RowState = never> = InternalBaseRowProps<
    T,
    State,
    RowState
> &
    ([RowState] extends [never] ? unknown : InitialRowStateProp<RowState>) &
    ([State] extends [never] ? unknown : StateProp<State>)

/** A row of a table. This is required because each row may store its own state. */
function Row<T, State = never, RowState = never>(props: RowProps<T, State, RowState>) {
    const {
        item,
        state,
        initialRowState,
        columns,
        selected,
        allowContextMenu,
        onClick,
        onContextMenu,
    } = props
    const { unsetModal } = modalProvider.useSetModal()

    /** This is SAFE, as the type is defined such that they MUST be
     * present if it is specified as a generic parameter.
     * See the type definitions of {@link RowProps} and {@link TableProps}.
     */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const [rowState, setRowState] = React.useState<RowState>(initialRowState!)

    return (
        <tr
            tabIndex={-1}
            onClick={event => {
                unsetModal()
                onClick(item, event)
            }}
            onContextMenu={event => {
                if (allowContextMenu) {
                    onContextMenu(item, event, rowState, setRowState)
                }
            }}
            className={`h-10 transition duration-300 ease-in-out hover:bg-gray-100 ${
                selected ? 'bg-gray-200' : ''
            }`}
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
                            selected={selected}
                            /** This is SAFE, as the type is defined such that they MUST be
                             * present if it is specified as a generic parameter.
                             * See the type definitions of {@link RowProps} and {@link TableProps}.
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

// =============
// === Table ===
// =============

/** Props for a {@link Table}. */
interface InternalTableProps<T, State = never, RowState = never> {
    items: T[]
    state?: State
    initialRowState?: RowState
    getKey: (item: T) => string
    columns: Column<T, State, RowState>[]
    isLoading: boolean
    placeholder: JSX.Element
    onContextMenu: (
        selectedItems: Set<T>,
        event: React.MouseEvent<HTMLTableElement>,
        setSelectedItems: (items: Set<T>) => void
    ) => void
    onRowContextMenu: (
        item: T,
        event: React.MouseEvent<HTMLTableRowElement>,
        rowState: RowState,
        setRowState: (newRowState: RowState) => void
    ) => void
}

/** Props for a {@link Table}. */
export type TableProps<T, State = never, RowState = never> = InternalTableProps<
    T,
    State,
    RowState
> &
    ([RowState] extends [never] ? unknown : InitialRowStateProp<RowState>) &
    ([State] extends [never] ? unknown : StateProp<State>)

/** Table that projects an object into each column. */
function Table<T, State = never, RowState = never>(props: TableProps<T, State, RowState>) {
    const { items, getKey, columns, isLoading, placeholder, onContextMenu, onRowContextMenu } =
        props

    const [spinnerClasses, setSpinnerClasses] = React.useState(SPINNER_INITIAL_CLASSES)
    // This should not be made mutable as an optimization, otherwise its value may change after
    // `await`ing an I/O operation.
    const [selectedItems, setSelectedItems] = React.useState(() => new Set<T>())
    const [previouslySelectedItem, setPreviouslySelectedItem] = React.useState<T | null>(null)

    React.useEffect(() => {
        const onDocumentClick = (event: MouseEvent) => {
            if (
                !shortcuts.SHORTCUT_REGISTRY.matchesMouseAction(
                    shortcuts.MouseAction.selectAdditional,
                    event
                ) &&
                !shortcuts.SHORTCUT_REGISTRY.matchesMouseAction(
                    shortcuts.MouseAction.selectAdditionalRange,
                    event
                ) &&
                selectedItems.size !== 0
            ) {
                setSelectedItems(new Set())
            }
        }
        document.addEventListener('click', onDocumentClick)
        return () => {
            document.removeEventListener('click', onDocumentClick)
        }
    }, [selectedItems])

    React.useEffect(() => {
        if (isLoading) {
            // Ensure the spinner stays in the "initial" state for at least one frame.
            requestAnimationFrame(() => {
                setSpinnerClasses(SPINNER_LOADING_CLASSES)
            })
        } else {
            setSpinnerClasses(SPINNER_INITIAL_CLASSES)
        }
    }, [isLoading])

    const onRowClick = React.useCallback(
        (item: T, event: React.MouseEvent) => {
            event.stopPropagation()
            const getNewlySelectedItems = () => {
                if (previouslySelectedItem == null) {
                    return [item]
                } else {
                    const index1 = items.indexOf(previouslySelectedItem)
                    const index2 = items.indexOf(item)
                    return index1 <= index2
                        ? items.slice(index1, index2 + 1)
                        : items.slice(index2, index1 + 1)
                }
            }
            if (
                shortcuts.SHORTCUT_REGISTRY.matchesMouseAction(
                    shortcuts.MouseAction.selectRange,
                    event
                )
            ) {
                setSelectedItems(new Set(getNewlySelectedItems()))
            } else if (
                shortcuts.SHORTCUT_REGISTRY.matchesMouseAction(
                    shortcuts.MouseAction.selectAdditionalRange,
                    event
                )
            ) {
                setSelectedItems(new Set([...selectedItems, ...getNewlySelectedItems()]))
            } else if (
                shortcuts.SHORTCUT_REGISTRY.matchesMouseAction(
                    shortcuts.MouseAction.selectAdditional,
                    event
                )
            ) {
                const newItems = new Set(selectedItems)
                if (selectedItems.has(item)) {
                    newItems.delete(item)
                } else {
                    newItems.add(item)
                }
                setSelectedItems(newItems)
            } else {
                setSelectedItems(new Set([item]))
            }
            setPreviouslySelectedItem(item)
        },
        [items, previouslySelectedItem, selectedItems]
    )

    const headerRow = (
        <tr>
            {columns.map(column => (
                <th
                    key={column.id}
                    className={`text-vs px-4 align-middle py-1 border-0 border-r whitespace-nowrap font-semibold text-left ${
                        column.className ?? ''
                    }`}
                >
                    {column.heading}
                </th>
            ))}
        </tr>
    )

    const itemRows = isLoading ? (
        <tr className="h-10">
            <td colSpan={columns.length}>
                <div className="grid justify-around w-full">
                    <svg.Spinner size={LOADING_SPINNER_SIZE} className={spinnerClasses} />
                </div>
            </td>
        </tr>
    ) : items.length === 0 ? (
        <tr className="h-10">
            <td colSpan={columns.length}>{placeholder}</td>
        </tr>
    ) : (
        items.map(item => (
            <Row<T, State, RowState>
                {...props}
                key={getKey(item)}
                item={item}
                selected={selectedItems.has(item)}
                allowContextMenu={selectedItems.size === 0}
                onClick={onRowClick}
                onContextMenu={onRowContextMenu}
            />
        ))
    )
    return (
        <table
            className="table-fixed items-center border-collapse w-0 mt-2"
            onContextMenu={event => {
                onContextMenu(selectedItems, event, () => {
                    setSelectedItems(new Set())
                })
            }}
        >
            <thead>{headerRow}</thead>
            <tbody>{itemRows}</tbody>
        </table>
    )
}

export default Table
