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
    setItem: React.Dispatch<React.SetStateAction<T>>
    selected: boolean
    state: State
    rowState: RowState
    setRowState: React.Dispatch<React.SetStateAction<RowState>>
}

/** Metadata describing how to render a column of the table. */
export interface Column<T, State = never, RowState = never> {
    id: string
    className?: string
    heading: JSX.Element
    render: (props: ColumnProps<T, State, RowState>) => JSX.Element
}

// =============================
// === Partial `Props` types ===
// =============================

/** `state: State`. */
interface StateProp<State> {
    state: State
}

/** `rowState` and `setRowState` */
interface InternalRowStateProps<RowState> {
    rowState: RowState
    setRowState: React.Dispatch<React.SetStateAction<RowState>>
}

/** `initialRowState: RowState`. */
interface InitialRowStateProp<RowState> {
    initialRowState: RowState
}

// ===========
// === Row ===
// ===========

/** Common properties for state and setters passed to event handlers on a {@link Row}. */
interface InternalRowInnerProps<T> {
    item: T
    setItem: (newItem: T) => void
    setNewKey: (newKey: string) => void
}

/** State and setters passed to event handlers on a {@link Row}. */
export type RowInnerProps<T, RowState = never> = InternalRowInnerProps<T> &
    ([RowState] extends never ? unknown : InternalRowStateProps<RowState>)

/** Props for a {@link Row}. */
interface InternalBaseRowProps<T, State = never, RowState = never> {
    item: T
    state?: State
    initialRowState?: RowState
    columns: Column<T, State, RowState>[]
    selected: boolean
    allowContextMenu: boolean
    setNewKey: (newKey: string) => void
    onClick: (props: RowInnerProps<T, RowState>, event: React.MouseEvent) => void
    onContextMenu: (
        props: RowInnerProps<T, RowState>,
        event: React.MouseEvent<HTMLTableRowElement>
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
        item: rawItem,
        state,
        initialRowState,
        columns,
        selected,
        allowContextMenu,
        setNewKey,
        onClick,
        onContextMenu,
    } = props
    const { unsetModal } = modalProvider.useSetModal()

    /** The internal state for this row. This may change as backend requests are sent. */
    const [item, setItem] = React.useState(rawItem)
    /** This is SAFE, as the type is defined such that they MUST be
     * present if it is specified as a generic parameter.
     * See the type definitions of {@link RowProps} and {@link TableProps}. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const [rowState, setRowState] = React.useState<RowState>(initialRowState!)

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
                            setItem={setItem}
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
        props: RowInnerProps<T, RowState>,
        event: React.MouseEvent<HTMLTableRowElement>
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
    const {
        items,
        getKey: rawGetKey,
        columns,
        isLoading,
        placeholder,
        onContextMenu,
        onRowContextMenu,
    } = props

    const [spinnerClasses, setSpinnerClasses] = React.useState(SPINNER_INITIAL_CLASSES)
    // This should not be made mutable as an optimization, otherwise its value may change after
    // `await`ing an I/O operation.
    const [selectedItems, setSelectedItems] = React.useState(() => new Set<T>())
    const [previouslySelectedItem, setPreviouslySelectedItem] = React.useState<T | null>(null)
    /** A mapping from the initial optimistic key (if any) to the corresponing key obtained from
     * the backend. */
    const [keyRemapping, setKeyRemapping] = React.useState<Record<string, string>>({})
    // FIXME: set key for each row (pass `setKeyRemapping`)

    const getKey = React.useCallback(
        (item: T) => {
            const key = rawGetKey(item)
            return keyRemapping[key] ?? key
        },
        [keyRemapping, rawGetKey]
    )

    React.useEffect(() => {
        setKeyRemapping(oldKeyRemapping =>
            items.reduce<Record<string, string>>((newKeyRemapping, item) => {
                const actualKey = rawGetKey(item)
                const remappedKey = oldKeyRemapping[actualKey]
                if (remappedKey != null) {
                    newKeyRemapping[actualKey] = remappedKey
                }
                return newKeyRemapping
            }, {})
        )
        // `rawGetKey` is not a dependency of this effect.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [items])

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
        (rowProps: RowInnerProps<T, RowState>, event: React.MouseEvent) => {
            const { item } = rowProps
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
        items.map(item => {
            const key = getKey(item)
            return (
                <Row<T, State, RowState>
                    {...props}
                    key={key}
                    item={item}
                    selected={selectedItems.has(item)}
                    allowContextMenu={selectedItems.size === 0}
                    setNewKey={newKey => {
                        setKeyRemapping({ ...keyRemapping, [newKey]: key })
                    }}
                    onClick={onRowClick}
                    onContextMenu={onRowContextMenu}
                />
            )
        })
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
