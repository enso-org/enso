/** @file A table that projects an object into each column. */
import * as React from 'react'

import * as shortcuts from '../shortcuts'
import * as svg from '../../components/svg'

import * as tableColumn from './tableColumn'
import TableRow, * as tableRow from './tableRow'

// =================
// === Constants ===
// =================

/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE = 36
/** The classes for the initial state of the spinner. */
const SPINNER_INITIAL_CLASSES = 'grow dasharray-5 ease-linear'
/** The classes for the final state of the spinner. */
const SPINNER_LOADING_CLASSES = 'grow dasharray-75 duration-1000 ease-linear'

// =============================
// === Partial `Props` types ===
// =============================

/** `state: State`. */
interface StateProp<State> {
    state: State
}

/** `initialRowState: RowState`. */
interface InitialRowStateProp<RowState> {
    initialRowState: RowState
}

// =============
// === Table ===
// =============

/** Props for a {@link Table}. */
interface InternalTableProps<T, State = never, RowState = never> {
    rowComponent?: (props: tableRow.TableRowProps<T, State, RowState>) => JSX.Element
    items: T[]
    state?: State
    initialRowState?: RowState
    getKey: (item: T) => string
    columns: tableColumn.TableColumn<T, State, RowState>[]
    isLoading: boolean
    placeholder: JSX.Element
    onContextMenu: (
        selectedItems: Set<T>,
        event: React.MouseEvent<HTMLTableElement>,
        setSelectedItems: (items: Set<T>) => void
    ) => void
    onRowContextMenu: (
        props: tableRow.TableRowInnerProps<T, RowState>,
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
        rowComponent: RowComponent = TableRow,
        items,
        getKey: rawGetKey,
        columns,
        isLoading,
        placeholder,
        onContextMenu,
        onRowContextMenu,
        ...rowProps
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
        (innerRowProps: tableRow.TableRowInnerProps<T, RowState>, event: React.MouseEvent) => {
            const { item } = innerRowProps
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
                <RowComponent
                    {...rowProps}
                    columns={columns}
                    // The following two lines are safe; the type error occurs because a property
                    // with a conditional type is being destructured.
                    // eslint-disable-next-line no-restricted-syntax
                    state={rowProps.state as never}
                    // eslint-disable-next-line no-restricted-syntax
                    initialRowState={rowProps.initialRowState as never}
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
