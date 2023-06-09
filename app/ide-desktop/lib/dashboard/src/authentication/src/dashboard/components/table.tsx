/** @file Table that projects an object into each column. */
import * as React from 'react'

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
export interface ColumnProps<T, State = never> {
    item: T
    selected: boolean
    state: State
}

/** Metadata describing how to render a column of the table. */
export interface Column<T, State = never> {
    id: string
    className?: string
    heading: JSX.Element
    render: (props: ColumnProps<T, State>) => JSX.Element
}

// =================
// === Component ===
// =================

/** Props for a {@link Table}. */
export interface TableProps<T, State = never> {
    items: T[]
    state?: State
    getKey: (item: T) => string
    columns: Column<T, State>[]
    isLoading: boolean
    placeholder: JSX.Element
    onClick: (item: T, event: React.MouseEvent<HTMLTableRowElement>) => void
    onContextMenu: (item: T, event: React.MouseEvent<HTMLTableRowElement>) => void
}

/** Table that projects an object into each column. */
function Table<T, State = never>(props: TableProps<T, State>) {
    const { items, state, getKey, columns, isLoading, placeholder, onClick, onContextMenu } = props
    const [spinnerClasses, setSpinnerClasses] = React.useState(SPINNER_INITIAL_CLASSES)
    const [selectedItems, setSelectedItems] = React.useState(() => new Set<T>())
    const [previouslySelectedItem, setPreviouslySelectedItem] = React.useState<T | null>(null)

    React.useEffect(() => {
        const onDocumentClick = (event: MouseEvent) => {
            if (!event.ctrlKey) {
                setSelectedItems(new Set())
            }
        }
        document.addEventListener('click', onDocumentClick)
        return () => {
            document.removeEventListener('click', onDocumentClick)
        }
    }, [])

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

    const onItemClicked = React.useCallback(
        (item: T, event: React.MouseEvent) => {
            event.stopPropagation()
            // The Shift key should select a range of items, however the current architecture
            // is not designed to handle this.
            if (event.shiftKey && previouslySelectedItem != null) {
                const index1 = items.indexOf(previouslySelectedItem)
                const index2 = items.indexOf(item)
                const newlySelectedItems =
                    index1 <= index2
                        ? items.slice(index1, index2 + 1)
                        : items.slice(index2, index1 + 1)
                if (event.ctrlKey) {
                    setSelectedItems(new Set([...selectedItems, ...newlySelectedItems]))
                } else {
                    setSelectedItems(new Set(newlySelectedItems))
                }
            } else if (event.ctrlKey) {
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
            <tr
                key={getKey(item)}
                tabIndex={-1}
                onClick={event => {
                    onItemClicked(item, event)
                    onClick(item, event)
                }}
                onContextMenu={event => {
                    onContextMenu(item, event)
                }}
                className={`h-10 transition duration-300 ease-in-out hover:bg-gray-100 ${
                    selectedItems.has(item) ? 'bg-gray-200' : ''
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
                            {/* This is UNSAFE if `State` is explicitly specified by the caller,
                             * however it is unavoidable. */}
                            {/* eslint-disable-next-line @typescript-eslint/no-non-null-assertion */}
                            <Render item={item} selected={selectedItems.has(item)} state={state!} />
                        </td>
                    )
                })}
            </tr>
        ))
    )
    return (
        <table className="table-fixed items-center border-collapse w-0 mt-2">
            <thead>{headerRow}</thead>
            <tbody>{itemRows}</tbody>
        </table>
    )
}

export default Table
