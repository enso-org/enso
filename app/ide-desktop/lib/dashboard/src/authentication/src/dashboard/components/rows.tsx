/** @file Table that projects an object into each column. */
import * as react from 'react'

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

/** Metadata describing how to render a column of the table. */
export interface Column<T> {
    id: string
    heading: JSX.Element
    render: (item: T, index: number) => react.ReactNode
}

// =================
// === Component ===
// =================

/** Props for a {@link Rows}. */
export interface RowsProps<T> {
    items: T[]
    getKey: (item: T) => string
    isLoading: boolean
    placeholder: JSX.Element
    columns: Column<T>[]
    onClick: (item: T, event: react.MouseEvent<HTMLTableRowElement>) => void
    onContextMenu: (item: T, event: react.MouseEvent<HTMLTableRowElement>) => void
}

/** Table that projects an object into each column. */
function Rows<T>(props: RowsProps<T>) {
    const { columns, items, isLoading, getKey, placeholder, onClick, onContextMenu } = props
    const [spinnerClasses, setSpinnerClasses] = react.useState(SPINNER_INITIAL_CLASSES)

    const headerRow = (
        <tr>
            {columns.map(column => (
                <th
                    key={column.id}
                    className="text-vs px-4 align-middle py-1 border-0 border-r whitespace-nowrap font-semibold text-left"
                >
                    {column.heading}
                </th>
            ))}
        </tr>
    )

    react.useEffect(() => {
        if (isLoading) {
            // Ensure the spinner stays in the "initial" state for at least one frame.
            requestAnimationFrame(() => {
                setSpinnerClasses(SPINNER_LOADING_CLASSES)
            })
        } else {
            setSpinnerClasses(SPINNER_INITIAL_CLASSES)
        }
    }, [isLoading])

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
        items.map((item, index) => (
            <tr
                key={getKey(item)}
                tabIndex={-1}
                onClick={event => {
                    onClick(item, event)
                }}
                onContextMenu={event => {
                    onContextMenu(item, event)
                }}
                className="h-10 transition duration-300 ease-in-out hover:bg-gray-100 focus:bg-gray-200"
            >
                {columns.map(column => (
                    <td key={column.id} className="px-4 border-0 border-r vertical-align-middle">
                        {column.render(item, index)}
                    </td>
                ))}
            </tr>
        ))
    )
    return (
        <>
            {headerRow}
            {itemRows}
        </>
    )
}

export default Rows
