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
    widthClass?: string
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
        <tr className="flex">
            {columns.map(column => (
                <th
                    key={column.id}
                    // className="flex h-8 transition duration-300 ease-in-out rounded-full odd:bg-gray-100 hover:bg-gray-200 focus:bg-gray-300"
                    className={`flex text-vs whitespace-nowrap font-bold text-left grow-0 shrink-0 px-4 py-1 ${
                        column.widthClass ?? ''
                    }`}
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
        <tr className="flex h-8 spinner">
            <td colSpan={columns.length} className="block grow">
                <div className="grid justify-around w-full">
                    <svg.Spinner size={LOADING_SPINNER_SIZE} className={spinnerClasses} />
                </div>
            </td>
        </tr>
    ) : items.length === 0 ? (
        <tr className="flex h-8 placeholder px-4">
            <td colSpan={columns.length} className="block grow">
                {placeholder}
            </td>
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
                className="flex h-8 transition duration-300 ease-in-out rounded-full even:bg-gray-100 hover:bg-gray-200 focus:bg-gray-250"
            >
                {columns.map(column => (
                    <td
                        key={column.id}
                        className={`flex items-center px-4 grow-0 shrink-0 ${
                            column.widthClass ?? ''
                        }`}
                    >
                        {column.render(item, index)}
                    </td>
                ))}
            </tr>
        ))
    )
    return (
        <table className="relative flex flex-col self-start rounded-rows gap-1 mx-0.75">
            <tr className="flex absolute top-2.75 h-[calc(100%-11px)] pointer-events-none">
                {columns.map(column => (
                    <td className={`border-r ${column.widthClass ?? ''}`} />
                ))}
            </tr>
            {headerRow}
            {itemRows}
        </table>
    )
}

export default Rows
