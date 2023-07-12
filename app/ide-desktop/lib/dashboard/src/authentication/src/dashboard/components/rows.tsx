/** @file Table that projects an object into each column. */
import * as React from 'react'

import Spinner, * as spinner from './spinner'

// =================
// === Constants ===
// =================

/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE = 36

// =============
// === Types ===
// =============

/** Metadata describing how to render a column of the table. */
export interface Column<T> {
    id: string
    heading: JSX.Element
    widthClass?: string
    render: (item: T, index: number) => React.ReactNode
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
    onClick: (item: T, event: React.MouseEvent<HTMLTableRowElement>) => void
    onContextMenu: (item: T, event: React.MouseEvent<HTMLTableRowElement>) => void
}

/** Table that projects an object into each column. */
function Rows<T>(props: RowsProps<T>) {
    const { columns, items, isLoading, getKey, placeholder, onClick, onContextMenu } = props
    const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)

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

    React.useEffect(() => {
        if (isLoading) {
            // Ensure the spinner stays in the "initial" state for at least one frame.
            requestAnimationFrame(() => {
                setSpinnerState(spinner.SpinnerState.loadingFast)
            })
        } else {
            setSpinnerState(spinner.SpinnerState.initial)
        }
    }, [isLoading])

    const itemRows = isLoading ? (
        <tr className="flex h-8 spinner">
            <td colSpan={columns.length} className="block grow">
                <div className="grid justify-around w-full">
                    <Spinner size={LOADING_SPINNER_SIZE} state={spinnerState} />
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
        <table className="relative flex flex-col self-start gap-1 mx-0.75">
            <thead>
                <tr
                    className={`flex absolute top-2.75 pointer-events-none ${
                        items.length !== 0 ? 'h-[calc(100%-11px)]' : 'h-[calc(100%-2rem-11px)]'
                    }`}
                >
                    {columns.map(column => (
                        <td key={column.id} className={`border-r ${column.widthClass ?? ''}`} />
                    ))}
                </tr>
                {headerRow}
            </thead>
            <tbody className="flex flex-col gap-1">{itemRows}</tbody>
        </table>
    )
}

export default Rows
