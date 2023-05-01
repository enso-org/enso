/** @file Table that projects an object into each column. */
import * as react from 'react'

// =============
// === Types ===
// =============

/** Metadata describing how to render a column of the table. */
export interface Column<T> {
    id: string
    heading: JSX.Element
    render: (item: T, index: number) => JSX.Element
}

// =================
// === Component ===
// =================

interface Props<T> {
    items: T[]
    getKey: (item: T) => string
    placeholder: JSX.Element
    columns: Column<T>[]
    onClick: (item: T, event: react.MouseEvent<HTMLTableRowElement>) => void
    onContextMenu: (item: T, event: react.MouseEvent<HTMLTableRowElement>) => void
}

/** Table that projects an object into each column. */
function Rows<T>(props: Props<T>) {
    const { columns, items, getKey, placeholder, onClick, onContextMenu } = props
    const headerRow = columns.map(({ heading }, index) => (
        <th
            key={index}
            className="text-vs px-4 align-middle py-1 border-0 border-r whitespace-nowrap font-semibold text-left"
        >
            {heading}
        </th>
    ))
    const itemRows =
        items.length === 0 ? (
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
                    {columns.map(({ id, render }) => (
                        <td key={id} className="px-4 border-0 border-r">
                            {render(item, index)}
                        </td>
                    ))}
                </tr>
            ))
        )
    return (
        <>
            <tr>{headerRow}</tr>
            {itemRows}
        </>
    )
}

export default Rows
