/** @file Table that projects an object into each column. */
import * as React from 'react'

// =============
// === Types ===
// =============

export interface ColumnProps<T, State = never> {
    item: T
    state: State
}

/** Metadata describing how to render a column of the table. */
export interface Column<T, State = never> {
    id: string
    heading: JSX.Element
    render: (props: ColumnProps<T, State>) => JSX.Element
}

// =================
// === Component ===
// =================

interface Props<T, State = never> {
    items: T[]
    state?: State
    getKey: (item: T) => string
    columns: Column<T, State>[]
    placeholder: JSX.Element
    onClick: (item: T, event: React.MouseEvent<HTMLTableRowElement>) => void
    onContextMenu: (item: T, event: React.MouseEvent<HTMLTableRowElement>) => void
}

/** Table that projects an object into each column. */
function Rows<T, State = never>(props: Props<T, State>) {
    const { items, state, getKey, columns, placeholder, onClick, onContextMenu } = props
    const headerRow = columns.map((column, index) => (
        <th
            key={index}
            className="text-vs px-4 align-middle py-1 border-0 border-r whitespace-nowrap font-semibold text-left"
        >
            {column.heading}
        </th>
    ))
    const itemRows =
        items.length === 0 ? (
            <tr className="h-10">
                <td colSpan={columns.length}>{placeholder}</td>
            </tr>
        ) : (
            items.map(item => (
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
                    {columns.map(column => {
                        // This is a React component even though it does not contain JSX.
                        // eslint-disable-next-line no-restricted-syntax
                        const Render = column.render
                        return (
                            <td key={column.id} className="px-4 border-0 border-r">
                                {/* This is UNSAFE if `State` is explicitly specified by the caller,
                                 * however it is unavoidable. */}
                                {/* eslint-disable-next-line @typescript-eslint/no-non-null-assertion */}
                                <Render item={item} state={state!} />
                            </td>
                        )
                    })}
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
