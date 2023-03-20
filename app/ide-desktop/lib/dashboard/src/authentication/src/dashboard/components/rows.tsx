/** @file Table that projects an object into each column. */

// =============
// === Types ===
// =============

/** Metadata describing how to render a column of the table. */
export interface Column<T> {
    id: string
    name: string
    render: (item: T, index: number) => JSX.Element
}

// =================
// === Component ===
// =================

interface Props<T> {
    columns: Column<T>[]
    items: T[]
    getKey: (item: T) => string
    placeholder: JSX.Element
}

/** Table that projects an object into each column. */
function Rows<T>({ columns, items, getKey, placeholder }: Props<T>) {
    const headerRow = columns.map(({ name }, index) => (
        <th
            key={index}
            className="px-6 align-middle border border-solid py-3 border-l-0 border-r-1 border-t-0 border-b-0 whitespace-nowrap font-semibold text-left"
        >
            {name}
        </th>
    ))
    const itemRows =
        items.length === 0 ? (
            <tr>
                <td colSpan={columns.length}>{placeholder}</td>
            </tr>
        ) : (
            items.map((item, index) => (
                <tr
                    key={getKey(item)}
                    className="transition duration-300 ease-in-out hover:bg-gray-100"
                >
                    {columns.map(({ id, render }) => (
                        <td
                            key={id}
                            className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0"
                        >
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
