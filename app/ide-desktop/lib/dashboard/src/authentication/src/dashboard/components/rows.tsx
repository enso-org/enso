/** @file Table that projects an object into each column. */

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
    onContextMenu: (item: T) => void
}

/** Table that projects an object into each column. */
function Rows<T>(props: Props<T>) {
    const { columns, items, getKey, placeholder, onContextMenu } = props
    const headerRow = columns.map(({ heading }, index) => (
        <th
            key={index}
            className="px-6 align-middle border border-solid py-3 border-l-0 border-r-1 border-t-0 border-b-0 whitespace-nowrap font-semibold text-left"
        >
            {heading}
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
                    className="transition duration-300 ease-in-out hover:bg-gray-100 focus:bg-gray-200"
                    tabIndex={-1}
                    onContextMenu={() => {
                        onContextMenu(item)
                    }}
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
