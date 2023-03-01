/** @file Table for displaying details of projects, folders, files etc. */

interface Props<T> {
  columns: [
    name: string,
    createElement: (item: T, index: number) => JSX.Element
  ][];
  items: T[];
  getKey: (item: T) => string;
  placeholder: JSX.Element;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const Table = <T,>({ columns, items, getKey, placeholder }: Props<T>) => {
  const headerRow = columns.map(([name], index) => (
    <th
      key={index}
      className="px-6 align-middle border border-solid py-3 border-l-0 border-r-1 border-t-0 border-b-0 whitespace-nowrap font-semibold text-left"
    >
      {name}
    </th>
  ));
  const itemRows =
    items.length === 0
      ? <tr><td colSpan={columns.length}>{placeholder}</td></tr>
      : items.map((item) => (
          <tr
            key={getKey(item)}
            className="transition duration-300 ease-in-out hover:bg-gray-100"
          >
            {columns.map(([, createElement], index) => (
              <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
                {createElement(item, index)}
              </td>
            ))}
          </tr>
        ));
  return (
    <table className="items-center w-full bg-transparent border-collapse">
      <thead>
        <tr>{headerRow}</tr>
      </thead>
      <tbody>{itemRows}</tbody>
    </table>
  );
};

export default Table;
