/** @file A table that projects an object into each column.
 * This is intended to be specialized into components for specific item types, rather than
 * being used directly. */
import * as React from 'react'

import * as set from '../../set'
import * as shortcutsModule from '../shortcuts'
import * as shortcutsProvider from '../../providers/shortcuts'

import * as tableColumn from './tableColumn'
import Spinner, * as spinner from './spinner'
import TableRow, * as tableRow from './tableRow'

// =================
// === Constants ===
// =================

/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE = 36

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

/** `selectedKeys` and `setSelectedKeys` when they are present. */
interface InternalSelectedKeysProps<Key> {
    selectedKeys: Set<Key>
    setSelectedKeys: React.Dispatch<React.SetStateAction<Set<Key>>>
}

/** The absence of `selectedKeys` and `setSelectedKeys`. */
interface InternalNoSelectedKeysProps {
    selectedKeys?: never
    setSelectedKeys?: never
}

// =============
// === Table ===
// =============

/** Props for a {@link Table}. */
interface InternalTableProps<T, State = never, RowState = never, Key extends string = string> {
    footer?: JSX.Element
    rowComponent?: (props: tableRow.TableRowProps<T, State, RowState, Key>) => JSX.Element
    items: T[]
    state?: State
    initialRowState?: RowState
    getKey: (item: T) => Key
    selectedKeys?: Set<Key>
    setSelectedKeys?: React.Dispatch<React.SetStateAction<Set<Key>>>
    columns: tableColumn.TableColumn<T, State, RowState, Key>[]
    isLoading: boolean
    placeholder?: JSX.Element
    className?: string
    onContextMenu: (
        selectedKeys: Set<Key>,
        event: React.MouseEvent<HTMLTableElement>,
        setSelectedKeys: (items: Set<Key>) => void
    ) => void
}

/** Props for a {@link Table}. */
export type TableProps<
    T,
    State = never,
    RowState = never,
    Key extends string = string
> = InternalTableProps<T, State, RowState, Key> &
    ([RowState] extends [never] ? unknown : InitialRowStateProp<RowState>) &
    ([State] extends [never] ? unknown : StateProp<State>) &
    (InternalNoSelectedKeysProps | InternalSelectedKeysProps<Key>)

/** Table that projects an object into each column. */
export default function Table<T, State = never, RowState = never, Key extends string = string>(
    props: TableProps<T, State, RowState, Key>
) {
    const {
        footer,
        rowComponent: RowComponent = TableRow,
        items,
        getKey,
        selectedKeys: rawSelectedKeys,
        setSelectedKeys: rawSetSelectedKeys,
        columns,
        isLoading,
        placeholder,
        onContextMenu,
        ...rowProps
    } = props
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
    // This should not be made mutable for the sake of optimization, otherwise its value may
    // be different after `await`ing an I/O operation. Also, a change in its value should trigger
    // a re-render.
    const [fallbackSelectedKeys, fallbackSetSelectedKeys] = React.useState(() => new Set<Key>())
    const [selectedKeys, setSelectedKeys] =
        rawSelectedKeys != null
            ? [rawSelectedKeys, rawSetSelectedKeys]
            : [fallbackSelectedKeys, fallbackSetSelectedKeys]
    const [previouslySelectedKey, setPreviouslySelectedKey] = React.useState<Key | null>(null)

    React.useEffect(() => {
        const onDocumentClick = (event: MouseEvent) => {
            if (
                !shortcuts.matchesMouseAction(
                    shortcutsModule.MouseAction.selectAdditional,
                    event
                ) &&
                !shortcuts.matchesMouseAction(
                    shortcutsModule.MouseAction.selectAdditionalRange,
                    event
                ) &&
                selectedKeys.size !== 0
            ) {
                setSelectedKeys(new Set())
            }
        }
        document.addEventListener('click', onDocumentClick)
        return () => {
            document.removeEventListener('click', onDocumentClick)
        }
    }, [selectedKeys, /* should never change */ setSelectedKeys, shortcuts])

    React.useEffect(() => {
        if (isLoading) {
            // Ensure the spinner stays in the "initial" state for at least one frame,
            // to ensure the CSS animation begins at the initial state.
            requestAnimationFrame(() => {
                setSpinnerState(spinner.SpinnerState.loadingFast)
            })
        } else {
            setSpinnerState(spinner.SpinnerState.initial)
        }
    }, [isLoading])

    const onRowClick = React.useCallback(
        (
            innerRowProps: tableRow.TableRowInnerProps<T, State, RowState, Key>,
            event: React.MouseEvent
        ) => {
            const { key } = innerRowProps
            event.stopPropagation()
            const getNewlySelectedKeys = () => {
                if (previouslySelectedKey == null) {
                    return [key]
                } else {
                    const index1 = items.findIndex(
                        innerItem => getKey(innerItem) === previouslySelectedKey
                    )
                    const index2 = items.findIndex(innerItem => getKey(innerItem) === key)
                    const selectedItems =
                        index1 <= index2
                            ? items.slice(index1, index2 + 1)
                            : items.slice(index2, index1 + 1)
                    return selectedItems.map(getKey)
                }
            }
            if (shortcuts.matchesMouseAction(shortcutsModule.MouseAction.selectRange, event)) {
                setSelectedKeys(new Set(getNewlySelectedKeys()))
            } else if (
                shortcuts.matchesMouseAction(
                    shortcutsModule.MouseAction.selectAdditionalRange,
                    event
                )
            ) {
                setSelectedKeys(
                    oldSelectedItems => new Set([...oldSelectedItems, ...getNewlySelectedKeys()])
                )
            } else if (
                shortcuts.matchesMouseAction(shortcutsModule.MouseAction.selectAdditional, event)
            ) {
                setSelectedKeys(oldSelectedItems => {
                    const newItems = new Set(oldSelectedItems)
                    if (oldSelectedItems.has(key)) {
                        newItems.delete(key)
                    } else {
                        newItems.add(key)
                    }
                    return newItems
                })
            } else {
                setSelectedKeys(new Set([key]))
            }
            setPreviouslySelectedKey(key)
        },
        [
            items,
            previouslySelectedKey,
            shortcuts,
            /* should never change */ setSelectedKeys,
            /* should never change */ getKey,
        ]
    )

    const headerRow = (
        <tr>
            {columns.map(column => {
                // This is a React component, even though it does not contain JSX.
                // eslint-disable-next-line no-restricted-syntax
                const Heading = column.heading
                return (
                    <th
                        key={column.id}
                        className={`text-sm font-semibold ${column.className ?? ''}`}
                    >
                        <Heading
                            // @ts-expect-error The following line is safe; the type error occurs
                            // because a property with a conditional type is being destructured.
                            state={props.state}
                        />
                    </th>
                )
            })}
        </tr>
    )

    const itemRows = isLoading ? (
        <tr className="h-10">
            <td colSpan={columns.length} className="bg-transparent">
                <div className="grid justify-around w-full">
                    <Spinner size={LOADING_SPINNER_SIZE} state={spinnerState} />
                </div>
            </td>
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
                    keyProp={key}
                    item={item}
                    selected={selectedKeys.has(key)}
                    setSelected={selected => {
                        setSelectedKeys(oldSelectedKeys =>
                            set.withPresence(oldSelectedKeys, key, selected)
                        )
                    }}
                    allowContextMenu={
                        selectedKeys.size === 0 ||
                        !selectedKeys.has(key) ||
                        (selectedKeys.size === 1 && selectedKeys.has(key))
                    }
                    onClick={onRowClick}
                    onContextMenu={(_innerProps, event) => {
                        if (!selectedKeys.has(key)) {
                            event.preventDefault()
                            event.stopPropagation()
                            setPreviouslySelectedKey(key)
                            setSelectedKeys(new Set([key]))
                        }
                    }}
                />
            )
        })
    )

    return (
        <table
            className="grow rounded-rows self-start table-fixed border-collapse mt-2"
            onContextMenu={event => {
                onContextMenu(selectedKeys, event, setSelectedKeys)
            }}
        >
            <thead>{headerRow}</thead>
            <tbody>
                {itemRows}
                {placeholder && (
                    <tr className="h-10 hidden first:table-row">
                        <td colSpan={columns.length} className="bg-transparent">
                            {placeholder}
                        </td>
                    </tr>
                )}
            </tbody>
            {footer}
        </table>
    )
}
