/** @file A select menu with a dropdown. */
import * as React from 'react'

// =================
// === Constants ===
// =================

/** A zero-width space. Useful to make a `div` take up at least one line. */
const ZWSP = '\u200b'

// === Autocomplete ===
// ====================

/** Base props for a {@link Autocomplete}. */
export interface BaseAutocompleteProps {
    type?: React.HTMLInputTypeAttribute
    itemNamePlural?: string
    initialValue?: string | null
    autoFocus?: boolean
    disabled?: boolean
    maxItemsToShow?: number
    /** This may change as the user types in the input. */
    items: string[]
    className?: string
    optionsClassName?: string
    /** This callback is only called when the text is changed. */
    onInput?: (value: string) => void
}

/** {@link AutocompleteProps} when `multiple` is `false`. */
interface InternalSingleAutocompleteProps extends BaseAutocompleteProps {
    inputRef?: React.MutableRefObject<HTMLInputElement | null>
    /** Whether selecting multiple values is allowed. */
    multiple?: false
    onChange: (value: [string]) => void
}

/** {@link AutocompleteProps} when `multiple` is `true`. */
interface InternalMultipleAutocompleteProps extends BaseAutocompleteProps {
    /** This is `null` when multiple values are selected, causing the input to switch to a
     * {@link HTMLTextAreaElement}. */
    inputRef?: React.MutableRefObject<HTMLInputElement | null>
    /** Whether selecting multiple values is allowed. */
    multiple: true
    onChange: (value: string[]) => void
}

/** Props for a {@link Autocomplete}. */
export type AutocompleteProps = InternalMultipleAutocompleteProps | InternalSingleAutocompleteProps

/** A select menu with a dropdown. */
function Autocomplete(props: InternalMultipleAutocompleteProps | InternalSingleAutocompleteProps) {
    const {
        type = 'text',
        itemNamePlural = 'items',
        inputRef: rawInputRef,
        initialValue,
        autoFocus,
        disabled = false,
        multiple,
        maxItemsToShow = 1,
        items,
        onInput,
        onChange,
        className,
        optionsClassName,
    } = props
    const [values, setValues] = React.useState(initialValue != null ? [initialValue] : [])
    const [isDropdownVisible, setIsDropdownVisible] = React.useState(false)
    const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)
    const [valuesText, setValuesText] = React.useState('')

    const valuesSet = React.useMemo(() => new Set(values), [values])

    /** This input should only act like a multiple select only when `multiple` is true,
     * there are multiple items, and all selected items are in the autocomplete list. */
    const actuallyMultiple =
        multiple === true &&
        items.length > 1 &&
        (values.length > 1 || (values[0] != null && items.includes(values[0])))

    React.useEffect(() => {
        if (actuallyMultiple) {
            setIsDropdownVisible(true)
        }
    }, [actuallyMultiple])

    React.useEffect(() => {
        const onClick = () => {
            setIsDropdownVisible(false)
        }
        document.addEventListener('click', onClick)
        return () => {
            document.removeEventListener('click', onClick)
        }
    }, [])

    // This is required. React emits an error when conditionally setting `value`. See:
    // https://react.dev/reference/react-dom/components/input#im-getting-an-error-a-component-is-changing-an-uncontrolled-input-to-be-controlled
    // `rawInputRef` MUST either alwoys be set, or always not be set, otherwise this `useRef` hook
    // is called conditionally, which is not allowed in React.
    // This is INCORRECT, but SAFE to use in hooks as its value will be set by the time any hook
    // runs.
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const inputRef = rawInputRef ?? React.useRef<HTMLInputElement>(null)

    /** Set values, while also changing the input text if the input is not using multi-select. */
    const overrideValues = React.useCallback(
        // This type is a little too wide but it is unavoidable.
        (newItems: string[] | [string]) => {
            if (!actuallyMultiple) {
                setIsDropdownVisible(false)
            }
            setValues(newItems)
            const firstItem = newItems[0]
            if (inputRef.current != null) {
                inputRef.current.value = firstItem
            }
            setValuesText(
                newItems.length <= maxItemsToShow
                    ? newItems.join(', ')
                    : `${newItems.length} ${itemNamePlural} selected`
            )
            if (multiple === true) {
                onChange(newItems)
            } else {
                onChange([newItems[0]])
            }
            onInput?.('')
        },
        [
            onChange,
            onInput,
            inputRef,
            actuallyMultiple,
            /* should never change */ itemNamePlural,
            /* should never change */ multiple,
            /* should never change */ maxItemsToShow,
        ]
    )

    const onKeyDown = (event: React.KeyboardEvent) => {
        switch (event.key) {
            case 'ArrowUp': {
                event.preventDefault()
                if (selectedIndex == null || selectedIndex === 0 || selectedIndex >= items.length) {
                    setSelectedIndex(items.length - 1)
                } else {
                    setSelectedIndex(selectedIndex - 1)
                }
                break
            }
            case 'ArrowDown': {
                event.preventDefault()
                if (selectedIndex == null || selectedIndex >= items.length - 1) {
                    setSelectedIndex(0)
                } else {
                    setSelectedIndex(selectedIndex + 1)
                }
                break
            }
            case 'Escape': {
                // Do not prevent default; the input needs to handle the event too.
                break
            }
            case 'Enter': {
                // Do not prevent default; the input needs to handle the event too.
                if (selectedIndex != null) {
                    const item = items[selectedIndex]
                    // If `item` is `null`, silently error. If it *is* `null`, it is out of range
                    // anyway, so no item will be selected in the UI.
                    if (item != null) {
                        overrideValues(actuallyMultiple ? [...items, item] : [item])
                    }
                    setSelectedIndex(null)
                }
                break
            }
            case 'Tab': {
                // Ignore completely.
                break
            }
            default: {
                setIsDropdownVisible(true)
                break
            }
        }
    }

    return (
        <div onKeyDown={onKeyDown}>
            <div className={`flex flex-1 ${disabled ? 'cursor-not-allowed' : ''}`}>
                {actuallyMultiple ? (
                    <div
                        ref={element => element?.focus()}
                        tabIndex={-1}
                        className="grow cursor-pointer bg-gray-200 rounded-xl px-2 py-1"
                        onClick={() => {
                            setIsDropdownVisible(true)
                        }}
                        onBlur={() => {
                            requestAnimationFrame(() => {
                                setIsDropdownVisible(false)
                            })
                        }}
                    >
                        {valuesText.replace(/-/g, '\u2060-\u2060') || ZWSP}
                    </div>
                ) : (
                    <input
                        type={type}
                        ref={inputRef}
                        autoFocus={autoFocus}
                        disabled={disabled}
                        className={`grow bg-gray-200 rounded-xl px-2 py-1 ${
                            disabled ? 'pointer-events-none opacity-70' : ''
                        } ${className ?? ''}`}
                        defaultValue={values}
                        onFocus={() => {
                            setIsDropdownVisible(true)
                        }}
                        onBlur={() => {
                            requestAnimationFrame(() => {
                                setIsDropdownVisible(false)
                            })
                        }}
                        onInput={event => {
                            setIsDropdownVisible(true)
                            onInput?.(event.currentTarget.value)
                        }}
                        onChange={event => {
                            onChange([event.target.value])
                        }}
                    />
                )}
            </div>
            <div className={`relative h-0 ${optionsClassName ?? ''}`}>
                <div
                    className={`absolute bg-white z-10 w-full rounded-lg shadow-soft max-h-10lh ${
                        isDropdownVisible ? 'overflow-auto' : 'overflow-hidden h-0'
                    }`}
                >
                    {items.map((item, index) => (
                        <div
                            key={item}
                            className={`cursor-pointer first:rounded-t-lg last:rounded-b-lg hover:bg-gray-100 p-1 ${
                                index === selectedIndex
                                    ? 'bg-gray-100'
                                    : valuesSet.has(item)
                                    ? 'bg-gray-200'
                                    : 'bg-white'
                            }`}
                            onMouseDown={event => {
                                event.preventDefault()
                            }}
                            onClick={event => {
                                event.stopPropagation()
                                overrideValues(
                                    actuallyMultiple
                                        ? valuesSet.has(item)
                                            ? values.filter(theItem => theItem !== item)
                                            : [...values, item]
                                        : [item]
                                )
                            }}
                        >
                            {item}
                        </div>
                    ))}
                </div>
            </div>
        </div>
    )
}

export default Autocomplete
