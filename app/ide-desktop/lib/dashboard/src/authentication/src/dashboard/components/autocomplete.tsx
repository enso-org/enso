/** @file A select menu with a dropdown. */
import * as React from 'react'

// =================
// === Constants ===
// =================

/** A zero-width space. Useful to make a `div` take up at least one line. */
const ZWSP = '\u200b'

// ====================
// === Autocomplete ===
// ====================

/** Base props for a {@link Autocomplete}. */
export interface BaseAutocompleteProps {
    multiple?: boolean
    maxItemsToShow?: number
    type?: React.HTMLInputTypeAttribute
    itemNamePlural?: string
    inputRef?: React.MutableRefObject<HTMLInputElement | null>
    placeholder?: string
    values: string[]
    autoFocus?: boolean
    /** This may change as the user types in the input. */
    items: string[]
    className?: string
    inputClassName?: string
    optionsClassName?: string
    /** This callback is called with the raw text of the text input. */
    onInput?: (value: string) => void
}

/** {@link AutocompleteProps} when `multiple` is `false`. */
interface InternalSingleAutocompleteProps extends BaseAutocompleteProps {
    /** Whether selecting multiple values is allowed. */
    multiple?: false
    setValues: (value: [string]) => void
}

/** {@link AutocompleteProps} when `multiple` is `true`. */
interface InternalMultipleAutocompleteProps extends BaseAutocompleteProps {
    /** Whether selecting multiple values is allowed. */
    multiple: true
    /** This is `null` when multiple values are selected, causing the input to switch to a
     * {@link HTMLTextAreaElement}. */
    inputRef?: React.MutableRefObject<HTMLInputElement | null>
    setValues: (value: string[]) => void
}

/** Props for a {@link Autocomplete}. */
export type AutocompleteProps = InternalMultipleAutocompleteProps | InternalSingleAutocompleteProps

/** A select menu with a dropdown. */
export default function Autocomplete(props: AutocompleteProps) {
    const {
        multiple,
        maxItemsToShow = 1,
        itemNamePlural = 'items',
        type = 'text',
        inputRef: rawInputRef,
        placeholder,
        values,
        setValues,
        autoFocus,
        items,
        setValues: onChange,
        className,
        inputClassName,
        optionsClassName,
    } = props
    const [isDropdownVisible, setIsDropdownVisible] = React.useState(false)
    const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)
    const valuesText = React.useMemo(
        () =>
            values.length <= maxItemsToShow
                ? values.join(', ')
                : `${values.length} ${itemNamePlural} selected`,
        [values, maxItemsToShow, itemNamePlural]
    )
    const valuesSet = React.useMemo(() => new Set(values), [values])
    const canEditText = multiple !== true || values.length <= 1
    const isMultipleAndCustomValue = React.useMemo(
        () =>
            multiple === true &&
            values.length === 1 &&
            values[0] != null &&
            !items.includes(values[0]),
        [items, values, /* should never change */ multiple]
    )

    React.useEffect(() => {
        if (!canEditText) {
            setIsDropdownVisible(true)
        }
    }, [canEditText])

    React.useEffect(() => {
        const onClick = () => {
            setIsDropdownVisible(false)
        }
        document.addEventListener('click', onClick)
        return () => {
            document.removeEventListener('click', onClick)
        }
    }, [])

    const fallbackInputRef = React.useRef<HTMLInputElement>(null)
    const inputRef = rawInputRef ?? fallbackInputRef

    React.useEffect(() => {
        if (inputRef.current != null) {
            inputRef.current.value = values[0] ?? ''
        }
        // `inputRef` is NOT a dependency as it is a React ref (a mutable value).
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [values])

    // This type is a little too wide but it is unavoidable.
    /** Set values, while also changing the input text. */
    const overrideValues = (newItems: string[] | [string]) => {
        if (multiple !== true || (newItems.length === 1 && !items.includes(newItems[0]))) {
            setIsDropdownVisible(false)
        }
        if (inputRef.current != null) {
            inputRef.current.value = newItems[0]
        }
        if (multiple === true) {
            setValues(newItems)
            onChange(newItems)
        } else {
            setValues([newItems[0]])
            onChange([newItems[0]])
        }
    }

    const toggleItem = (item: string) => {
        overrideValues(
            multiple === true && !isMultipleAndCustomValue
                ? valuesSet.has(item)
                    ? values.filter(theItem => theItem !== item)
                    : [...values, item]
                : [item]
        )
    }

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
                    // If `item` is `null`, silently error. This is because it is out of range
                    // anyway, so no item will be selected in the UI.
                    if (item != null) {
                        toggleItem(item)
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
        <div onKeyDown={onKeyDown} className={className}>
            <div className="flex flex-1">
                {canEditText ? (
                    <input
                        type={type}
                        ref={inputRef}
                        autoFocus={autoFocus}
                        size={1}
                        defaultValue={values[0]}
                        placeholder={placeholder}
                        className={`grow ${inputClassName ?? ''}`}
                        onFocus={() => {
                            setIsDropdownVisible(true)
                        }}
                        onBlur={() => {
                            requestAnimationFrame(() => {
                                setIsDropdownVisible(false)
                            })
                        }}
                        onChange={event => {
                            setIsDropdownVisible(true)
                            onChange([event.target.value])
                        }}
                    />
                ) : (
                    <div
                        ref={element => element?.focus()}
                        tabIndex={-1}
                        className={`grow cursor-pointer ${inputClassName ?? ''}`}
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
                )}
            </div>
            <div className={`relative h-0 ${optionsClassName ?? ''}`}>
                <div
                    className={`absolute rounded-2xl w-full max-h-10lh ${
                        isDropdownVisible ? 'overflow-auto' : 'overflow-hidden h-0'
                    }`}
                >
                    <div className="absolute bg-frame-selected rounded-2xl backdrop-blur-3xl w-full h-full" />
                    {items.map((item, index) => (
                        <div
                            key={item}
                            className={`relative cursor-pointer first:rounded-t-2xl last:rounded-b-2xl hover:bg-black-a5 p-1 z-10 ${
                                index === selectedIndex
                                    ? 'bg-black-a5'
                                    : valuesSet.has(item)
                                    ? 'bg-black-a10'
                                    : ''
                            }`}
                            onMouseDown={event => {
                                event.preventDefault()
                            }}
                            onClick={event => {
                                event.stopPropagation()
                                toggleItem(item)
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
