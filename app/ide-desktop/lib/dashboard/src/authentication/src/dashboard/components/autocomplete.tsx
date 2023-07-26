/** @file A select menu with a dropdown. */
import * as React from 'react'

// === Autocomplete ===
// ====================

/** Base props for a {@link Autocomplete}. */
export interface AutocompleteProps {
    type?: React.HTMLInputTypeAttribute
    inputRef?: React.MutableRefObject<HTMLInputElement | null>
    placeholder?: string
    initialValue?: string | null
    autoFocus?: boolean
    disabled?: boolean
    /** This may change as the user types in the input. */
    items: string[]
    className?: string
    inputClassName?: string
    optionsClassName?: string
    onChange: (value: string) => void
    /** This callback is only called when the text is changed. */
    onInput?: (value: string) => void
}

/** A select menu with a dropdown. */
export default function Autocomplete(props: AutocompleteProps) {
    const {
        type = 'text',
        inputRef: rawInputRef,
        placeholder,
        initialValue,
        autoFocus,
        disabled = false,
        items,
        onInput,
        onChange,
        className,
        inputClassName,
        optionsClassName,
    } = props
    const [value, setValue] = React.useState(initialValue ?? null)
    const [isDropdownVisible, setIsDropdownVisible] = React.useState(false)
    const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)

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

    React.useEffect(() => {
        if (inputRef.current != null) {
            inputRef.current.value = initialValue ?? ''
        }
        // `inputRef` is NOT a dependency as it is a React ref (a mutable value).
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [initialValue])

    /** Set values, while also changing the input text. */
    const overrideValues = React.useCallback(
        (newItem: string) => {
            setIsDropdownVisible(false)
            setValue(newItem)
            if (inputRef.current != null) {
                inputRef.current.value = newItem
            }
            onChange(newItem)
            onInput?.('')
        },
        [onChange, onInput, inputRef]
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
                        overrideValues(item)
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
            <div className={`flex flex-1 ${disabled ? 'cursor-not-allowed' : ''}`}>
                <input
                    type={type}
                    ref={inputRef}
                    autoFocus={autoFocus}
                    disabled={disabled}
                    size={1}
                    placeholder={placeholder}
                    // The types come from a third-party API and cannot be changed.
                    // eslint-disable-next-line no-restricted-syntax
                    defaultValue={value ?? undefined}
                    className={`grow ${disabled ? 'opacity-50' : ''} ${inputClassName ?? ''}`}
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
                        onChange(event.target.value)
                    }}
                />
            </div>
            <div className={`relative h-0 ${optionsClassName ?? ''}`}>
                <div
                    className={`absolute bg-frame-selected w-full rounded-2xl max-h-10lh ${
                        isDropdownVisible ? 'overflow-auto' : 'overflow-hidden h-0'
                    }`}
                >
                    {items.map((item, index) => (
                        <div
                            key={item}
                            className={`cursor-pointer first:rounded-t-2xl last:rounded-b-2xl hover:bg-black-a5 p-1 ${
                                index === selectedIndex ? 'bg-gray-100' : ''
                            }`}
                            onMouseDown={event => {
                                event.preventDefault()
                            }}
                            onClick={event => {
                                event.stopPropagation()
                                overrideValues(item)
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
