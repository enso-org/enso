/** @file A select menu with a dropdown. */
import * as react from 'react'

/** Props for a {@link Autocomplete}. */
export interface AutocompleteProps {
    initialValue: string
    autoFocus?: boolean
    items: string[]
    onInput?: (value: string) => void
    onChange: (value: string) => void
    className?: string
    optionsClassName?: string
}

/** A select menu with a dropdown. */
function Autocomplete(props: AutocompleteProps) {
    const {
        initialValue,
        autoFocus = false,
        items,
        onInput,
        onChange,
        className,
        optionsClassName,
    } = props
    const [value, setValue] = react.useState(initialValue)
    const [isDropdownVisible, setIsDropdownVisible] = react.useState(false)
    const [selectedIndex, setSelectedIndex] = react.useState<number | null>(null)

    const overrideValue = react.useCallback((item: string) => {
        setIsDropdownVisible(false)
        setValue(item)
        inputRef.current.value = item
        onChange(item)
    }, [])

    // This is required, rather than conditionally setting the `value` prop on the input,
    // because React disallows that for some reason. See:
    // https://react.dev/reference/react-dom/components/input#im-getting-an-error-a-component-is-changing-an-uncontrolled-input-to-be-controlled
    // This is INCORRECT, but SAFE to use in hooks as its value will be set by the time any hook
    // runs.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const inputRef = react.useRef<HTMLInputElement>(null!)

    const onKeyDown = (event: react.KeyboardEvent) => {
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
                        overrideValue(item)
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
        <div
            className="whitespace-nowrap bg-gray-200 rounded-full cursor-pointer"
            onKeyDown={onKeyDown}
        >
            <div className="flex flex-1">
                <input
                    ref={inputRef}
                    autoFocus={autoFocus}
                    className={`grow bg-transparent mx-1 ${className ?? ''}`}
                    defaultValue={value}
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
                    className={`absolute w-full rounded-lg shadow-soft ${
                        isDropdownVisible ? 'overflow-visible' : 'overflow-hidden h-0'
                    }`}
                >
                    {items.map((item, index) => (
                        <div
                            key={item}
                            className={`cursor-pointer first:rounded-t-lg last:rounded-b-lg hover:bg-gray-100 p-1 ${
                                index === selectedIndex ? 'bg-gray-100' : 'bg-white'
                            }`}
                            onMouseDown={event => {
                                event.preventDefault()
                            }}
                            onClick={event => {
                                event.stopPropagation()
                                overrideValue(item)
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
