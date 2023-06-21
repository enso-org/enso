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
                if (selectedIndex == null) {
                    setSelectedIndex(items.length - 1)
                } else if (selectedIndex === 0) {
                    setSelectedIndex(null)
                } else {
                    setSelectedIndex(selectedIndex - 1)
                }
                break
            }
            case 'ArrowDown': {
                event.preventDefault()
                console.log('ad')
                if (selectedIndex == null) {
                    setSelectedIndex(0)
                } else if (selectedIndex === items.length - 1) {
                    setSelectedIndex(null)
                } else {
                    setSelectedIndex(selectedIndex + 1)
                }
                break
            }
        }
    }

    return (
        <div
            className="whitespace-nowrap bg-gray-200 rounded-full cursor-pointer"
            onClick={() => {
                setIsDropdownVisible(false)
            }}
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
                    onClick={event => {
                        event.stopPropagation()
                    }}
                    onInput={event => {
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
                            className={`cursor-pointer bg-white first:rounded-t-lg last:rounded-b-lg hover:bg-gray-100 p-1 ${
                                index === selectedIndex ? 'bg-gray-100' : ''
                            }`}
                            onClick={event => {
                                event.stopPropagation()
                                setIsDropdownVisible(false)
                                setValue(item)
                                inputRef.current.value = item
                                onChange(item)
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
