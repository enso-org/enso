/** @file A select menu with a dropdown. */
import * as react from 'react'

import * as svg from '../../components/svg'

/** Props for a {@link Dropdown}. */
export interface DropdownProps {
    items: [string, ...string[]]
    onChange: (value: string) => void
    className?: string
    optionsClassName?: string
}

/** A select menu with a dropdown. */
function Dropdown(props: DropdownProps) {
    const { items, onChange, className, optionsClassName } = props
    const [value, setValue] = react.useState(items[0])
    // TODO:
    const [isDropdownVisible, setIsDropdownVisible] = react.useState(false)

    return (
        <div
            className={`whitespace-nowrap bg-gray-200 rounded-full cursor-pointer ${
                className ?? ''
            }`}
        >
            <div
                className="flex flex-1"
                onClick={() => {
                    setIsDropdownVisible(!isDropdownVisible)
                }}
            >
                <span className="grow">{value}</span> {svg.DOWN_CARET_ICON}
            </div>
            <div className={`relative h-0 ${optionsClassName ?? ''}`}>
                <div
                    className={`rounded-lg h-0 ${
                        isDropdownVisible ? 'overflow-visible' : 'overflow-hidden'
                    }`}
                >
                    {items.map(item => (
                        <div
                            onClick={() => {
                                setIsDropdownVisible(false)
                                setValue(item)
                                onChange(item)
                            }}
                            className="cursor-pointer bg-white first:rounded-t-lg last:rounded-b-lg hover:bg-gray-100 p-1"
                        >
                            {item}
                        </div>
                    ))}
                </div>
            </div>
        </div>
    )
}

export default Dropdown
