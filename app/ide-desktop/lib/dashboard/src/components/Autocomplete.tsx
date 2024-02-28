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
interface InternalBaseAutocompleteProps<T> {
  readonly multiple?: boolean
  readonly type?: React.HTMLInputTypeAttribute
  readonly inputRef?: React.MutableRefObject<HTMLInputElement | null>
  readonly placeholder?: string
  readonly values: T[]
  readonly autoFocus?: boolean
  /** This may change as the user types in the input. */
  readonly items: T[]
  readonly itemToKey: (item: T) => string
  readonly itemToString: (item: T) => string
  readonly itemsToString?: (items: T[]) => string
  readonly matches: (item: T, text: string) => boolean
  readonly text?: string | null
  readonly setText?: (text: string | null) => void
}

/** {@link AutocompleteProps} when `multiple` is `false`. */
interface InternalSingleAutocompleteProps<T> extends InternalBaseAutocompleteProps<T> {
  /** Whether selecting multiple values is allowed. */
  readonly multiple?: false
  readonly setValues: (value: [T]) => void
  readonly itemsToString?: never
}

/** {@link AutocompleteProps} when `multiple` is `true`. */
interface InternalMultipleAutocompleteProps<T> extends InternalBaseAutocompleteProps<T> {
  /** Whether selecting multiple values is allowed. */
  readonly multiple: true
  /** This is `null` when multiple values are selected, causing the input to switch to a
   * {@link HTMLTextAreaElement}. */
  readonly inputRef?: React.MutableRefObject<HTMLInputElement | null>
  readonly setValues: (value: T[]) => void
  readonly itemsToString: (items: T[]) => string
}

/** {@link AutocompleteProps} when the text cannot be edited. */
interface WithoutText {
  readonly text?: never
  readonly setText?: never
}

/** {@link AutocompleteProps} when the text can be edited. */
interface WithText {
  readonly text: string | null
  readonly setText: (text: string | null) => void
}

/** Props for a {@link Autocomplete}. */
export type AutocompleteProps<T> = (
  | InternalMultipleAutocompleteProps<T>
  | InternalSingleAutocompleteProps<T>
) &
  (WithoutText | WithText)

/** A select menu with a dropdown. */
export default function Autocomplete<T>(props: AutocompleteProps<T>) {
  const { multiple, type = 'text', inputRef: rawInputRef, placeholder, values, setValues } = props
  const { text, setText, autoFocus, items, itemToKey, itemToString, itemsToString, matches } = props
  const [isDropdownVisible, setIsDropdownVisible] = React.useState(false)
  const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)
  const valuesSet = React.useMemo(() => new Set(values), [values])
  const canEditText = setText != null && values.length === 0
  const isMultipleAndCustomValue = multiple === true && text != null
  const matchingItems = React.useMemo(
    () => (text == null ? items : items.filter(item => matches(item, text))),
    [items, matches, text]
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

  // This type is a little too wide but it is unavoidable.
  /** Set values, while also changing the input text. */
  const overrideValues = (newItems: T[] | [T]) => {
    if (multiple !== true || (newItems.length === 1 && !items.includes(newItems[0]))) {
      setIsDropdownVisible(false)
    }
    if (multiple === true) {
      setValues(newItems)
    } else {
      setValues([newItems[0]])
    }
    setText?.(null)
  }

  const toggleValue = (value: T) => {
    overrideValues(
      multiple === true && !isMultipleAndCustomValue
        ? valuesSet.has(value)
          ? values.filter(theItem => theItem !== value)
          : [...values, value]
        : [value]
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
      case 'Enter': {
        // Do not prevent default; the input needs to handle the event too.
        if (selectedIndex != null) {
          const item = items[selectedIndex]
          // If `item` is `null`, silently error. This is because it is out of range
          // anyway, so no item will be selected in the UI.
          if (item != null) {
            toggleValue(item)
          }
          setSelectedIndex(null)
        }
        break
      }
      case 'Escape':
      case 'Tab': {
        // For 'Escape', do not prevent default; the input needs to handle the event too.
        // For 'Tab', ignore completely.
        break
      }
      default: {
        setIsDropdownVisible(true)
        break
      }
    }
  }

  return (
    <div onKeyDown={onKeyDown} className="grow">
      <div className="flex flex-1">
        {canEditText ? (
          <input
            type={type}
            ref={inputRef}
            autoFocus={autoFocus}
            size={1}
            value={text ?? ''}
            placeholder={placeholder}
            className="grow bg-transparent leading-170 h-6 py-px px-2"
            onFocus={() => {
              setIsDropdownVisible(true)
            }}
            onBlur={() => {
              window.setTimeout(() => {
                setIsDropdownVisible(false)
              })
            }}
            onChange={event => {
              setIsDropdownVisible(true)
              setText(event.currentTarget.value === '' ? null : event.currentTarget.value)
            }}
          />
        ) : (
          <div
            ref={element => element?.focus()}
            tabIndex={-1}
            className="grow cursor-pointer bg-transparent leading-170 h-6 py-px px-2"
            onClick={() => {
              setIsDropdownVisible(true)
            }}
            onBlur={() => {
              requestAnimationFrame(() => {
                setIsDropdownVisible(false)
              })
            }}
          >
            {itemsToString?.(values) ?? (values[0] != null ? itemToString(values[0]) : ZWSP)}
          </div>
        )}
      </div>
      <div className="h-0">
        <div
          className={`relative rounded-2xl shadow-soft w-full h-max top-2 z-1 before:absolute before:rounded-2xl before:backdrop-blur-3xl before:top-0 before:w-full before:h-full ${
            isDropdownVisible ? 'before:border before:border-black/10' : ''
          }`}
        >
          <div
            className={`relative rounded-2xl overflow-auto w-full max-h-10lh ${
              isDropdownVisible ? '' : 'h-0'
            }`}
          >
            {matchingItems.map((item, index) => (
              <div
                key={itemToKey(item)}
                className={`relative cursor-pointer first:rounded-t-2xl last:rounded-b-2xl hover:bg-black/5 py-1 px-2 ${
                  index === selectedIndex ? 'bg-black/5' : valuesSet.has(item) ? 'bg-black/10' : ''
                }`}
                onMouseDown={event => {
                  event.preventDefault()
                }}
                onClick={event => {
                  event.stopPropagation()
                  toggleValue(item)
                }}
              >
                {itemToString(item)}
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}
