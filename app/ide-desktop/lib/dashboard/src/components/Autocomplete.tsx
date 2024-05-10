/** @file A select menu with a dropdown. */
import * as React from 'react'

import FocusRing from '#/components/styled/FocusRing'
import Input from '#/components/styled/Input'

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
  readonly values: readonly T[]
  readonly autoFocus?: boolean
  /** This may change as the user types in the input. */
  readonly items: readonly T[]
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
  readonly setValues: (value: readonly T[]) => void
  readonly itemsToString: (items: readonly T[]) => string
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
  // We are only interested in the initial value of `canEditText` in effects.
  const canEditTextRef = React.useRef(canEditText)
  const isMultipleAndCustomValue = multiple === true && text != null
  const matchingItems = React.useMemo(
    () => (text == null ? items : items.filter(item => matches(item, text))),
    [items, matches, text]
  )

  React.useEffect(() => {
    if (!canEditTextRef.current) {
      setIsDropdownVisible(true)
    }
  }, [])

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
            toggleValue(item)
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
    <div onKeyDown={onKeyDown} className="grow">
      <FocusRing within>
        <div className="flex flex-1 rounded-full">
          {canEditText ? (
            <Input
              type={type}
              ref={inputRef}
              autoFocus={autoFocus}
              size={1}
              value={text ?? ''}
              placeholder={placeholder}
              className="text grow rounded-full bg-transparent px-button-x"
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
              className="text grow cursor-pointer whitespace-nowrap bg-transparent px-button-x"
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
      </FocusRing>
      <div className="h">
        <div
          className={`relative top-2 z-1 h-max w-full rounded-default shadow-soft before:absolute before:top before:h-full before:w-full before:rounded-default before:bg-frame before:backdrop-blur-default ${
            isDropdownVisible && matchingItems.length !== 0
              ? 'before:border before:border-primary/10'
              : ''
          }`}
        >
          <div
            className={`relative max-h-autocomplete-suggestions w-full overflow-auto rounded-default ${
              isDropdownVisible && matchingItems.length !== 0 ? '' : 'h'
            }`}
          >
            {/* FIXME: "Invite" modal does not take into account the height of the autocomplete,
             * so the suggestions may go offscreen. */}
            {matchingItems.map((item, index) => (
              <div
                key={itemToKey(item)}
                className={`text relative cursor-pointer whitespace-nowrap px-input-x first:rounded-t-default last:rounded-b-default hover:bg-hover-bg ${
                  index === selectedIndex ? 'bg-black/5' : valuesSet.has(item) ? 'bg-hover-bg' : ''
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
