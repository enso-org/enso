/** @file A select menu with a dropdown. */
import {
  useEffect,
  useMemo,
  useRef,
  useState,
  type HTMLInputTypeAttribute,
  type KeyboardEvent,
  type MutableRefObject,
} from 'react'

import CloseIcon from '#/assets/cross.svg'
import { Button, Input, Text } from '#/components/AriaComponents'
import FocusRing from '#/components/styled/FocusRing'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'

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
  readonly type?: HTMLInputTypeAttribute
  readonly inputRef?: MutableRefObject<HTMLFieldSetElement | null>
  readonly placeholder?: string
  readonly values: readonly T[]
  readonly autoFocus?: boolean
  /** This may change as the user types in the input. */
  readonly items: readonly T[]
  readonly itemToKey: (item: T) => string
  readonly children: (item: T) => string
  readonly itemsToString?: (items: T[]) => string
  readonly matches: (item: T, text: string) => boolean
  readonly text?: string | null
  readonly setText?: (text: string | null) => void
}

/** {@link AutocompleteProps} when `multiple` is `false`. */
interface InternalSingleAutocompleteProps<T> extends InternalBaseAutocompleteProps<T> {
  /** Whether selecting multiple values is allowed. */
  readonly multiple?: false
  readonly setValues: (value: readonly [] | readonly [T]) => void
  readonly itemsToString?: never
}

/** {@link AutocompleteProps} when `multiple` is `true`. */
interface InternalMultipleAutocompleteProps<T> extends InternalBaseAutocompleteProps<T> {
  /** Whether selecting multiple values is allowed. */
  readonly multiple: true
  /**
   * This is `null` when multiple values are selected, causing the input to switch to a
   * {@link HTMLTextAreaElement}.
   */
  readonly inputRef?: MutableRefObject<HTMLFieldSetElement | null>
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
  const { text, setText, autoFocus = false, items, itemToKey, children, itemsToString } = props
  const { matches } = props
  const [isDropdownVisible, setIsDropdownVisible] = useState(false)
  const [selectedIndex, setSelectedIndex] = useState<number | null>(null)
  const valuesSet = useMemo(() => new Set(values), [values])
  const canEditText = setText != null && values.length === 0
  // We are only interested in the initial value of `canEditText` in effects.
  const canEditTextRef = useRef(canEditText)
  const isMultipleAndCustomValue = multiple === true && text != null
  const matchingItems = useMemo(
    () => (text == null ? items : items.filter((item) => matches(item, text))),
    [items, matches, text],
  )

  useEffect(() => {
    if (!canEditTextRef.current) {
      setIsDropdownVisible(true)
    }
  }, [])

  const fallbackInputRef = useRef<HTMLFieldSetElement>(null)
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
      multiple === true && !isMultipleAndCustomValue ?
        valuesSet.has(value) ?
          values.filter((theItem) => theItem !== value)
        : [...values, value]
      : [value],
    )
  }

  const onKeyDown = (event: KeyboardEvent) => {
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
    <div className={twJoin('relative isolate h-6 w-full', isDropdownVisible && 'z-1')}>
      <div
        onKeyDown={onKeyDown}
        className={twMerge(
          'absolute w-full grow transition-colors',
          isDropdownVisible && matchingItems.length !== 0 ?
            'before:absolute before:inset-0 before:z-1 before:rounded-xl before:border-0.5 before:border-primary/20 before:bg-frame before:shadow-soft before:backdrop-blur-default'
          : '',
        )}
      >
        <FocusRing within>
          <div className="relative z-1 flex flex-1 items-center gap-2 rounded-full px-2">
            {canEditText ?
              <Input
                name="autocomplete"
                type={type}
                ref={inputRef}
                autoFocus={autoFocus}
                size="custom"
                variant="custom"
                value={text ?? ''}
                autoComplete="off"
                {...(placeholder == null ? {} : { placeholder })}
                onFocus={() => {
                  setIsDropdownVisible(true)
                }}
                onBlur={() => {
                  window.setTimeout(() => {
                    setIsDropdownVisible(false)
                  })
                }}
                onChange={(event) => {
                  setIsDropdownVisible(true)
                  setText(event.currentTarget.value === '' ? null : event.currentTarget.value)
                }}
              />
            : <Text
                tabIndex={-1}
                truncate="1"
                tooltipPlacement="left"
                onClick={() => {
                  setIsDropdownVisible(true)
                }}
                onBlur={() => {
                  window.setTimeout(() => {
                    setIsDropdownVisible(false)
                  })
                }}
              >
                {itemsToString?.(values) ?? (values[0] != null ? children(values[0]) : ZWSP)}
              </Text>
            }
            <Button
              size="medium"
              variant="icon"
              icon={CloseIcon}
              onPress={() => {
                setValues([])
                // setIsDropdownVisible(true)
                setText?.('')
              }}
            />
          </div>
        </FocusRing>
        <div
          className={twMerge(
            'relative z-1 grid h-max w-full rounded-b-xl transition-grid-template-rows duration-200',
            isDropdownVisible && matchingItems.length !== 0 ? 'grid-rows-1fr' : 'grid-rows-0fr',
          )}
        >
          <div className="relative max-h-60 w-full overflow-auto rounded-b-xl">
            {/* FIXME: "Invite" modal does not take into account the height of the autocomplete,
             * so the suggestions may go offscreen. */}
            {matchingItems.map((item, index) => (
              <div
                key={itemToKey(item)}
                className={twMerge(
                  'text relative cursor-pointer whitespace-nowrap px-input-x last:rounded-b-xl hover:bg-hover-bg',
                  valuesSet.has(item) && 'bg-hover-bg',
                  index === selectedIndex && 'bg-black/5',
                )}
                onMouseDown={(event) => {
                  event.preventDefault()
                }}
                onClick={(event) => {
                  event.stopPropagation()
                  toggleValue(item)
                }}
              >
                <Text truncate="1" className="w-full" tooltipPlacement="left">
                  {children(item)}
                </Text>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}
