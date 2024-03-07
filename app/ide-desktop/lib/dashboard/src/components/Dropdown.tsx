/** @file A styled dropdown. */
import * as React from 'react'

import CheckMarkIcon from 'enso-assets/check_mark.svg'
import TriangleDownIcon from 'enso-assets/triangle_down.svg'

import SvgMask from '#/components/SvgMask'

// ================
// === Dropdown ===
// ================

/** Props for a list item child. */
interface InternalChildProps<T> {
  readonly item: T
}

/** Props for the display of the currently selected item, when the dropdown supports multiple children. */
interface InternalChildrenProps<T> {
  readonly items: T[]
  /** This is the value passed as {@link DropdownProps.render}. */
  readonly render: (props: InternalChildProps<T>) => React.ReactNode
}

/** Props for a {@link Dropdown} shared between all variants. */
interface InternalBaseDropdownProps<T> {
  readonly readOnly?: boolean
  readonly className?: string
  readonly items: readonly T[]
  readonly render: (props: InternalChildProps<T>) => React.ReactNode
}

/** Props for a {@link Dropdown}, when `multiple` is `false` or absent. */
interface InternalSingleDropdownProps<T> extends InternalBaseDropdownProps<T> {
  readonly multiple?: false
  readonly selectedIndex: number | null
  readonly onClick: (item: T, index: number) => void
}

/** Props for a {@link Dropdown}, when `multiple` is `true`. */
interface InternalMultipleDropdownProps<T> extends InternalBaseDropdownProps<T> {
  readonly multiple: true
  readonly selectedIndices: readonly number[]
  readonly renderMultiple: (props: InternalChildrenProps<T>) => React.ReactNode
  readonly onClick: (items: readonly T[], indices: readonly number[]) => void
}

/** Props for a {@link Dropdown}. */
export type DropdownProps<T> = InternalMultipleDropdownProps<T> | InternalSingleDropdownProps<T>

/** A styled dropdown. */
export default function Dropdown<T>(props: DropdownProps<T>) {
  const { readOnly = false, className, items, render: Child } = props
  const [isDropdownVisible, setIsDropdownVisible] = React.useState(false)
  const [tempSelectedIndex, setTempSelectedIndex] = React.useState<number | null>(null)
  const rootRef = React.useRef<HTMLDivElement>(null)
  const justFocusedRef = React.useRef(false)
  const isMouseDown = React.useRef(false)
  const multiple = props.multiple === true
  const selectedIndex = 'selectedIndex' in props ? props.selectedIndex : null
  const selectedIndices = 'selectedIndices' in props ? props.selectedIndices : []
  const selectedItems = selectedIndices.flatMap(index => {
    const item = items[index]
    return item != null ? [item] : []
  })
  const visuallySelectedIndex = tempSelectedIndex ?? selectedIndex
  const visuallySelectedItem = visuallySelectedIndex == null ? null : items[visuallySelectedIndex]

  React.useEffect(() => {
    setTempSelectedIndex(selectedIndex)
  }, [selectedIndex])

  React.useEffect(() => {
    if (!isDropdownVisible) {
      rootRef.current?.blur()
    }
  }, [isDropdownVisible])

  React.useEffect(() => {
    const onDocumentClick = () => {
      setIsDropdownVisible(false)
    }
    document.addEventListener('click', onDocumentClick)
    return () => {
      document.removeEventListener('click', onDocumentClick)
    }
  }, [])

  const onKeyDown = (event: React.KeyboardEvent) => {
    if (!event.ctrlKey && !event.shiftKey && !event.altKey && !event.metaKey) {
      switch (event.key) {
        case 'Escape': {
          event.stopPropagation()
          setIsDropdownVisible(false)
          break
        }
        case 'Enter':
        case 'Tab': {
          event.stopPropagation()
          if (tempSelectedIndex != null) {
            const item = items[tempSelectedIndex]
            if (item != null) {
              if (multiple) {
                const newIndices = selectedIndices.includes(tempSelectedIndex)
                  ? selectedIndices.filter(index => index !== tempSelectedIndex)
                  : [...selectedIndices, tempSelectedIndex]
                props.onClick(
                  newIndices.flatMap(index => {
                    const otherItem = items[index]
                    return otherItem != null ? [otherItem] : []
                  }),
                  newIndices
                )
              } else {
                props.onClick(item, tempSelectedIndex)
              }
            }
          }
          if (event.key !== 'Enter' || !multiple) {
            setIsDropdownVisible(false)
          }
          break
        }
        case 'ArrowUp': {
          event.preventDefault()
          setTempSelectedIndex(
            tempSelectedIndex == null ||
              tempSelectedIndex === 0 ||
              tempSelectedIndex >= items.length
              ? items.length - 1
              : tempSelectedIndex - 1
          )
          break
        }
        case 'ArrowDown': {
          event.preventDefault()
          setTempSelectedIndex(
            tempSelectedIndex == null || tempSelectedIndex >= items.length - 1
              ? 0
              : tempSelectedIndex + 1
          )
          break
        }
      }
    }
  }

  return (
    <div
      ref={rootRef}
      tabIndex={0}
      className={`group relative flex w-max cursor-pointer flex-col items-start whitespace-nowrap rounded-xl leading-5 ${
        className ?? ''
      }`}
      onFocus={event => {
        if (!readOnly && event.target === event.currentTarget) {
          setIsDropdownVisible(true)
          justFocusedRef.current = true
        }
      }}
      onBlur={event => {
        // TODO: should not blur when `multiple` and clicking on option
        if (!readOnly && event.target === event.currentTarget) {
          setIsDropdownVisible(false)
        }
      }}
      onKeyDown={onKeyDown}
      onKeyUp={() => {
        justFocusedRef.current = false
      }}
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <div
        className={`absolute left-0 h-full w-full min-w-max ${isDropdownVisible ? 'z-1' : 'overflow-hidden'}`}
      >
        <div
          className={`relative before:absolute before:top-0 before:w-full before:rounded-xl before:border before:border-black/10 before:backdrop-blur-3xl before:transition-all ${
            isDropdownVisible
              ? 'before:h-full before:shadow-soft'
              : 'before:h-6 group-hover:before:bg-frame'
          }`}
        >
          {/* Spacing. */}
          <div
            className="padding relative h-6"
            onClick={event => {
              event.stopPropagation()
              if (!justFocusedRef.current && !readOnly) {
                setIsDropdownVisible(false)
              }
              justFocusedRef.current = false
            }}
          />
          <div
            className={`max-h-10lh relative grid w-full rounded-xl transition-grid-template-rows ${
              isDropdownVisible ? 'grid-rows-1fr' : 'grid-rows-0fr'
            }`}
          >
            <div className="overflow-hidden">
              {items.map((item, i) => (
                <div
                  tabIndex={-1}
                  className={`flex h-6 items-center gap-1 rounded-xl px-2 transition-colors ${multiple ? 'hover:font-semibold' : ''} ${
                    i === visuallySelectedIndex
                      ? `cursor-default bg-frame font-bold`
                      : 'hover:bg-frame-selected'
                  }`}
                  key={i}
                  onMouseDown={event => {
                    event.preventDefault()
                    isMouseDown.current = true
                  }}
                  onMouseUp={() => {
                    isMouseDown.current = false
                  }}
                  onClick={() => {
                    if (i !== visuallySelectedIndex) {
                      if (multiple) {
                        const newIndices = selectedIndices.includes(i)
                          ? selectedIndices.filter(index => index !== i)
                          : [...selectedIndices, i]
                        props.onClick(
                          newIndices.flatMap(index => {
                            const otherItem = items[index]
                            return otherItem != null ? [otherItem] : []
                          }),
                          newIndices
                        )
                        rootRef.current?.focus()
                      } else {
                        setIsDropdownVisible(false)
                        props.onClick(item, i)
                      }
                    }
                  }}
                  onFocus={() => {
                    if (!isMouseDown.current) {
                      // This is from keyboard navigation.
                      if (multiple) {
                        // FIXME: Is this correct behavior?
                        props.onClick([item], [i])
                      } else {
                        props.onClick(item, i)
                      }
                    }
                  }}
                >
                  {selectedIndices.includes(i) ? (
                    <SvgMask src={CheckMarkIcon} />
                  ) : (
                    <SvgMask src={TriangleDownIcon} className="invisible" />
                  )}
                  <Child item={item} />
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
      <div
        className={`relative flex h-6 items-center gap-1 px-2 ${isDropdownVisible ? 'z-1' : ''} ${
          readOnly ? 'cursor-not-allowed opacity-75' : ''
        }`}
        onClick={event => {
          event.stopPropagation()
          if (!justFocusedRef.current && !readOnly) {
            setIsDropdownVisible(false)
          }
          justFocusedRef.current = false
        }}
      >
        <SvgMask src={TriangleDownIcon} />
        <div className="grow">
          {visuallySelectedItem != null ? (
            <Child item={visuallySelectedItem} />
          ) : (
            multiple && <props.renderMultiple items={selectedItems} render={Child} />
          )}
        </div>
      </div>
      {/* Hidden, but required to exist for the width of the parent element to be correct. */}
      <div className="flex h-0 flex-col overflow-hidden">
        {items.map((item, i) => (
          <div
            key={i}
            className={`flex h-6 items-center gap-1 rounded-xl px-2 transition-colors ${
              i === visuallySelectedIndex
                ? `cursor-default bg-frame font-bold`
                : 'hover:bg-frame-selected'
            }`}
          >
            {selectedIndices.includes(i) ? (
              <SvgMask src={CheckMarkIcon} />
            ) : (
              <SvgMask src={TriangleDownIcon} className="invisible" />
            )}
            <Child item={item} />
          </div>
        ))}
      </div>
    </div>
  )
}
