/** @file A styled dropdown. */
import * as React from 'react'

import FolderArrowIcon from 'enso-assets/folder_arrow.svg'

import SvgMask from '#/components/SvgMask'

// ================
// === Dropdown ===
// ================

/** Props for a list item child. */
interface InternalChildProps<T> {
  readonly item: T
}

/** Props for a {@link Dropdown}. */
export interface DropdownProps<T> {
  readonly readOnly?: boolean
  readonly className?: string
  readonly items: T[]
  readonly selectedIndex: number | null
  readonly render: (props: InternalChildProps<T>) => React.ReactNode
  readonly onClick: (item: T, index: number) => void
}

/** A styled dropdown. */
export default function Dropdown<T>(props: DropdownProps<T>) {
  const { readOnly = false, className, items, selectedIndex, render: Child, onClick } = props
  const [isDropdownVisible, setIsDropdownVisible] = React.useState(false)
  const [tempSelectedIndex, setTempSelectedIndex] = React.useState<number | null>(null)
  const rootRef = React.useRef<HTMLDivElement>(null)
  const isMouseDown = React.useRef(false)
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
              onClick(item, tempSelectedIndex)
            }
          }
          setIsDropdownVisible(false)
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
      className={`group relative flex w-max cursor-pointer flex-col items-center whitespace-nowrap rounded-input leading-cozy ${
        className ?? ''
      }`}
      onFocus={event => {
        if (!readOnly && event.target === event.currentTarget) {
          setIsDropdownVisible(true)
        }
      }}
      onBlur={event => {
        if (!readOnly && event.target === event.currentTarget) {
          setIsDropdownVisible(false)
        }
      }}
      onKeyDown={onKeyDown}
    >
      <div
        className={`absolute left h-full w-max ${isDropdownVisible ? 'z-1' : 'overflow-hidden'}`}
      >
        <div
          className={`relative before:absolute before:top before:w-full before:rounded-input before:border before:border-black/10 before:backdrop-blur-default before:transition-colors ${
            isDropdownVisible
              ? 'before:h-full before:bg-frame before:shadow-soft'
              : 'before:h-text group-hover:before:bg-frame'
          }`}
        >
          {/* Spacing. */}
          <div className="padding relative h-text" />
          <div
            className={`relative grid max-h-dropdown-items w-full overflow-auto rounded-input transition-grid-template-rows ${
              isDropdownVisible ? 'grid-rows-1fr' : 'grid-rows-0fr'
            }`}
          >
            {items.map((item, i) => (
              <div
                className={`flex h-text gap-dropdown-arrow rounded-input px-input-x transition-colors ${
                  i === visuallySelectedIndex
                    ? 'cursor-default bg-frame font-bold'
                    : 'hover:bg-selected-frame'
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
                    setIsDropdownVisible(false)
                    onClick(item, i)
                  }
                }}
                onFocus={() => {
                  if (!isMouseDown.current) {
                    // This is from keyboard navigation.
                    onClick(item, i)
                  }
                }}
              >
                <div className="size-icon" />
                <Child item={item} />
              </div>
            ))}
          </div>
        </div>
      </div>
      <div
        className={`relative flex h-text items-center gap-dropdown-arrow px-input-x ${
          // This style is conditionally applied so that closed dropdowns do not draw over the
          // currently opened dropdown.
          isDropdownVisible ? 'z-1' : ''
        } ${readOnly ? 'read-only' : ''}`}
      >
        <SvgMask src={FolderArrowIcon} className="size-icon rotate-90" />
        <div className="grow">
          {visuallySelectedItem != null && <Child item={visuallySelectedItem} />}
        </div>
      </div>
    </div>
  )
}
