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
      className={`group relative flex flex-col w-max items-center rounded-input cursor-pointer leading-cozy whitespace-nowrap ${
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
        className={`absolute left w-max h-full ${isDropdownVisible ? 'z-1' : 'overflow-hidden'}`}
      >
        <div
          className={`relative before:absolute before:border before:border-black/10 before:rounded-input before:backdrop-blur-default before:top before:w-full before:transition-colors ${
            isDropdownVisible
              ? 'before:bg-frame before:h-full before:shadow-soft'
              : 'before:h-text group-hover:before:bg-frame'
          }`}
        >
          {/* Spacing. */}
          <div className="relative padding h-text" />
          <div
            className={`relative grid rounded-input w-full max-h-dropdown-items overflow-auto transition-grid-template-rows ${
              isDropdownVisible ? 'grid-rows-1fr' : 'grid-rows-0fr'
            }`}
          >
            {items.map((item, i) => (
              <div
                className={`flex gap-dropdown-arrow rounded-input px-input-x h-text transition-colors ${
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
        className={`relative flex gap-dropdown-arrow items-center h-text px-input-x ${
          isDropdownVisible ? 'z-1' : ''
        } ${readOnly ? 'opacity-read-only cursor-not-allowed' : ''}`}
      >
        <SvgMask src={FolderArrowIcon} className="size-icon rotate-90" />
        <div className="grow">
          {visuallySelectedItem != null && <Child item={visuallySelectedItem} />}
        </div>
      </div>
    </div>
  )
}
