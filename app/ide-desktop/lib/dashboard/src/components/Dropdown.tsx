/** @file A styled dropdown. */
import * as React from 'react'

import TriangleDownIcon from 'enso-assets/triangle_down.svg'

import SvgMask from '#/components/SvgMask'

// ================
// === Dropdown ===
// ================

/** Props for a list item child. */
interface InternalChildProps<T> {
  item: T
}

/** Props for a {@link Dropdown}. */
export interface DropdownProps<T> {
  className?: string
  items: T[]
  selectedIndex: number | null
  render: (props: InternalChildProps<T>) => React.ReactNode
  onClick: (item: T, index: number) => void
}

/** A styled dropdown. */
export default function Dropdown<T>(props: DropdownProps<T>) {
  const { className, items, selectedIndex, render: Child, onClick } = props
  const [isDropdownVisible, setIsDropdownVisible] = React.useState(false)
  const isMouseDown = React.useRef(false)
  const selectedItem = selectedIndex == null ? null : items[selectedIndex]

  React.useEffect(() => {
    const onDocumentClick = () => {
      setIsDropdownVisible(false)
    }
    document.addEventListener('click', onDocumentClick)
    return () => {
      document.removeEventListener('click', onDocumentClick)
    }
  }, [])

  return (
    <div
      tabIndex={-1}
      ref={el => {
        if (isDropdownVisible && el?.contains(document.activeElement) === true) {
          el.focus()
        }
      }}
      className={`group relative flex flex-col w-max items-center rounded-xl cursor-pointer leading-5 whitespace-nowrap ${
        className ?? ''
      }`}
      onFocus={event => {
        if (event.target === event.currentTarget) {
          setIsDropdownVisible(true)
        }
      }}
      onBlur={event => {
        if (event.target === event.currentTarget) {
          setIsDropdownVisible(false)
        }
      }}
      onKeyDown={event => {
        if (!event.ctrlKey && !event.shiftKey && !event.altKey && !event.metaKey) {
          switch (event.key) {
            case 'Escape': {
              event.stopPropagation()
              setIsDropdownVisible(false)
            }
          }
        }
      }}
    >
      <div
        className={`absolute left-0 w-max h-full ${isDropdownVisible ? 'z-1' : 'overflow-hidden'}`}
      >
        <div
          className={`relative before:absolute before:border before:border-black/10 before:rounded-xl before:backdrop-blur-3xl before:top-0 before:w-full before:transition-colors ${
            isDropdownVisible
              ? 'before:h-full before:shadow-soft'
              : 'before:h-6 group-hover:before:bg-frame'
          }`}
        >
          {/* Spacing. */}
          <div
            className="relative padding h-6"
            onClick={() => {
              setIsDropdownVisible(visible => !visible)
            }}
          />
          <div
            className={`relative grid rounded-xl w-full max-h-10lh transition-grid-template-rows ${
              isDropdownVisible ? 'grid-rows-1fr' : 'grid-rows-0fr'
            }`}
          >
            {items.map((item, i) => (
              <div
                tabIndex={0}
                className={`flex gap-1 rounded-xl px-2 h-6 transition-colors ${
                  i === selectedIndex
                    ? 'cursor-default bg-frame font-bold'
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
                  if (i !== selectedIndex) {
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
                <SvgMask src={TriangleDownIcon} className="invisible" />
                <Child item={item} />
              </div>
            ))}
          </div>
        </div>
      </div>
      <div
        className={`relative flex gap-1 items-center h-6 px-2 ${isDropdownVisible ? 'z-1' : ''}`}
        onClick={() => {
          setIsDropdownVisible(visible => !visible)
        }}
      >
        <SvgMask src={TriangleDownIcon} />
        <div className="grow">{selectedItem != null && <Child item={selectedItem} />}</div>
      </div>
    </div>
  )
}
