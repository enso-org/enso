/** @file A styled dropdown. */
import { useEffect, useMemo, useRef, useState, type ForwardedRef, type ReactNode } from 'react'

import CheckMarkIcon from '#/assets/check_mark.svg'
import FolderArrowIcon from '#/assets/folder_arrow.svg'
import { ListBox, ListBoxItem, mergeProps, useFocusWithin } from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'
import { forwardRef } from '#/utilities/react'
import { twMerge } from '#/utilities/tailwindMerge'

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
  readonly render: (props: InternalChildProps<T>) => ReactNode
}

/** Props for a {@link Dropdown} shared between all variants. */
interface InternalBaseDropdownProps<T> {
  readonly readOnly?: boolean
  readonly className?: string
  readonly items: readonly T[]
  readonly render: (props: InternalChildProps<T>) => ReactNode
}

/** Props for a {@link Dropdown}, when `multiple` is `false` or absent. */
interface InternalSingleDropdownProps<T> extends InternalBaseDropdownProps<T> {
  readonly multiple?: false
  readonly selectedIndex: number | null
  readonly onChange: (item: T, index: number) => void
}

/** Props for a {@link Dropdown}, when `multiple` is `true`. */
interface InternalMultipleDropdownProps<T> extends InternalBaseDropdownProps<T> {
  readonly multiple: true
  readonly selectedIndices: readonly number[]
  readonly renderMultiple: (props: InternalChildrenProps<T>) => ReactNode
  readonly onChange: (items: readonly T[], indices: readonly number[]) => void
}

/** Props for a {@link Dropdown}. */
export type DropdownProps<T> = InternalMultipleDropdownProps<T> | InternalSingleDropdownProps<T>

/** A styled dropdown. */
export const Dropdown = forwardRef(function Dropdown<T>(
  props: DropdownProps<T>,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const { readOnly = false, className, items, render: Child } = props
  const listBoxItems = useMemo(() => items.map((item, i) => ({ item, i })), [items])
  const [tempSelectedIndex, setTempSelectedIndex] = useState<number | null>(null)
  const rootRef = useRef<HTMLDivElement | null>(null)
  const [isFocused, setIsFocused] = useState(false)
  const { focusWithinProps } = useFocusWithin({ onFocusWithinChange: setIsFocused })
  const multiple = props.multiple === true
  const selectedIndex = 'selectedIndex' in props ? props.selectedIndex : null
  const selectedIndices = 'selectedIndices' in props ? props.selectedIndices : []
  const selectedItems = selectedIndices.flatMap((index) => {
    const item = items[index]
    return item != null ? [item] : []
  })
  const visuallySelectedIndex = tempSelectedIndex ?? selectedIndex
  const visuallySelectedItem = visuallySelectedIndex == null ? null : items[visuallySelectedIndex]

  useEffect(() => {
    setTempSelectedIndex(selectedIndex)
  }, [selectedIndex])

  useEffect(() => {
    const onDocumentClick = () => {
      rootRef.current?.blur()
    }
    document.addEventListener('click', onDocumentClick)
    return () => {
      document.removeEventListener('click', onDocumentClick)
    }
  }, [])

  return (
    <FocusRing placement="outset">
      <div
        {...mergeProps<JSX.IntrinsicElements['div']>()(
          focusWithinProps,
          { ref: rootRef },
          {
            tabIndex: 0,
            ref,
            className: twMerge(
              'focus-child group relative flex w-max cursor-pointer flex-col items-start whitespace-nowrap rounded-input leading-cozy',
              className,
            ),
          },
        )}
      >
        <div
          className={twMerge(
            'absolute left-0 h-full w-full min-w-max',
            isFocused ? 'z-1' : 'overflow-hidden',
          )}
        >
          <div
            className={twMerge(
              'relative before:absolute before:top before:w-full before:rounded-input before:border-0.5 before:border-primary/20 before:backdrop-blur-default before:transition-colors',
              isFocused ?
                'before:h-full before:shadow-soft'
              : 'before:h-6 group-hover:before:bg-hover-bg',
            )}
          >
            {/* Spacing. */}
            <div className="padding relative h-6" />
            <div
              className={twMerge(
                'relative grid max-h-dropdown-items w-full overflow-auto rounded-input transition-grid-template-rows',
                isFocused ? 'grid-rows-1fr' : 'grid-rows-0fr',
              )}
            >
              <ListBox
                selectionMode={
                  !isFocused ? 'none'
                  : multiple ?
                    'multiple'
                  : 'single'
                }
                items={listBoxItems}
                className="overflow-hidden"
                onSelectionChange={(keys) => {
                  if (multiple) {
                    const indices = Array.from(keys, (i) => Number(i))
                    props.onChange(
                      indices.flatMap((i) => {
                        const item = items[i]
                        return item === undefined ? [] : [item]
                      }),
                      indices,
                    )
                  } else {
                    const [key] = keys
                    if (key != null) {
                      const i = Number(key)
                      const item = items[i]
                      if (item !== undefined) {
                        props.onChange(item, i)
                      }
                    }
                  }
                }}
              >
                {({ item, i }) => (
                  <ListBoxItem
                    key={i}
                    id={i}
                    className={twMerge(
                      'not-selected:hover:bg-hover-bg not-focus:hover:bg-hover-bg flex h-6 items-center gap-dropdown-arrow rounded-input px-input-x transition-colors selected:focus:cursor-default selected:focus:bg-frame selected:focus:font-bold selected:focus:focus-ring',
                      multiple && 'hover:font-semibold',
                      i === visuallySelectedIndex ?
                        'cursor-default bg-frame font-bold focus-ring'
                      : 'hover:bg-hover-bg',
                    )}
                  >
                    <SvgMask
                      src={CheckMarkIcon}
                      className={selectedIndices.includes(i) ? '' : 'invisible'}
                    />
                    <Child item={item} />
                  </ListBoxItem>
                )}
              </ListBox>
            </div>
          </div>
        </div>
        <div
          className={twMerge(
            'relative flex h-6 items-center gap-dropdown-arrow px-input-x',
            isFocused && 'z-1',
            readOnly && 'read-only',
          )}
        >
          <SvgMask src={FolderArrowIcon} className="rotate-90" />
          <div className="grow">
            {visuallySelectedItem != null ?
              <Child item={visuallySelectedItem} />
            : multiple && <props.renderMultiple items={selectedItems} render={Child} />}
          </div>
        </div>
        {/* Hidden, but required to exist for the width of the parent element to be correct.
         * Classes that do not affect width have been removed. */}
        <div className="flex h-0 flex-col overflow-hidden">
          {items.map((item, i) => (
            <div key={i} className="flex gap-dropdown-arrow px-input-x font-bold">
              <SvgMask src={CheckMarkIcon} />
              <Child item={item} />
            </div>
          ))}
        </div>
      </div>
    </FocusRing>
  )
})
