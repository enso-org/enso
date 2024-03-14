/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/CategorySwitcher/Category'

import SvgMask from '#/components/SvgMask'

import * as drag from '#/utilities/drag'

// =================
// === Constants ===
// =================

const CATEGORIES = Object.values(Category)

const CATEGORY_ICONS: Readonly<Record<Category, string>> = {
  [Category.recent]: RecentIcon,
  [Category.home]: Home2Icon,
  [Category.trash]: Trash2Icon,
}

// ============================
// === CategorySwitcherItem ===
// ============================

/** Props for a {@link CategorySwitcherItem}. */
interface InternalCategorySwitcherItemProps {
  readonly focusRing?: boolean
  readonly category: Category
  readonly isCurrent: boolean
  readonly onClick: (event: React.MouseEvent<HTMLButtonElement>) => void
  readonly onDragOver: (event: React.DragEvent) => void
  readonly onDrop: (event: React.DragEvent) => void
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItemInternal(
  props: InternalCategorySwitcherItemProps,
  ref: React.ForwardedRef<HTMLButtonElement>
) {
  const { focusRing = false, category, isCurrent, onClick, onDragOver, onDrop } = props

  return (
    <div
      className={`relative after:pointer-events-none after:absolute after:inset after:rounded-full after:transition-all ${focusRing ? 'after:focus-ring' : ''}`}
    >
      <button
        ref={ref}
        tabIndex={0} // Required so that it is still selectable when disabled.
        title={`Go To ${category}`}
        className={`selectable ${
          isCurrent ? 'disabled bg-selected-frame active' : ''
        } group flex h-row items-center gap-icon-with-text rounded-full px-button-x hover:bg-selected-frame`}
        onClick={onClick}
        // Required because `dragover` does not fire on `mouseenter`.
        onDragEnter={onDragOver}
        onDragOver={onDragOver}
        onDrop={onDrop}
      >
        <SvgMask
          src={CATEGORY_ICONS[category]}
          className={
            // This explicit class is a special-case due to the unusual shape of the "Recent" icon.
            // eslint-disable-next-line no-restricted-syntax
            category === Category.recent ? '-ml-0.5' : ''
          }
        />
        <span>{category}</span>
      </button>
    </div>
  )
}

/** An entry in a {@link CategorySwitcher}. */
// This is a function, even though it does not contain a function expression.
// eslint-disable-next-line no-restricted-syntax
const CategorySwitcherItem = React.forwardRef(CategorySwitcherItemInternal)

// ========================
// === CategorySwitcher ===
// ========================

/** Props for a {@link CategorySwitcher}. */
export interface CategorySwitcherProps {
  readonly category: Category
  readonly setCategory: (category: Category) => void
  readonly dispatchAssetEvent: (directoryEvent: assetEvent.AssetEvent) => void
}

/** A switcher to choose the currently visible assets table category. */
export default function CategorySwitcher(props: CategorySwitcherProps) {
  const { category, setCategory, dispatchAssetEvent } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const rootRef = React.useRef<HTMLDivElement>(null)
  const [selectedChildElement, setSelectedChildElement] = React.useState<HTMLElement | null>(null)
  const selectedChildIndexRef = React.useRef(0)
  const navigator2D = navigator2DProvider.useNavigator2D()

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, {
      length: CATEGORIES.length,
      defaultIndex: selectedChildIndexRef.current,
    })

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      navigator2D.register(root, {
        focusPrimaryChild: () => {
          setKeyboardSelectedIndex(selectedChildIndexRef.current)
        },
        focusWhenPressed: {
          up: setKeyboardSelectedIndex.bind(null, CATEGORIES.length - 1),
          down: setKeyboardSelectedIndex.bind(null, 0),
        },
      })
      return () => {
        navigator2D.unregister(root)
      }
    }
  }, [selectedChildElement, navigator2D, setKeyboardSelectedIndex])

  React.useEffect(() => {
    localStorage.set('driveCategory', category)
    selectedChildIndexRef.current = CATEGORIES.indexOf(category)
  }, [category, /* should never change */ localStorage])

  return (
    <div ref={rootRef} className="flex w-full flex-col gap-sidebar-section-heading">
      <div className="text-header px-sidebar-section-heading-x text-sm font-bold">Category</div>
      <div className="flex flex-col items-start">
        {CATEGORIES.map((currentCategory, i) => (
          <CategorySwitcherItem
            key={currentCategory}
            ref={element => {
              if (category === currentCategory) {
                setSelectedChildElement(element)
              }
              if (keyboardSelectedIndex === i) {
                element?.focus()
              }
            }}
            focusRing={i === keyboardSelectedIndex}
            category={currentCategory}
            isCurrent={category === currentCategory}
            onClick={() => {
              setCategory(currentCategory)
              setKeyboardSelectedIndex(null)
            }}
            onDragOver={event => {
              if (
                (category === Category.trash && currentCategory === Category.home) ||
                (category !== Category.trash && currentCategory === Category.trash)
              ) {
                event.preventDefault()
              }
            }}
            onDrop={event => {
              if (
                (category === Category.trash && currentCategory === Category.home) ||
                (category !== Category.trash && currentCategory === Category.trash)
              ) {
                event.preventDefault()
                event.stopPropagation()
                unsetModal()
                const payload = drag.ASSET_ROWS.lookup(event)
                if (payload != null) {
                  dispatchAssetEvent({
                    type:
                      category === Category.trash ? AssetEventType.restore : AssetEventType.delete,
                    ids: new Set(payload.map(item => item.key)),
                  })
                }
              }
            }}
          />
        ))}
      </div>
    </div>
  )
}
