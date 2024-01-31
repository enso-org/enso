/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/dashboard/CategorySwitcher/Category'

import SvgMask from '#/components/SvgMask'

import * as drag from '#/utilities/drag'
import * as localStorageModule from '#/utilities/LocalStorage'

// ============================
// === CategorySwitcherItem ===
// ============================

/** Props for a {@link CategorySwitcherItem}. */
interface InternalCategorySwitcherItemProps {
  /** When true, the button is not faded out even when not hovered. */
  active?: boolean
  /** When true, the button is not clickable. */
  disabled?: boolean
  image: string
  name: string
  iconClassName?: string
  onClick: () => void
  onDragOver: (event: React.DragEvent) => void
  onDrop: (event: React.DragEvent) => void
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
  const { active = false, disabled = false, image, name, iconClassName, onClick } = props
  const { onDragOver, onDrop } = props
  return (
    <button
      disabled={disabled}
      title={`Go To ${name}`}
      className={`group flex items-center rounded-full gap-2 h-8 px-2 hover:bg-frame-selected transition-colors ${
        active ? 'bg-frame-selected' : 'text-not-selected'
      } ${disabled ? '' : 'hover:text-primary hover:bg-frame-selected hover:opacity-100'} ${
        !active && disabled ? 'cursor-not-allowed' : ''
      }`}
      onClick={onClick}
      // Required because `dragover` does not fire on `mouseenter`.
      onDragEnter={onDragOver}
      onDragOver={onDragOver}
      onDrop={onDrop}
    >
      <SvgMask
        src={image}
        className={`group-hover:text-icon-selected ${
          active ? 'text-icon-selected' : 'text-icon-not-selected'
        } ${iconClassName ?? ''}`}
      />
      <span>{name}</span>
    </button>
  )
}

// ========================
// === CategorySwitcher ===
// ========================

const CATEGORIES: Category[] = [Category.recent, Category.home, Category.trash]

const CATEGORY_ICONS: Record<Category, string> = {
  [Category.recent]: RecentIcon,
  [Category.home]: Home2Icon,
  [Category.trash]: Trash2Icon,
}

const CATEGORY_CLASS_NAMES: Record<Category, string> = {
  [Category.recent]: '-ml-0.5',
  [Category.home]: '',
  [Category.trash]: '',
} as const

/** Props for a {@link CategorySwitcher}. */
export interface CategorySwitcherProps {
  category: Category
  setCategory: (category: Category) => void
  dispatchAssetEvent: (directoryEvent: assetEvent.AssetEvent) => void
}

/** A switcher to choose the currently visible assets table category. */
export default function CategorySwitcher(props: CategorySwitcherProps) {
  const { category, setCategory, dispatchAssetEvent } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()

  React.useEffect(() => {
    localStorage.set(localStorageModule.LocalStorageKey.driveCategory, category)
  }, [category, /* should never change */ localStorage])

  return (
    <div className="flex flex-col items-start w-30">
      <div className="pl-2 pb-1.5">
        <span className="inline-block font-bold text-sm leading-144.5 h-6 py-0.5">Category</span>
      </div>
      {CATEGORIES.map(currentCategory => (
        <CategorySwitcherItem
          key={currentCategory}
          active={category === currentCategory}
          disabled={category === currentCategory}
          image={CATEGORY_ICONS[currentCategory]}
          name={currentCategory}
          iconClassName={CATEGORY_CLASS_NAMES[currentCategory]}
          onClick={() => {
            setCategory(currentCategory)
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
  )
}
