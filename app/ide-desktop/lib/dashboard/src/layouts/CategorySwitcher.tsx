/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as modalProvider from '#/providers/ModalProvider'

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
  readonly category: Category
  readonly isCurrent: boolean
  readonly onClick: () => void
  readonly onDragOver: (event: React.DragEvent) => void
  readonly onDrop: (event: React.DragEvent) => void
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
  const { category, isCurrent, onClick } = props
  const { onDragOver, onDrop } = props

  return (
    <button
      disabled={isCurrent}
      title={`Go To ${category}`}
      className={`selectable ${
        isCurrent ? 'bg-selected-frame active' : ''
      } group flex h-row items-center gap-icon-with-text rounded-full px-button-x transition-colors hover:bg-selected-frame`}
      onClick={onClick}
      // Required because `dragover` does not fire on `mouseenter`.
      onDragEnter={onDragOver}
      onDragOver={onDragOver}
      onDrop={onDrop}
    >
      <SvgMask
        src={CATEGORY_ICONS[category]}
        className={`group-hover:text-icon-selected ${
          isCurrent ? 'text-icon-selected' : 'text-icon-not-selected'
        } ${
          // This explicit class is a special-case due to the unusual shape of the "Recent" icon.
          // eslint-disable-next-line no-restricted-syntax
          category === Category.recent ? '-ml-0.5' : ''
        }`}
      />
      <span>{category}</span>
    </button>
  )
}

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

  return (
    <div className="flex w-full flex-col gap-sidebar-section-heading">
      <div className="text-header px-sidebar-section-heading-x text-sm font-bold">Category</div>
      <div className="flex flex-col items-start">
        {CATEGORIES.map(currentCategory => (
          <CategorySwitcherItem
            key={currentCategory}
            category={currentCategory}
            isCurrent={category === currentCategory}
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
    </div>
  )
}
