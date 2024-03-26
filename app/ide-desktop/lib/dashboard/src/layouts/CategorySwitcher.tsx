/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as localStorageProvider from '#/providers/LocalStorageProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/styled/UnstyledButton'
import SvgMask from '#/components/SvgMask'

import type * as backend from '#/services/Backend'

// =============
// === Types ===
// =============

/** Metadata for a category. */
interface CategoryMetadata {
  readonly category: Category
  readonly icon: string
}

// =================
// === Constants ===
// =================

const CATEGORY_DATA: CategoryMetadata[] = [
  { category: Category.recent, icon: RecentIcon },
  { category: Category.home, icon: Home2Icon },
  { category: Category.trash, icon: Trash2Icon },
]

// ============================
// === CategorySwitcherItem ===
// ============================

/** Props for a {@link CategorySwitcherItem}. */
interface InternalCategorySwitcherItemProps {
  readonly id: string
  readonly data: CategoryMetadata
  readonly isCurrent: boolean
  readonly onPress: (event: aria.PressEvent) => void
  readonly acceptedDragTypes: string[]
  readonly onDrop: (event: aria.DropEvent) => void
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
  const { data, isCurrent, onPress, acceptedDragTypes, onDrop } = props
  const { category, icon } = data

  return (
    <aria.DropZone
      aria-label={`Move To ${category}`}
      getDropOperation={types =>
        acceptedDragTypes.some(type => types.has(type)) ? 'move' : 'cancel'
      }
      className="drop-target-after rounded-full"
      // Required because `dragover` does not fire on `mouseenter`.
      onDrop={onDrop}
    >
      <UnstyledButton
        aria-label={`Go To ${category}`}
        className={`rounded-inherit ${isCurrent ? 'focus-default' : ''}`}
        onPress={onPress}
      >
        <div
          className={`selectable ${
            isCurrent ? 'disabled bg-selected-frame active' : ''
          } rounded-inherit group flex h-row items-center gap-icon-with-text px-button-x hover:bg-selected-frame`}
        >
          <SvgMask
            src={icon}
            className={
              // This explicit class is a special-case due to the unusual shape of the "Recent" icon.
              // eslint-disable-next-line no-restricted-syntax
              category === Category.recent ? '-ml-0.5' : ''
            }
          />
          <aria.Text slot="description">{category}</aria.Text>
        </div>
      </UnstyledButton>
    </aria.DropZone>
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
  const { localStorage } = localStorageProvider.useLocalStorage()

  React.useEffect(() => {
    localStorage.set('driveCategory', category)
  }, [category, /* should never change */ localStorage])

  return (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div ref={ref} className="flex w-full flex-col" {...innerProps}>
          <aria.Header
            id="header"
            className="text-header mb-sidebar-section-heading-b px-sidebar-section-heading-x text-sm font-bold"
          >
            Category
          </aria.Header>
          <div aria-label="Category switcher" role="grid" className="flex flex-col items-start">
            {CATEGORY_DATA.map(data => (
              <CategorySwitcherItem
                key={data.category}
                id={data.category}
                data={data}
                isCurrent={category === data.category}
                onPress={() => {
                  setCategory(data.category)
                }}
                acceptedDragTypes={
                  (category === Category.trash && data.category === Category.home) ||
                  (category !== Category.trash && data.category === Category.trash)
                    ? ['application/vnd.enso.assets+json']
                    : []
                }
                onDrop={event => {
                  void Promise.all(
                    event.items.flatMap(async item => {
                      if (item.kind === 'text') {
                        const text = await item.getText('application/vnd.enso.assets+json')
                        const payload: unknown = JSON.parse(text)
                        return Array.isArray(payload)
                          ? payload.flatMap(key =>
                              // This is SAFE, assuming only this app creates payloads with
                              // the specific mimetype above.
                              // eslint-disable-next-line no-restricted-syntax
                              typeof key === 'string' ? [key as backend.AssetId] : []
                            )
                          : []
                      } else {
                        return []
                      }
                    })
                  ).then(keys => {
                    dispatchAssetEvent({
                      type:
                        category === Category.trash
                          ? AssetEventType.restore
                          : AssetEventType.delete,
                      ids: new Set(keys.flat(1)),
                    })
                  })
                }}
              />
            ))}
          </div>
        </div>
      )}
    </FocusArea>
  )
}
