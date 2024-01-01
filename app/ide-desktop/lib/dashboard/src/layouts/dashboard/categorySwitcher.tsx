/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import RootIcon from 'enso-assets/root.svg'
import TempIcon from 'enso-assets/temp.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as events from '#/events'
import * as categorySwitcherConstants from '#/layouts/dashboard/categorySwitcher/categorySwitcherConstants'
import * as providers from '#/providers'
import * as drag from '#/util/drag'
import * as localStorageModule from '#/util/localStorage'

import SvgMask from '#/components/svgMask'

// ============================
// === CategorySwitcherItem ===
// ============================

/** Props for a {@link CategorySwitcherItem}. */
interface InternalCategorySwitcherItemProps {
    /** When true, the button is not faded out even when not hovered. */
    active?: boolean
    /** When true, the button is not clickable. */
    disabled?: boolean
    /** A title that is only shown when `disabled` is true. */
    hidden: boolean
    image: string
    name: string
    iconClassName?: string
    onClick: () => void
    onDragOver: (event: React.DragEvent) => void
    onDrop: (event: React.DragEvent) => void
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
    const {
        active = false,
        disabled = false,
        hidden,
        image,
        name,
        iconClassName,
        onClick,
        onDragOver,
        onDrop,
    } = props
    return (
        <div
            className={`group flex items-center rounded-full gap-2 h-8 px-2 ${
                hidden ? 'hidden' : ''
            } ${active ? 'bg-frame-selected' : 'text-not-selected'} ${
                disabled
                    ? ''
                    : 'hover:text-primary hover:bg-frame-selected cursor-pointer hover:opacity-100'
            } ${!active && disabled ? 'cursor-not-allowed' : ''}`}
            {...(disabled ? {} : { onClick, onDragOver, onDrop })}
        >
            <SvgMask
                src={image}
                className={`${active ? 'text-icon-selected' : 'text-icon-not-selected'} ${
                    disabled ? '' : 'group-hover:text-icon-selected'
                } ${iconClassName ?? ''}`}
            />
            <span>{name}</span>
        </div>
    )
}

// ========================
// === CategorySwitcher ===
// ========================

const CATEGORIES: categorySwitcherConstants.Category[] = [
    categorySwitcherConstants.Category.recent,
    categorySwitcherConstants.Category.drafts,
    categorySwitcherConstants.Category.home,
    categorySwitcherConstants.Category.root,
    categorySwitcherConstants.Category.trash,
]

const IS_NOT_YET_IMPLEMENTED: Record<categorySwitcherConstants.Category, boolean> = {
    [categorySwitcherConstants.Category.recent]: false,
    [categorySwitcherConstants.Category.drafts]: true,
    [categorySwitcherConstants.Category.home]: false,
    [categorySwitcherConstants.Category.root]: true,
    [categorySwitcherConstants.Category.trash]: false,
}

const CATEGORY_ICONS: Record<categorySwitcherConstants.Category, string> = {
    [categorySwitcherConstants.Category.recent]: RecentIcon,
    [categorySwitcherConstants.Category.drafts]: TempIcon,
    [categorySwitcherConstants.Category.home]: Home2Icon,
    [categorySwitcherConstants.Category.root]: RootIcon,
    [categorySwitcherConstants.Category.trash]: Trash2Icon,
}

const CATEGORY_CLASS_NAMES: Record<categorySwitcherConstants.Category, string> = {
    [categorySwitcherConstants.Category.recent]: '-ml-0.5',
    [categorySwitcherConstants.Category.drafts]: '-ml-0.5',
    [categorySwitcherConstants.Category.home]: '',
    [categorySwitcherConstants.Category.root]: '',
    [categorySwitcherConstants.Category.trash]: '',
} as const

/** Props for a {@link CategorySwitcher}. */
export interface CategorySwitcherProps {
    category: categorySwitcherConstants.Category
    setCategory: (category: categorySwitcherConstants.Category) => void
    dispatchAssetEvent: (directoryEvent: events.AssetEvent) => void
}

/** A switcher to choose the currently visible assets table category. */
export default function CategorySwitcher(props: CategorySwitcherProps) {
    const { category, setCategory, dispatchAssetEvent } = props
    const { unsetModal } = providers.useSetModal()
    const { localStorage } = providers.useLocalStorage()

    React.useEffect(() => {
        localStorage.set(localStorageModule.LocalStorageKey.driveCategory, category)
    }, [category, /* should never change */ localStorage])

    return (
        <div className="flex flex-col items-start w-30">
            <div className="pl-2 pb-1.5">
                <span className="inline-block font-bold text-sm leading-144.5 h-6 py-0.5">
                    Category
                </span>
            </div>
            {CATEGORIES.map(currentCategory => (
                <CategorySwitcherItem
                    key={currentCategory}
                    active={category === currentCategory}
                    disabled={category === currentCategory}
                    hidden={IS_NOT_YET_IMPLEMENTED[currentCategory]}
                    image={CATEGORY_ICONS[currentCategory]}
                    name={currentCategory}
                    iconClassName={CATEGORY_CLASS_NAMES[currentCategory]}
                    onClick={() => {
                        setCategory(currentCategory)
                    }}
                    onDragOver={event => {
                        if (
                            (category === categorySwitcherConstants.Category.trash &&
                                currentCategory === categorySwitcherConstants.Category.home) ||
                            (category !== categorySwitcherConstants.Category.trash &&
                                currentCategory === categorySwitcherConstants.Category.trash)
                        ) {
                            event.preventDefault()
                        }
                    }}
                    onDrop={event => {
                        if (
                            (category === categorySwitcherConstants.Category.trash &&
                                currentCategory === categorySwitcherConstants.Category.home) ||
                            (category !== categorySwitcherConstants.Category.trash &&
                                currentCategory === categorySwitcherConstants.Category.trash)
                        ) {
                            event.preventDefault()
                            event.stopPropagation()
                            unsetModal()
                            const payload = drag.ASSET_ROWS.lookup(event)
                            if (payload != null) {
                                dispatchAssetEvent({
                                    type:
                                        category === categorySwitcherConstants.Category.trash
                                            ? events.AssetEventType.restore
                                            : events.AssetEventType.delete,
                                    ids: new Set(payload.map(item => item.asset.id)),
                                })
                            }
                        }
                    }}
                />
            ))}
        </div>
    )
}
