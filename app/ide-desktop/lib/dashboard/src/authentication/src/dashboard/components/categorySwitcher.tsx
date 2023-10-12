/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import RootIcon from 'enso-assets/root.svg'
import TempIcon from 'enso-assets/temp.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as assetEvent from '../events/assetEvent'
import * as localStorageModule from '../localStorage'
import * as localStorageProvider from '../../providers/localStorage'
import * as modalProvider from '../../providers/modal'

import * as assetsTable from './assetsTable'
import SvgMask from '../../authentication/components/svgMask'

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

/** The categories available in the category switcher. */
export enum Category {
    recent = 'Recent',
    drafts = 'Drafts',
    home = 'Home',
    root = 'Root',
    trash = 'Trash',
}

const CATEGORIES: Category[] = [
    Category.recent,
    Category.drafts,
    Category.home,
    Category.root,
    Category.trash,
]

const IS_NOT_YET_IMPLEMENTED: Record<Category, boolean> = {
    [Category.recent]: false,
    [Category.drafts]: true,
    [Category.home]: false,
    [Category.root]: true,
    [Category.trash]: false,
}

const CATEGORY_ICONS: Record<Category, string> = {
    [Category.recent]: RecentIcon,
    [Category.drafts]: TempIcon,
    [Category.home]: Home2Icon,
    [Category.root]: RootIcon,
    [Category.trash]: Trash2Icon,
}

const CATEGORY_CLASS_NAMES: Record<Category, string> = {
    [Category.recent]: '-ml-0.5',
    [Category.drafts]: '-ml-0.5',
    [Category.home]: '',
    [Category.root]: '',
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
                            const payload = assetsTable.tryGetAssetRowsDragPayload(
                                event.dataTransfer
                            )
                            if (payload != null) {
                                dispatchAssetEvent({
                                    type:
                                        category === Category.trash
                                            ? assetEvent.AssetEventType.restore
                                            : assetEvent.AssetEventType.delete,
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
