/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import RootIcon from 'enso-assets/root.svg'
import TempIcon from 'enso-assets/temp.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as localStorageModule from '../localStorage'
import * as localStorageProvider from '../../providers/localStorage'

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
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
    const { active = false, disabled = false, hidden, image, name, iconClassName, onClick } = props
    return (
        <div
            className={`group flex h-8 items-center gap-2 rounded-full px-2 ${
                hidden ? 'hidden' : ''
            } ${active ? 'bg-frame-selected' : 'text-not-selected'} ${
                disabled
                    ? ''
                    : 'cursor-pointer hover:bg-frame-selected hover:text-primary hover:opacity-100'
            } ${!active && disabled ? 'cursor-not-allowed' : ''}`}
            {...(disabled ? {} : { onClick })}
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
}

/** A switcher to choose the currently visible assets table category. */
export default function CategorySwitcher(props: CategorySwitcherProps) {
    const { category, setCategory } = props
    const { localStorage } = localStorageProvider.useLocalStorage()

    React.useEffect(() => {
        localStorage.set(localStorageModule.LocalStorageKey.driveCategory, category)
    }, [category, /* should never change */ localStorage])

    return (
        <div className="flex w-30 flex-col items-start">
            <div className="pb-1.5 pl-2">
                <span className="inline-block h-6 py-0.5 text-sm font-bold leading-144.5">
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
                />
            ))}
        </div>
    )
}
