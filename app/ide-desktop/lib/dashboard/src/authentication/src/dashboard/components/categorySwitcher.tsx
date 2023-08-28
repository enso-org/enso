/** @file Switcher to choose the currently visible assets table category. */
import * as React from 'react'

import Home2Icon from 'enso-assets/home2.svg'
import RecentIcon from 'enso-assets/recent.svg'
import RootIcon from 'enso-assets/root.svg'
import TempIcon from 'enso-assets/temp.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

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
    image: string
    name: string
    /** A title that is only shown when `disabled` is true. */
    error?: string | null
    iconClassName?: string
    onClick: () => void
}

/** An entry in a {@link CategorySwitcher}. */
function CategorySwitcherItem(props: InternalCategorySwitcherItemProps) {
    const { active = false, disabled = false, image, name, error, iconClassName, onClick } = props
    return (
        <div
            {...(disabled && error != null ? { title: error } : {})}
            className={`flex items-center rounded-full gap-2 h-8 px-2 ${
                active ? 'bg-frame-selected' : 'text-not-selected'
            } ${disabled ? '' : 'cursor-pointer hover:opacity-100'} ${
                !active && disabled ? 'cursor-not-allowed' : ''
            }`}
            {...(disabled ? {} : { onClick })}
        >
            <SvgMask
                src={image}
                className={`${active ? 'text-icon-selected' : 'text-icon-not-selected'} ${
                    iconClassName ?? ''
                }`}
            />
            <span>{name}</span>
        </div>
    )
}

// ========================
// === CategorySwitcher ===
// ========================

/** A switcher to choose the currently visible assets table category. */
export default function CategorySwitcher() {
    return (
        <div className="flex flex-col items-start w-30">
            <div className="pl-2 pb-1.5">
                <span className="inline-block font-bold text-sm leading-144.5 h-6 py-0.5">
                    Category
                </span>
            </div>
            <CategorySwitcherItem
                disabled
                image={RecentIcon}
                name="Recent"
                error="Not implemented yet."
                iconClassName="-ml-0.5"
                onClick={() => {
                    // No backend support yet.
                }}
            />
            <CategorySwitcherItem
                disabled
                image={TempIcon}
                name="Drafts"
                error="Not implemented yet."
                iconClassName="-ml-0.5"
                onClick={() => {
                    // No backend support yet.
                }}
            />
            <CategorySwitcherItem
                active
                disabled
                image={Home2Icon}
                name="Home"
                onClick={() => {
                    // Not implemented yet - waiting on implementation details to determine how to
                    // switch back to the home category.
                }}
            />
            <CategorySwitcherItem
                disabled
                image={RootIcon}
                name="Root"
                error="Not implemented yet."
                onClick={() => {
                    // No backend support yet.
                }}
            />
            <CategorySwitcherItem
                disabled
                image={Trash2Icon}
                name="Trash"
                error="Not implemented yet."
                onClick={() => {
                    // No backend support yet.
                }}
            />
        </div>
    )
}
