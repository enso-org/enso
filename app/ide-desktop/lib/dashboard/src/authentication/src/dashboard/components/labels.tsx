/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as modalProvider from '../../providers/modal'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps extends React.PropsWithChildren {
    /** When true, the button is not faded out even when not hovered. */
    active?: boolean
    /** When true, the button is not clickable. */
    disabled?: boolean
    className: string
    onClick: () => void
}

/** An entry in a {@link CategorySwitcher}. */
function Label(props: InternalLabelProps) {
    const { active = false, disabled = false, className, onClick, children } = props
    return (
        <div
            className={`flex items-center rounded-full gap-1.5 h-6 px-2.25 ${className} ${
                active ? 'bg-frame-selected' : 'text-not-selected'
            } ${
                disabled
                    ? ''
                    : 'hover:text-primary hover:bg-frame-selected cursor-pointer hover:opacity-100'
            } ${!active && disabled ? 'cursor-not-allowed' : ''}`}
            {...(disabled ? {} : { onClick })}
        >
            {children}
        </div>
    )
}

// ==============
// === Labels ===
// ==============

/** Props for a {@link Labels}. */
export interface LabelsProps {
    labels: backendModule.Label[]
    currentLabel: backendModule.TagAssetAssociationId | null
    setCurrentLabel: (currentLabel: backendModule.TagAssetAssociationId | null) => void
    doCreateLabel: (name: string) => void
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
    const { labels, currentLabel, setCurrentLabel, doCreateLabel } = props
    const { setModal } = modalProvider.useSetModal()
    return (
        <div className="flex flex-col items-start w-30">
            <div className="pl-2 pb-1.5">
                <span className="inline-block font-bold text-sm leading-144.5 h-6 py-0.5">
                    Category
                </span>
            </div>
            <div className="flex flex-col items-start gap-1">
                {labels.map(label => (
                    <Label
                        key={label.id}
                        active={label.id === currentLabel}
                        disabled={label.id === currentLabel}
                        className="text-white"
                        onClick={() => {
                            setCurrentLabel(label.id)
                        }}
                    >
                        {label.value}
                    </Label>
                ))}
                <Label
                    className="text-not-selected"
                    onClick={() => {
                        setModal(<NewLabelModal doCreate={doCreateLabel} />)
                    }}
                >
                    <img src={PlusIcon} className="w-1.5 h-1.5" />
                    <span className="leading-144.5 h-6 py-0.5">new label</span>
                </Label>
            </div>
        </div>
    )
}
