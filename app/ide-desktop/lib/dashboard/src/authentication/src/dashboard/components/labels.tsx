/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as modalProvider from '../../providers/modal'

import NewLabelModal from './newLabelModal'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps extends React.PropsWithChildren {
    /** When true, the button is not faded out even when not hovered. */
    active?: boolean
    className: string
    onClick: React.MouseEventHandler<HTMLDivElement>
}

/** An entry in a {@link CategorySwitcher}. */
function Label(props: InternalLabelProps) {
    const { active = false, className, onClick, children } = props
    return (
        <div
            className={`cursor-pointer flex items-center rounded-full gap-1.5 h-6 px-2.25 hover:opacity-100 ${className} ${
                active ? 'bg-frame-selected' : 'text-not-selected opacity-50'
            }`}
            onClick={onClick}
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
                    Labels
                </span>
            </div>
            <div className="flex flex-col items-start gap-1">
                {labels.map(label => (
                    <Label
                        key={label.id}
                        active={label.id === currentLabel}
                        className="bg-frame-selected text-primary"
                        onClick={() => {
                            setCurrentLabel(label.id === currentLabel ? null : label.id)
                        }}
                    >
                        {label.value}
                    </Label>
                ))}
                <Label
                    active
                    className="bg-frame-selected text-not-selected"
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <NewLabelModal
                                eventTarget={event.currentTarget}
                                doCreate={doCreateLabel}
                            />
                        )
                    }}
                >
                    <img src={PlusIcon} className="w-1.5 h-1.5" />
                    <span className="leading-144.5 h-6 py-0.5">new label</span>
                </Label>
            </div>
        </div>
    )
}
