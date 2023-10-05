/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as modalProvider from '../../providers/modal'

import Label, * as labelModule from './label'
import NewLabelModal from './newLabelModal'

// ==============
// === Labels ===
// ==============

/** Props for a {@link Labels}. */
export interface LabelsProps {
    labels: backendModule.Label[]
    currentLabels: backendModule.LabelName[]
    setCurrentLabels: React.Dispatch<React.SetStateAction<backendModule.LabelName[]>>
    doCreateLabel: (name: string) => void
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
    const { labels, currentLabels, setCurrentLabels, doCreateLabel } = props
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
                        color={label.color}
                        active={currentLabels.includes(label.value)}
                        onClick={() => {
                            setCurrentLabels(oldLabels =>
                                oldLabels.includes(label.value)
                                    ? oldLabels.filter(oldLabel => oldLabel !== label.value)
                                    : [...oldLabels, label.value]
                            )
                        }}
                    >
                        {label.value}
                    </Label>
                ))}
                <Label
                    active
                    color={labelModule.DEFAULT_LABEL_COLOR}
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
