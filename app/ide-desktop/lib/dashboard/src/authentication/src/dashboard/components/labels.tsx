/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import type * as backend from '../backend'
import * as modalProvider from '../../providers/modal'

import Label, * as labelModule from './label'
import ConfirmDeleteModal from './confirmDeleteModal'
import NewLabelModal from './newLabelModal'
import SvgMask from '../../authentication/components/svgMask'

// ==============
// === Labels ===
// ==============

/** Props for a {@link Labels}. */
export interface LabelsProps {
    labels: backend.Label[]
    currentLabels: backend.LabelName[] | null
    setCurrentLabels: React.Dispatch<React.SetStateAction<backend.LabelName[] | null>>
    doCreateLabel: (name: string, color: backend.LChColor) => void
    doDeleteLabel: (id: backend.TagId, name: backend.LabelName) => void
    newLabelNames: Set<backend.LabelName>
    deletedLabelNames: Set<backend.LabelName>
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
    const {
        labels,
        currentLabels,
        setCurrentLabels,
        doCreateLabel,
        doDeleteLabel,
        newLabelNames,
        deletedLabelNames,
    } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div data-testid="labels" className="flex flex-col items-start w-30">
            <div className="pl-2 pb-1.5">
                <span className="inline-block font-bold text-sm leading-144.5 h-6 py-0.5">
                    Labels
                </span>
            </div>
            <ul data-testid="labels-list" className="flex flex-col items-start gap-1">
                {labels
                    .filter(label => !deletedLabelNames.has(label.value))
                    .map(label => (
                        <li key={label.id} className="group flex items-center gap-1">
                            <Label
                                color={label.color}
                                active={currentLabels?.includes(label.value) ?? false}
                                disabled={newLabelNames.has(label.value)}
                                onClick={() => {
                                    setCurrentLabels(oldLabels => {
                                        if (oldLabels == null) {
                                            return [label.value]
                                        } else {
                                            const newLabels = oldLabels.includes(label.value)
                                                ? oldLabels.filter(
                                                      oldLabel => oldLabel !== label.value
                                                  )
                                                : [...oldLabels, label.value]
                                            return newLabels.length === 0 ? null : newLabels
                                        }
                                    })
                                }}
                            >
                                {label.value}
                            </Label>
                            {!newLabelNames.has(label.value) && (
                                <button
                                    className="flex"
                                    onClick={event => {
                                        event.stopPropagation()
                                        setModal(
                                            <ConfirmDeleteModal
                                                description={`the label '${label.value}'`}
                                                doDelete={() => {
                                                    doDeleteLabel(label.id, label.value)
                                                }}
                                            />
                                        )
                                    }}
                                >
                                    <SvgMask
                                        src={Trash2Icon}
                                        alt="Delete"
                                        className="opacity-0 group-hover:opacity-100 text-delete w-4 h-4"
                                    />
                                </button>
                            )}
                        </li>
                    ))}
                <li>
                    <Label
                        active
                        color={labelModule.DEFAULT_LABEL_COLOR}
                        className="bg-frame-selected text-not-selected"
                        onClick={event => {
                            event.stopPropagation()
                            setModal(
                                <NewLabelModal
                                    labelNames={new Set(labels.map(label => label.value))}
                                    eventTarget={event.currentTarget}
                                    doCreate={doCreateLabel}
                                />
                            )
                        }}
                    >
                        <img src={PlusIcon} className="w-1.5 h-1.5" />
                        <span className="leading-144.5 h-6 py-0.5">new label</span>
                    </Label>
                </li>
            </ul>
        </div>
    )
}
