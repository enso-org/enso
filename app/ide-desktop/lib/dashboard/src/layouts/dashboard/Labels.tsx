/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import NewLabelModal from '#/layouts/dashboard/NewLabelModal'
import * as modalProvider from '#/providers/ModalProvider'
import type * as backend from '#/services/backend'
import * as array from '#/utilities/array'
import * as assetQuery from '#/utilities/assetQuery'
import * as drag from '#/utilities/drag'

import ConfirmDeleteModal from '#/components/dashboard/ConfirmDeleteModal'
import Label from '#/components/dashboard/Label'
import * as labelUtils from '#/components/dashboard/Label/labelUtils'
import DragModal from '#/components/DragModal'
import SvgMask from '#/components/SvgMask'

// ==============
// === Labels ===
// ==============

/** Props for a {@link Labels}. */
export interface LabelsProps {
  labels: backend.Label[]
  query: assetQuery.AssetQuery
  setQuery: React.Dispatch<React.SetStateAction<assetQuery.AssetQuery>>
  doCreateLabel: (name: string, color: backend.LChColor) => void
  doDeleteLabel: (id: backend.TagId, name: backend.LabelName) => void
  newLabelNames: Set<backend.LabelName>
  deletedLabelNames: Set<backend.LabelName>
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
  const { labels, query, setQuery, doCreateLabel, doDeleteLabel, newLabelNames } = props
  const { deletedLabelNames } = props
  const currentLabels = query.labels
  const currentNegativeLabels = query.negativeLabels
  const { setModal } = modalProvider.useSetModal()

  return (
    <div data-testid="labels" className="flex flex-col items-start w-30">
      <div className="pl-2 pb-1.5">
        <span className="inline-block font-bold text-sm leading-144.5 h-6 py-0.5">Labels</span>
      </div>
      <ul data-testid="labels-list" className="flex flex-col items-start gap-1">
        {labels
          .filter(label => !deletedLabelNames.has(label.value))
          .sort((a, b) => (a.value > b.value ? 1 : a.value < b.value ? -1 : 0))
          .map(label => {
            const negated = currentNegativeLabels.some(term =>
              array.shallowEqual(term, [label.value])
            )
            return (
              <li key={label.id} className="group flex items-center gap-1">
                <Label
                  draggable
                  color={label.color}
                  active={
                    negated || currentLabels.some(term => array.shallowEqual(term, [label.value]))
                  }
                  negated={negated}
                  disabled={newLabelNames.has(label.value)}
                  onClick={event => {
                    setQuery(oldQuery =>
                      assetQuery.toggleLabel(oldQuery, label.value, event.shiftKey)
                    )
                  }}
                  onDragStart={event => {
                    drag.setDragImageToBlank(event)
                    const payload: drag.LabelsDragPayload = new Set([label.value])
                    drag.LABELS.bind(event, payload)
                    setModal(
                      <DragModal
                        event={event}
                        doCleanup={() => {
                          drag.LABELS.unbind(payload)
                        }}
                      >
                        <Label active color={label.color} onClick={() => {}}>
                          {label.value}
                        </Label>
                      </DragModal>
                    )
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
            )
          })}
        <li>
          <Label
            active
            color={labelUtils.DEFAULT_LABEL_COLOR}
            className="bg-frame text-not-selected"
            onClick={event => {
              event.stopPropagation()
              setModal(
                <NewLabelModal
                  labels={labels}
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
