/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import Label from '#/components/dashboard/Label'
import * as labelUtils from '#/components/dashboard/Label/labelUtils'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import DragModal from '#/modals/DragModal'
import NewLabelModal from '#/modals/NewLabelModal'

import type * as backend from '#/services/Backend'

import * as array from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import * as drag from '#/utilities/drag'
import * as string from '#/utilities/string'

// ==============
// === Labels ===
// ==============

/** Props for a {@link Labels}. */
export interface LabelsProps {
  readonly draggable: boolean
  readonly labels: backend.Label[]
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly doCreateLabel: (name: string, color: backend.LChColor) => void
  readonly doDeleteLabel: (id: backend.TagId, name: backend.LabelName) => void
  readonly newLabelNames: Set<backend.LabelName>
  readonly deletedLabelNames: Set<backend.LabelName>
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
  const {
    labels,
    query,
    setQuery,
    doCreateLabel,
    doDeleteLabel,
    newLabelNames,
    draggable = true,
    deletedLabelNames,
  } = props
  const currentLabels = query.labels
  const currentNegativeLabels = query.negativeLabels
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const displayLabels = React.useMemo(
    () =>
      labels
        .filter(label => !deletedLabelNames.has(label.value))
        .sort((a, b) => string.compareCaseInsensitive(a.value, b.value)),
    [deletedLabelNames, labels]
  )

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          data-testid="labels"
          className="gap-sidebar-section-heading flex w-full flex-col items-start"
          {...innerProps}
        >
          <div className="text-header px-sidebar-section-heading-x text-sm font-bold">
            {getText('labels')}
          </div>
          <div
            data-testid="labels-list"
            aria-label={getText('labelsListLabel')}
            className="flex flex-col items-start gap-labels"
          >
            {displayLabels.map(label => {
              const negated = currentNegativeLabels.some(term =>
                array.shallowEqual(term, [label.value])
              )
              return (
                <div key={label.id} className="group relative flex items-center gap-label-icons">
                  <Label
                    draggable={draggable}
                    color={label.color}
                    active={
                      negated || currentLabels.some(term => array.shallowEqual(term, [label.value]))
                    }
                    negated={negated}
                    isDisabled={newLabelNames.has(label.value)}
                    onPress={event => {
                      setQuery(oldQuery =>
                        oldQuery.withToggled(
                          'labels',
                          'negativeLabels',
                          label.value,
                          event.shiftKey
                        )
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
                          <Label active color={label.color} onPress={() => {}}>
                            {label.value}
                          </Label>
                        </DragModal>
                      )
                    }}
                  >
                    {label.value}
                  </Label>
                  {!newLabelNames.has(label.value) && (
                    <FocusRing placement="after">
                      <Button
                        active
                        image={Trash2Icon}
                        alt={getText('delete')}
                        className="relative flex size-icon text-delete transition-all transparent after:absolute after:inset-button-focus-ring-inset after:rounded-button-focus-ring group-has-[[data-focus-visible]]:active group-hover:active"
                        onPress={() => {
                          setModal(
                            <ConfirmDeleteModal
                              actionText={getText('deleteLabelActionText', label.value)}
                              doDelete={() => {
                                doDeleteLabel(label.id, label.value)
                              }}
                            />
                          )
                        }}
                      />
                    </FocusRing>
                  )}
                </div>
              )
            })}
            <Label
              color={labelUtils.DEFAULT_LABEL_COLOR}
              className="bg-selected-frame"
              onPress={event => {
                if (event.target instanceof HTMLElement) {
                  setModal(
                    <NewLabelModal
                      labels={labels}
                      eventTarget={event.target}
                      doCreate={doCreateLabel}
                    />
                  )
                }
              }}
            >
              {/* This is a non-standard-sized icon. */}
              {/* eslint-disable-next-line no-restricted-syntax */}
              <img src={PlusIcon} className="mr-[6px] size-[6px]" />
              <aria.Text className="text-header">{getText('newLabelButtonLabel')}</aria.Text>
            </Label>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
