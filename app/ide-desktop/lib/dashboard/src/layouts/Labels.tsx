/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as backendHooks from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Label from '#/components/dashboard/Label'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import DragModal from '#/modals/DragModal'
import NewLabelModal from '#/modals/NewLabelModal'

import type Backend from '#/services/Backend'

import * as array from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import * as drag from '#/utilities/drag'

// ==============
// === Labels ===
// ==============

/** Props for a {@link Labels}. */
export interface LabelsProps {
  readonly backend: Backend
  readonly draggable: boolean
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
  const { backend, query, setQuery, draggable = true } = props
  const currentLabels = query.labels
  const currentNegativeLabels = query.negativeLabels
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const labels = backendHooks.useBackendListTags(backend) ?? []

  const deleteTagMutation = backendHooks.useBackendMutation(backend, 'deleteTag')

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          data-testid="labels"
          className="gap-sidebar-section-heading flex w-full flex-col items-start"
          {...innerProps}
        >
          <aria.Header
            id="header"
            className="text-header mb-sidebar-section-heading-b px-sidebar-section-heading-x text-sm font-bold"
          >
            {getText('labels')}
          </aria.Header>
          <div
            data-testid="labels-list"
            aria-label={getText('labelsListLabel')}
            className="flex flex-col items-start gap-labels"
          >
            {labels.map(label => {
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
                  {!label.isPlaceholder && (
                    <FocusRing placement="after">
                      <Button
                        active
                        image={Trash2Icon}
                        alt={getText('delete')}
                        className="relative flex size-icon text-delete transition-all transparent after:absolute after:-inset-1 after:rounded-button-focus-ring group-has-[[data-focus-visible]]:active group-hover:active"
                        onPress={() => {
                          setModal(
                            <ConfirmDeleteModal
                              actionText={getText('deleteLabelActionText', label.value)}
                              doDelete={() => {
                                deleteTagMutation.mutate([label.id, label.value])
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
            <ariaComponents.Button
              size="custom"
              variant="bar"
              className="px-2 selectable"
              onPress={event => {
                if (event.target instanceof HTMLElement) {
                  setModal(<NewLabelModal backend={backend} eventTarget={event.target} />)
                }
              }}
            >
              {/* This is a non-standard-sized icon. */}
              {/* eslint-disable-next-line no-restricted-syntax */}
              <img src={PlusIcon} className="mr-[6px] size-[6px]" />
              <aria.Text className="text-header">{getText('newLabelButtonLabel')}</aria.Text>
            </ariaComponents.Button>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
