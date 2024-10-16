/** @file A list of selectable labels. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import PlusIcon from '#/assets/plus.svg'
import Trash2Icon from '#/assets/trash2.svg'

import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Label from '#/components/dashboard/Label'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import DragModal from '#/modals/DragModal'
import NewLabelModal from '#/modals/NewLabelModal'

import type Backend from '#/services/Backend'

import AssetEventType from '#/events/AssetEventType'
import { useDispatchAssetEvent } from '#/layouts/AssetsTable/EventListProvider'
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
  const dispatchAssetEvent = useDispatchAssetEvent()
  const labels = useBackendQuery(backend, 'listTags', []).data ?? []
  const deleteTag = useMutation(
    backendMutationOptions(backend, 'deleteTag', {
      onSuccess: (_data, [, labelName]) => {
        dispatchAssetEvent({ type: AssetEventType.deleteLabel, labelName })
      },
    }),
  ).mutate

  return (
    <FocusArea direction="vertical">
      {(innerProps) => (
        <div data-testid="labels" className="flex flex-col items-start gap-4" {...innerProps}>
          <ariaComponents.Text variant="subtitle" className="px-2 font-bold">
            {getText('labels')}
          </ariaComponents.Text>
          <div
            data-testid="labels-list"
            aria-label={getText('labelsListLabel')}
            className="flex flex-col items-start gap-labels"
          >
            {labels.map((label) => {
              const negated = currentNegativeLabels.some((term) =>
                array.shallowEqual(term, [label.value]),
              )
              return (
                <div key={label.id} className="group relative flex items-center gap-label-icons">
                  <Label
                    draggable={draggable}
                    color={label.color}
                    active={
                      negated ||
                      currentLabels.some((term) => array.shallowEqual(term, [label.value]))
                    }
                    negated={negated}
                    onPress={(event) => {
                      setQuery((oldQuery) =>
                        oldQuery.withToggled(
                          'labels',
                          'negativeLabels',
                          label.value,
                          event.shiftKey,
                        ),
                      )
                    }}
                    onDragStart={(event) => {
                      drag.setDragImageToBlank(event)
                      const payload: drag.LabelsDragPayload = new Set([label.value])
                      drag.LABELS.bind(event, payload)
                      setModal(
                        <DragModal
                          event={event}
                          onDragEnd={() => {
                            drag.LABELS.unbind(payload)
                          }}
                        >
                          <Label active color={label.color} onPress={() => {}}>
                            {label.value}
                          </Label>
                        </DragModal>,
                      )
                    }}
                  >
                    {label.value}
                  </Label>
                  <FocusRing placement="after">
                    <ariaComponents.DialogTrigger>
                      <ariaComponents.Button
                        variant="icon"
                        icon={Trash2Icon}
                        aria-label={getText('delete')}
                        tooltipPlacement="right"
                        className="relative flex size-4 text-delete opacity-0 transition-all after:absolute after:-inset-1 after:rounded-button-focus-ring group-has-[[data-focus-visible]]:active group-hover:active"
                      />
                      <ConfirmDeleteModal
                        actionText={getText('deleteLabelActionText', label.value)}
                        doDelete={() => {
                          deleteTag([label.id, label.value])
                        }}
                      />
                    </ariaComponents.DialogTrigger>
                  </FocusRing>
                </div>
              )
            })}
            <ariaComponents.DialogTrigger>
              <ariaComponents.Button
                size="xsmall"
                variant="outline"
                className="pl-1 pr-2"
                /* eslint-disable-next-line no-restricted-syntax */
                icon={<SvgMask src={PlusIcon} alt="" className="ml-auto size-[8px]" />}
              >
                {getText('newLabelButtonLabel')}
              </ariaComponents.Button>
              <NewLabelModal backend={backend} />
            </ariaComponents.DialogTrigger>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
