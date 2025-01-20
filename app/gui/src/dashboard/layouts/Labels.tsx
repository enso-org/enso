/** @file A list of selectable labels. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import PlusIcon from '#/assets/plus.svg'
import Trash2Icon from '#/assets/trash2.svg'
import * as ariaComponents from '#/components/AriaComponents'
import Label from '#/components/dashboard/Label'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import DragModal from '#/modals/DragModal'
import NewLabelModal from '#/modals/NewLabelModal'
import { useDriveStore, useNodeMap, useSetLabelsDragPayload } from '#/providers/DriveProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as array from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import * as drag from '#/utilities/drag'

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
  const driveStore = useDriveStore()
  const nodeMapRef = useNodeMap()
  const setLabelsDragPayload = useSetLabelsDragPayload()
  const labels = useBackendQuery(backend, 'listTags', []).data ?? []
  const deleteTagMutation = useMutation(backendMutationOptions(backend, 'deleteTag'))

  return (
    <FocusArea direction="vertical">
      {(innerProps) => (
        <div className="flex flex-none flex-col overflow-auto">
          <div
            data-testid="labels"
            className="flex flex-col items-start gap-4 overflow-auto"
            {...innerProps}
          >
            <ariaComponents.Text variant="subtitle" className="px-2 font-bold">
              {getText('labels')}
            </ariaComponents.Text>
            <div
              data-testid="labels-list"
              aria-label={getText('labelsListLabel')}
              className="flex flex-1 flex-col items-start gap-labels overflow-auto"
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
                        const { selectedKeys } = driveStore.getState()
                        const selectedAssets = [...selectedKeys].flatMap((id) => {
                          const item = nodeMapRef.current.get(id)?.item
                          return item ? [item] : []
                        })
                        drag.setDragImageToBlank(event)
                        const payloadLabels = [label.value]
                        const payload: drag.LabelsDragPayload = new Set(payloadLabels)
                        drag.LABELS.bind(event, payload)
                        let count = 0
                        for (const asset of selectedAssets) {
                          if (asset.labels?.includes(label.value) === true) {
                            count += 1
                          }
                        }
                        setLabelsDragPayload({
                          typeWhenAppliedToSelection:
                            count * 2 < selectedAssets.length ? 'add' : 'remove',
                          labels: payloadLabels,
                        })
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
                          extraClickZone={false}
                          aria-label={getText('delete')}
                          tooltipPlacement="right"
                          className="relative flex size-4 text-delete opacity-0 transition-all after:absolute after:-inset-1 after:rounded-button-focus-ring group-has-[[data-focus-visible]]:active group-hover:active"
                        />
                        <ConfirmDeleteModal
                          actionText={getText('deleteLabelActionText', label.value)}
                          doDelete={async () => {
                            await deleteTagMutation.mutateAsync([label.id, label.value])
                          }}
                        />
                      </ariaComponents.DialogTrigger>
                    </FocusRing>
                  </div>
                )
              })}
            </div>
          </div>
          <ariaComponents.DialogTrigger>
            <ariaComponents.Button
              size="xsmall"
              variant="outline"
              className="mt-1 self-start pl-1 pr-2"
              icon={PlusIcon}
            >
              {getText('newLabelButtonLabel')}
            </ariaComponents.Button>
            <NewLabelModal backend={backend} />
          </ariaComponents.DialogTrigger>
        </div>
      )}
    </FocusArea>
  )
}
