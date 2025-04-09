/** @file A list of selectable labels. */
import PlusIcon from '#/assets/plus.svg'
import Trash2Icon from '#/assets/trash2.svg'
import { Button, DialogTrigger, Text } from '#/components/AriaComponents'
import Label from '#/components/dashboard/Label'
import { Scroller } from '#/components/Scroller'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import DragModal from '#/modals/DragModal'
import NewLabelModal from '#/modals/NewLabelModal'
import { useDriveStore, useSetLabelsDragPayload } from '#/providers/DriveProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { shallowEqual } from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import { setDragImageToBlank } from '#/utilities/drag'
import { useMutation } from '@tanstack/react-query'
import type { Dispatch, SetStateAction } from 'react'

/** Props for a {@link Labels}. */
export interface LabelsProps {
  readonly backend: Backend
  readonly draggable: boolean
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
  const { backend, query, setQuery, draggable = true } = props
  const currentLabels = query.labels
  const currentNegativeLabels = query.negativeLabels
  const { setModal } = useSetModal()
  const { getText } = useText()
  const driveStore = useDriveStore()
  const getAsset = useGetAsset()
  const setLabelsDragPayload = useSetLabelsDragPayload()
  const labels = useBackendQuery(backend, 'listTags', []).data ?? []
  const deleteTagMutation = useMutation(backendMutationOptions(backend, 'deleteTag'))

  return (
    <FocusArea direction="vertical">
      {(innerProps) => (
        <div className="flex flex-none flex-col">
          <section data-testid="labels" className="flex flex-col items-start gap-2" {...innerProps}>
            <Text variant="subtitle" elementType="h3" weight="semibold">
              {getText('labels')}
            </Text>

            <Scroller testId="labels-list" className="max-h-48" orientation="vertical">
              <div className="flex flex-col items-start gap-1">
                {labels.map((label) => {
                  const negated = currentNegativeLabels.some((term) =>
                    shallowEqual(term, [label.value]),
                  )
                  return (
                    <div
                      key={label.id}
                      className="group relative flex items-center gap-label-icons"
                    >
                      <Label
                        draggable={draggable}
                        color={label.color}
                        active={
                          negated || currentLabels.some((term) => shallowEqual(term, [label.value]))
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
                          const { selectedIds } = driveStore.getState()
                          const selectedAssets = [...selectedIds].flatMap((id) => {
                            const otherAsset = getAsset(id)
                            return otherAsset ? [otherAsset] : []
                          })
                          setDragImageToBlank(event)
                          let count = 0
                          for (const asset of selectedAssets) {
                            if (asset.labels?.includes(label.value) === true) {
                              count += 1
                            }
                          }
                          setLabelsDragPayload({
                            typeWhenAppliedToSelection:
                              count * 2 < selectedAssets.length ? 'add' : 'remove',
                            labels: [label.value],
                          })
                          setModal(
                            <DragModal
                              hideBadge
                              event={event}
                              className="w-0"
                              onDragEnd={() => {
                                setLabelsDragPayload(null)
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
                        <DialogTrigger>
                          <Button
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
                        </DialogTrigger>
                      </FocusRing>
                    </div>
                  )
                })}
              </div>
            </Scroller>
          </section>

          <DialogTrigger>
            <Button size="xsmall" variant="outline" className="mt-3 self-start" icon={PlusIcon}>
              {getText('newLabelButtonLabel')}
            </Button>

            <NewLabelModal backend={backend} />
          </DialogTrigger>
        </div>
      )}
    </FocusArea>
  )
}
