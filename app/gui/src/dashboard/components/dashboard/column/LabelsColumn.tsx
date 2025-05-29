/** @file A column listing the labels on this asset. */
import DotsIcon from '#/assets/dots.svg'
import { Button } from '#/components/Button'
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import type * as column from '#/components/dashboard/column'
import Label from '#/components/dashboard/Label'
import { Dialog, Popover } from '#/components/Dialog'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMeasureCallback } from '#/hooks/measureHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { FALLBACK_COLOR } from '#/services/Backend'
import { mergeRefs } from '#/utilities/mergeRefs'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { useRef, useState } from 'react'

/** A column listing the labels on this asset. */
export default function LabelsColumn(props: column.AssetColumnProps) {
  const { item, state, labels } = props

  const { backend } = state

  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const labelsByName = new Map(labels.map((label) => [label.value, label]))

  const rootRef = useRef<HTMLDivElement>(null)
  const labelsListRef = useRef<HTMLDivElement>(null)
  const [isOverflowing, setIsOverflowing] = useState(false)
  const [measureRef] = useMeasureCallback({
    onResize: () => {
      const el = labelsListRef.current
      if (!el) {
        return
      }
      setIsOverflowing(el.scrollWidth > el.clientWidth)
    },
  })

  const associateTag = useMutationCallback(backendMutationOptions(backend, 'associateTag'))

  const doDelete = useEventCallback(async (label: string) => {
    unsetModal()
    const newLabels = item.labels?.filter((oldLabel) => oldLabel !== label) ?? []

    return associateTag([item.id, newLabels, item.title]).catch((error) => {
      toastAndLog('deleteLabelBackendError', error, label)
    })
  })

  const labelsList = (item.labels ?? [])
    .filter((label) => labelsByName.has(label))
    .map((label) => (
      <Label
        key={label}
        data-testid="asset-label"
        title={getText('rightClickToRemoveLabel')}
        color={labelsByName.get(label)?.color ?? FALLBACK_COLOR}
        active
        onDelete={() => doDelete(label)}
        onContextMenu={(event) => {
          event.preventDefault()
          event.stopPropagation()
          setModal(
            <ContextMenu aria-label={getText('labelContextMenuLabel')} event={event}>
              <ContextMenuEntry
                action="delete"
                label={getText('removeLabelShortcut')}
                doAction={() => {
                  unsetModal()
                  void doDelete(label)
                }}
              />
            </ContextMenu>,
          )
        }}
      >
        {label}
      </Label>
    ))

  return (
    <div ref={rootRef} className="group relative flex items-center gap-1">
      <div
        ref={(el) => {
          mergeRefs(measureRef, labelsListRef)(el)
        }}
        className="flex h-6 items-center gap-1 overflow-hidden"
      >
        {labelsList}
        {isOverflowing && (
          <div className="pointer-events-none absolute bottom-0 right-10 top-0 w-10 bg-gradient-to-l from-dashboard-row opacity-100" />
        )}
      </div>
      <div
        className="contents"
        onClick={(event) => {
          // Prevent the click from being propagated to the parent and trigger the row selection.
          event.stopPropagation()
        }}
      >
        {isOverflowing && (
          <Popover.Trigger>
            <Button
              variant="icon"
              showIconOnHover
              icon={DotsIcon}
              tooltip={getText('showAllLabels')}
            />
            <Popover
              triggerRef={rootRef}
              size="auto"
              style={() => ({ width: rootRef.current?.clientWidth })}
            >
              <div className="flex flex-wrap items-center gap-1">
                {labelsList}

                <Dialog.Trigger>
                  <Button
                    variant="icon"
                    tooltip={getText('manageLabels')}
                    tooltipPlacement="top"
                    icon="edit"
                  />
                  <ManageLabelsModal backend={backend} item={item} />
                </Dialog.Trigger>
              </div>
            </Popover>
          </Popover.Trigger>
        )}

        <Dialog.Trigger>
          <Button
            variant="icon"
            showIconOnHover
            tooltip={getText('manageLabels')}
            tooltipPlacement="top"
            icon="edit"
          />
          <ManageLabelsModal backend={backend} item={item} />
        </Dialog.Trigger>
      </div>
    </div>
  )
}
