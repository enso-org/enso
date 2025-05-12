/** @file A column listing the labels on this asset. */
import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import DotsIcon from '#/assets/dots.svg'
import ContextMenu from '#/components/ContextMenu'
import type * as column from '#/components/dashboard/column'
import Label from '#/components/dashboard/Label'

import { Button, DialogTrigger, Popover } from '#/components/AriaComponents'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import { useOnHover } from '#/hooks/hoverHooks'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { FALLBACK_COLOR } from '#/services/Backend'
import * as permissions from '#/utilities/permissions'
import { createRef } from 'react'

/** A column listing the labels on this asset. */
export default function LabelsColumn(props: column.AssetColumnProps) {
  const { item, state, labels } = props
  const { backend, category, setQuery } = state
  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()
  const labelsByName = new Map(labels.map((label) => [label.value, label]))
  const self = permissions.tryFindSelfPermission(user, item.permissions)
  const managesThisAsset =
    category.type !== 'trash' &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)

  const rootRef = createRef<HTMLDivElement>()
  const targetRef = createRef<HTMLDivElement>()
  const { shouldDisplay: shouldDisplayHoverContent, targetProps } = useOnHover({
    targetRef,
    display: 'whenOverflowing',
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
        onContextMenu={(event) => {
          event.preventDefault()
          event.stopPropagation()
          const doDelete = () => {
            unsetModal()
            const newLabels = item.labels?.filter((oldLabel) => oldLabel !== label) ?? []
            void backend.associateTag(item.id, newLabels, item.title)
          }
          setModal(
            <ContextMenu aria-label={getText('labelContextMenuLabel')} event={event}>
              <ContextMenuEntry
                action="delete"
                label={getText('deleteLabelShortcut')}
                doAction={doDelete}
              />
            </ContextMenu>,
          )
        }}
        onPress={(event) => {
          setQuery((oldQuery) =>
            oldQuery.withToggled('labels', 'negativeLabels', label, event.shiftKey),
          )
        }}
      >
        {label}
      </Label>
    ))

  return (
    <div ref={rootRef} className="group flex items-center gap-1" {...targetProps}>
      <div ref={targetRef} className="flex h-6 flex-wrap items-center gap-1 overflow-hidden">
        {labelsList}
      </div>
      {managesThisAsset && (
        <DialogTrigger>
          <Button
            variant="icon"
            showIconOnHover
            tooltip={getText('manageLabels')}
            tooltipPlacement="left"
            icon="edit"
          />
          <ManageLabelsModal backend={backend} item={item} />
        </DialogTrigger>
      )}
      {shouldDisplayHoverContent && (
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
              {managesThisAsset && (
                <DialogTrigger>
                  <Button
                    variant="icon"
                    tooltip={getText('manageLabels')}
                    tooltipPlacement="left"
                    icon="edit"
                  />
                  <ManageLabelsModal backend={backend} item={item} />
                </DialogTrigger>
              )}
            </div>
          </Popover>
        </Popover.Trigger>
      )}
    </div>
  )
}
