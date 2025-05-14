/** @file A column listing the labels on this asset. */

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import { Button, DialogTrigger } from '#/components/AriaComponents'
import ContextMenu from '#/components/ContextMenu'
import type * as column from '#/components/dashboard/column'
import Label from '#/components/dashboard/Label'

import ManageLabelsModal from '#/modals/ManageLabelsModal'

import * as backendModule from '#/services/Backend'

import ContextMenuEntry from '#/components/ContextMenuEntry'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import * as permissions from '#/utilities/permissions'

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

  return (
    <div className="group flex items-center gap-column-items">
      {(item.labels ?? [])
        .filter((label) => labelsByName.has(label))
        .map((label) => (
          <Label
            key={label}
            data-testid="asset-label"
            title={getText('rightClickToRemoveLabel')}
            color={labelsByName.get(label)?.color ?? backendModule.COLORS[0]}
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
        ))}
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
    </div>
  )
}
