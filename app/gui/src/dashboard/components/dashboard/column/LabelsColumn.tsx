/** @file A column listing the labels on this asset. */
import * as React from 'react'

import Plus2Icon from '#/assets/plus2.svg'

import * as backendHooks from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import { Button, DialogTrigger } from '#/components/AriaComponents'
import ContextMenu from '#/components/ContextMenu'
import ContextMenus from '#/components/ContextMenus'
import type * as column from '#/components/dashboard/column'
import Label from '#/components/dashboard/Label'
import MenuEntry from '#/components/MenuEntry'

import ManageLabelsModal from '#/modals/ManageLabelsModal'

import * as backendModule from '#/services/Backend'

import * as permissions from '#/utilities/permissions'

// ====================
// === LabelsColumn ===
// ====================

/** A column listing the labels on this asset. */
export default function LabelsColumn(props: column.AssetColumnProps) {
  const { item, state, rowState } = props
  const { backend, category, setQuery } = state
  const { temporarilyAddedLabels, temporarilyRemovedLabels } = rowState
  const { user } = authProvider.useFullUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const { data: labels } = backendHooks.useBackendQuery(backend, 'listTags', [])
  const labelsByName = React.useMemo(() => {
    return new Map(labels?.map((label) => [label.value, label]))
  }, [labels])
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
            active={!temporarilyRemovedLabels.has(label)}
            isDisabled={temporarilyRemovedLabels.has(label)}
            negated={temporarilyRemovedLabels.has(label)}
            onContextMenu={(event) => {
              event.preventDefault()
              event.stopPropagation()
              const doDelete = () => {
                unsetModal()
                const newLabels = item.labels?.filter((oldLabel) => oldLabel !== label) ?? []
                void backend.associateTag(item.id, newLabels, item.title)
              }
              setModal(
                <ContextMenus key={`label-${label}`} event={event}>
                  <ContextMenu aria-label={getText('labelContextMenuLabel')}>
                    <MenuEntry
                      action="delete"
                      label={getText('deleteLabelShortcut')}
                      doAction={doDelete}
                    />
                  </ContextMenu>
                </ContextMenus>,
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
      {...[...temporarilyAddedLabels]
        .filter((label) => item.labels?.includes(label) !== true)
        .map((label) => (
          <Label
            isDisabled
            key={label}
            color={labelsByName.get(label)?.color ?? backendModule.COLORS[0]}
            onPress={() => {}}
          >
            {label}
          </Label>
        ))}
      {managesThisAsset && (
        <DialogTrigger>
          <Button variant="ghost" showIconOnHover icon={Plus2Icon} />
          <ManageLabelsModal backend={backend} item={item} />
        </DialogTrigger>
      )}
    </div>
  )
}
