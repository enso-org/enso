/** @file A column listing the labels on this asset. */
import * as React from 'react'

import Plus2Icon from 'enso-assets/plus2.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import ManageLabelsModal from '#/layouts/dashboard/ManageLabelsModal'

import ContextMenu from '#/components/ContextMenu'
import ContextMenus from '#/components/ContextMenus'
import type * as column from '#/components/dashboard/column'
import Label from '#/components/dashboard/Label'
import * as labelUtils from '#/components/dashboard/Label/labelUtils'
import MenuEntry from '#/components/MenuEntry'

import type * as backendModule from '#/services/Backend'

import * as assetQuery from '#/utilities/AssetQuery'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as shortcutManager from '#/utilities/ShortcutManager'
import * as uniqueString from '#/utilities/uniqueString'

// ====================
// === LabelsColumn ===
// ====================

/** A column listing the labels on this asset. */
export default function LabelsColumn(props: column.AssetColumnProps) {
  const { item, setItem, state, rowState } = props
  const { category, labels, setQuery, deletedLabelNames, doCreateLabel } = state
  const { temporarilyAddedLabels, temporarilyRemovedLabels } = rowState
  const asset = item.item
  const session = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const self = asset.permissions?.find(
    permission => permission.user.user_email === session.organization?.email
  )
  const managesThisAsset =
    category !== Category.trash &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)
  const setAsset = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
      setItem(oldItem =>
        object.merge(oldItem, {
          item:
            typeof valueOrUpdater !== 'function' ? valueOrUpdater : valueOrUpdater(oldItem.item),
        })
      )
    },
    [/* should never change */ setItem]
  )
  return (
    <div className="group flex items-center gap-1">
      {(asset.labels ?? [])
        .filter(label => !deletedLabelNames.has(label))
        .map(label => (
          <Label
            key={label}
            data-testid="asset-label"
            title="Right click to remove label."
            color={labels.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
            active={!temporarilyRemovedLabels.has(label)}
            disabled={temporarilyRemovedLabels.has(label)}
            negated={temporarilyRemovedLabels.has(label)}
            className={
              temporarilyRemovedLabels.has(label)
                ? 'relative before:absolute before:rounded-full before:border-2 before:border-delete before:inset-0 before:w-full before:h-full'
                : ''
            }
            onContextMenu={event => {
              event.preventDefault()
              event.stopPropagation()
              const doDelete = () => {
                unsetModal()
                setAsset(oldAsset => {
                  const newLabels = oldAsset.labels?.filter(oldLabel => oldLabel !== label) ?? []
                  void backend.associateTag(asset.id, newLabels, asset.title).catch(error => {
                    toastAndLog(null, error)
                    setAsset(oldAsset2 =>
                      oldAsset2.labels?.some(oldLabel => oldLabel === label) === true
                        ? oldAsset2
                        : object.merge(oldAsset2, {
                            labels: [...(oldAsset2.labels ?? []), label],
                          })
                    )
                  })
                  return object.merge(oldAsset, { labels: newLabels })
                })
              }
              setModal(
                <ContextMenus key={`label-${label}`} event={event}>
                  <ContextMenu>
                    <MenuEntry action={shortcutManager.KeyboardAction.delete} doAction={doDelete} />
                  </ContextMenu>
                </ContextMenus>
              )
            }}
            onClick={event => {
              event.preventDefault()
              event.stopPropagation()
              setQuery(oldQuery => assetQuery.toggleLabel(oldQuery, label, event.shiftKey))
            }}
          >
            {label}
          </Label>
        ))}
      {...[...temporarilyAddedLabels]
        .filter(label => asset.labels?.includes(label) !== true)
        .map(label => (
          <Label
            disabled
            key={label}
            color={labels.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
            className="pointer-events-none"
            onClick={() => {}}
          >
            {label}
          </Label>
        ))}
      {managesThisAsset && (
        <button
          className="h-4 w-4 invisible group-hover:visible"
          onClick={event => {
            event.stopPropagation()
            setModal(
              <ManageLabelsModal
                key={uniqueString.uniqueString()}
                item={asset}
                setItem={setAsset}
                allLabels={labels}
                doCreateLabel={doCreateLabel}
                eventTarget={event.currentTarget}
              />
            )
          }}
        >
          <img className="w-4.5 h-4.5" src={Plus2Icon} />
        </button>
      )}
    </div>
  )
}
