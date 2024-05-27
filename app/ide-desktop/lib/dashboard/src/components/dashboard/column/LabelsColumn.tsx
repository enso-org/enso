/** @file A column listing the labels on this asset. */
import * as React from 'react'

import Plus2Icon from 'enso-assets/plus2.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Category from '#/layouts/CategorySwitcher/Category'

import ContextMenu from '#/components/ContextMenu'
import ContextMenus from '#/components/ContextMenus'
import type * as column from '#/components/dashboard/column'
import Label from '#/components/dashboard/Label'
import * as labelUtils from '#/components/dashboard/Label/labelUtils'
import MenuEntry from '#/components/MenuEntry'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

import ManageLabelsModal from '#/modals/ManageLabelsModal'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
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
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useStrictBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const plusButtonRef = React.useRef<HTMLButtonElement>(null)
  const self = asset.permissions?.find(
    backendModule.isUserPermissionAnd(permission => permission.user.userId === user?.userId)
  )
  const managesThisAsset =
    category !== Category.trash &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)
  const setAsset = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
      setItem(oldItem =>
        oldItem.with({
          item:
            typeof valueOrUpdater !== 'function' ? valueOrUpdater : valueOrUpdater(oldItem.item),
        })
      )
    },
    [setItem]
  )

  return (
    <div className="group flex items-center gap-column-items">
      {(asset.labels ?? [])
        .filter(label => !deletedLabelNames.has(label))
        .map(label => (
          <Label
            key={label}
            data-testid="asset-label"
            title={getText('rightClickToRemoveLabel')}
            color={labels.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
            active={!temporarilyRemovedLabels.has(label)}
            isDisabled={temporarilyRemovedLabels.has(label)}
            negated={temporarilyRemovedLabels.has(label)}
            className={
              temporarilyRemovedLabels.has(label)
                ? 'relative before:absolute before:inset before:h-full before:w-full before:rounded-full before:border-2 before:border-delete'
                : ''
            }
            onContextMenu={event => {
              event.preventDefault()
              event.stopPropagation()
              const doDelete = () => {
                unsetModal()
                setAsset(oldAsset => {
                  const newLabels = oldAsset.labels?.filter(oldLabel => oldLabel !== label) ?? []
                  void backend
                    .associateTag(asset.id, newLabels, asset.title)
                    .catch((error: unknown) => {
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
                  <ContextMenu aria-label={getText('labelContextMenuLabel')}>
                    <MenuEntry
                      action="delete"
                      label={getText('deleteLabelShortcut')}
                      doAction={doDelete}
                    />
                  </ContextMenu>
                </ContextMenus>
              )
            }}
            onPress={event => {
              setQuery(oldQuery =>
                oldQuery.withToggled('labels', 'negativeLabels', label, event.shiftKey)
              )
            }}
          >
            {label}
          </Label>
        ))}
      {...[...temporarilyAddedLabels]
        .filter(label => asset.labels?.includes(label) !== true)
        .map(label => (
          <Label
            isDisabled
            key={label}
            color={labels.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
            className="pointer-events-none"
            onPress={() => {}}
          >
            {label}
          </Label>
        ))}
      {managesThisAsset && (
        <div className="relative inline-flex shrink-0 transition-all transparent before:absolute before:inset-0 before:rounded-full before:border before:border-primary/10 before:transition-all before:hover:bg-primary/5 group-hover:opacity-100">
          <UnstyledButton
            ref={plusButtonRef}
            className="relative inline-flex min-w-max rounded-full transition-all selectable focus-visible:opacity-100"
            onPress={() => {
              setModal(
                <ManageLabelsModal
                  key={uniqueString.uniqueString()}
                  item={asset}
                  setItem={setAsset}
                  allLabels={labels}
                  doCreateLabel={doCreateLabel}
                  eventTarget={plusButtonRef.current}
                />
              )
            }}
          >
            <SvgMask className="size-icon" src={Plus2Icon} />
          </UnstyledButton>
        </div>
      )}
    </div>
  )
}
