/** @file A column listing the labels on this asset. */
import * as React from 'react'

import Plus2Icon from 'enso-assets/plus2.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Category from '#/layouts/CategorySwitcher/Category'

import * as ariaComponents from '#/components/AriaComponents'
import ContextMenu from '#/components/ContextMenu'
import ContextMenus from '#/components/ContextMenus'
import type * as column from '#/components/dashboard/column'
import Label from '#/components/dashboard/Label'
import * as labelUtils from '#/components/dashboard/Label/labelUtils'
import MenuEntry from '#/components/MenuEntry'

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
  const { backend, category, setQuery } = state
  const { temporarilyAddedLabels, temporarilyRemovedLabels } = rowState
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const labels = backendHooks.useBackendListTags(backend)
  const labelsByName = React.useMemo(() => {
    return new Map(labels?.map(label => [label.value, label]))
  }, [labels])
  const plusButtonRef = React.useRef<HTMLButtonElement>(null)
  const self = item.permissions?.find(
    backendModule.isUserPermissionAnd(permission => permission.user.userId === user?.userId)
  )
  const managesThisAsset =
    category !== Category.trash &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)

  const associateTagMutation = backendHooks.useBackendMutation(backend, 'associateTag')

  return (
    <div className="group flex items-center gap-column-items">
      {(item.labels ?? [])
        .filter(label => labelsByName.has(label))
        .map(label => (
          <Label
            key={label}
            data-testid="asset-label"
            title={getText('rightClickToRemoveLabel')}
            color={labelsByName.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
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
                const newLabels = item.labels?.filter(oldLabel => oldLabel !== label) ?? []
                associateTagMutation.mutate([item.id, newLabels, item.title])
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
        .filter(label => item.labels?.includes(label) !== true)
        .map(label => (
          <Label
            isDisabled
            key={label}
            color={labelsByName.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
            className="pointer-events-none"
            onPress={() => {}}
          >
            {label}
          </Label>
        ))}
      {managesThisAsset && (
        <ariaComponents.Button
          size="custom"
          variant="custom"
          ref={plusButtonRef}
          className="shrink-0 rounded-full opacity-0 group-hover:opacity-100 focus-visible:opacity-100"
          onPress={() => {
            setModal(
              <ManageLabelsModal
                key={uniqueString.uniqueString()}
                backend={backend}
                item={item}
                setItem={setItem}
                eventTarget={plusButtonRef.current}
              />
            )
          }}
        >
          <img className="size-plus-icon" src={Plus2Icon} />
        </ariaComponents.Button>
      )}
    </div>
  )
}
