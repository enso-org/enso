/** @file Components for column cells. */
import DotsIcon from '#/assets/dots.svg'
import { Button } from '#/components/Button'
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import { Dialog, Popover } from '#/components/Dialog'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMeasureCallback } from '#/hooks/measureHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import DatalinkNameColumn from '#/pages/dashboard/components/column/DatalinkNameColumn'
import DirectoryNameColumn from '#/pages/dashboard/components/column/DirectoryNameColumn'
import FileNameColumn from '#/pages/dashboard/components/column/FileNameColumn'
import ProjectNameColumn from '#/pages/dashboard/components/column/ProjectNameColumn'
import SecretNameColumn from '#/pages/dashboard/components/column/SecretNameColumn'
import Label from '#/pages/dashboard/components/Label'
import PermissionDisplay from '#/pages/dashboard/components/PermissionDisplay'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import {
  AssetType,
  FALLBACK_COLOR,
  getAssetPermissionId,
  getAssetPermissionName,
} from '#/services/Backend'
import { mergeRefs } from '#/utilities/mergeRefs'
import { PermissionAction } from '#/utilities/permissions'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import { useRef, useState } from 'react'
export { PathColumn } from './PathColumn'

/** A column listing the labels on this asset. */
export function LabelsColumn(props: AssetColumnProps) {
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

/** A column displaying the time at which the asset was last modified. */
export function ModifiedColumn(props: AssetColumnProps) {
  const { item } = props

  return <Text nowrap>{toReadableIsoString(new Date(item.modifiedAt))}</Text>
}

/** The icon and name of an {@link backendModule.Asset}. */
export function NameColumn(props: AssetColumnProps) {
  const { item } = props

  switch (item.type) {
    case AssetType.directory: {
      return <DirectoryNameColumn {...props} item={item} />
    }
    case AssetType.project: {
      return <ProjectNameColumn {...props} item={item} />
    }
    case AssetType.file: {
      return <FileNameColumn {...props} item={item} />
    }
    case AssetType.datalink: {
      return <DatalinkNameColumn {...props} item={item} />
    }
    case AssetType.secret: {
      return <SecretNameColumn {...props} item={item} />
    }
    case AssetType.specialUp: {
      // Special rows do not display columns at all.
      return <></>
    }
  }
}

/** A placeholder component for columns which do not yet have corresponding data to display. */
export function PlaceholderColumn() {
  return <></>
}

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp
  extends Pick<AssetColumnProps['state'], 'backend' | 'category'> {
  readonly setQuery: AssetColumnProps['state']['setQuery'] | null
}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<AssetColumnProps, 'item'> {
  readonly isReadonly?: boolean
  readonly state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
export function SharedWithColumn(props: SharedWithColumnPropsInternal) {
  const { item, state } = props
  const { category, setQuery } = state

  const assetPermissions = item.permissions ?? []

  return (
    <div className="group flex items-center gap-1">
      {(category.type === 'trash' ?
        assetPermissions.filter((permission) => permission.permission === PermissionAction.own)
      : assetPermissions
      ).map((other, idx) => (
        <PermissionDisplay
          key={getAssetPermissionId(other) + idx}
          action={other.permission}
          onPress={
            setQuery == null ? null : (
              (event) => {
                setQuery((oldQuery) =>
                  oldQuery.withToggled(
                    'owners',
                    'negativeOwners',
                    getAssetPermissionName(other),
                    event.shiftKey,
                  ),
                )
              }
            )
          }
        >
          {getAssetPermissionName(other)}
        </PermissionDisplay>
      ))}
    </div>
  )
}
