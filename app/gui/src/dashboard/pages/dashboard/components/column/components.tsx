/** @file Components for column cells. */
import DotsIcon from '#/assets/dots.svg'
import { Button } from '#/components/Button'
import { Dialog, Popover } from '#/components/Dialog'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMeasureCallback } from '#/hooks/measureHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import type { AssetColumnProps, AssetNameColumnProps } from '#/pages/dashboard/components/column'
import DatalinkNameColumn from '#/pages/dashboard/components/column/DatalinkNameColumn'
import DirectoryNameColumn from '#/pages/dashboard/components/column/DirectoryNameColumn'
import FileNameColumn from '#/pages/dashboard/components/column/FileNameColumn'
import ProjectNameColumn from '#/pages/dashboard/components/column/ProjectNameColumn'
import SecretNameColumn from '#/pages/dashboard/components/column/SecretNameColumn'
import Label from '#/pages/dashboard/components/Label'
import PermissionDisplay from '#/pages/dashboard/components/PermissionDisplay'
import { useSelectedAssets } from '#/providers/DriveProvider'
import { unsetModal } from '#/providers/ModalProvider'
import { mergeRefs } from '#/utilities/mergeRefs'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import {
  AssetType,
  FALLBACK_COLOR,
  getAssetPermissionId,
  getAssetPermissionName,
  type LabelName,
  type LChColor,
} from 'enso-common/src/services/Backend'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import { PermissionAction } from 'enso-common/src/utilities/permissions'
import { useMemo, useRef, useState } from 'react'
export { PathColumn } from './PathColumn'

/** A column listing the labels on this asset. */
export function LabelsColumn(props: AssetColumnProps) {
  const { item, labels } = props

  const { associatedBackend: backend } = useCategoriesAPI()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const labelsByName = new Map(labels.map((label) => [label.value, label]))
  const selectedAssets = useSelectedAssets()
  const labelsItems = useMemo(
    () => (selectedAssets.some((asset) => asset.id === item.id) ? selectedAssets : [item]),
    [selectedAssets, item],
  )

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
      <LabelInColumn
        key={label}
        label={label}
        color={labelsByName.get(label)?.color}
        doDelete={doDelete}
      />
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
                  <ManageLabelsModal backend={backend} items={labelsItems} />
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
          <ManageLabelsModal backend={backend} items={labelsItems} />
        </Dialog.Trigger>
      </div>
    </div>
  )
}

/** Props for a {@link LabelInColumn}. */
interface LabelInColumnProps {
  readonly label: LabelName
  readonly color: LChColor | undefined
  readonly doDelete: (label: LabelName) => void
}

/** A {@link Label} in a {@link LabelsColumn}. */
function LabelInColumn(props: LabelInColumnProps) {
  const { label, color = FALLBACK_COLOR, doDelete } = props

  const { getText } = useText()

  return (
    <Label
      active
      data-testid="asset-label"
      title={getText('rightClickToRemoveLabel')}
      color={color}
      onDelete={() => doDelete(label)}
    >
      {label}
    </Label>
  )
}

/** A column displaying the time at which the asset was last modified. */
export function ModifiedColumn(props: AssetColumnProps) {
  const { item } = props

  return <Text nowrap>{toReadableIsoString(new Date(item.modifiedAt))}</Text>
}

/** The icon and name of an {@link backendModule.Asset}. */
export function NameColumn(props: AssetNameColumnProps) {
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
  }
}

/** A placeholder component for columns which do not yet have corresponding data to display. */
export function PlaceholderColumn() {
  return <></>
}

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp extends Pick<AssetColumnProps['state'], 'category'> {}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<AssetColumnProps, 'item'> {
  readonly state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
export function SharedWithColumn(props: SharedWithColumnPropsInternal) {
  const { item, state } = props
  const { category } = state

  const assetPermissions = item.permissions ?? []

  return (
    <div className="group flex items-center gap-1">
      {(category.type === 'trash' ?
        assetPermissions.filter((permission) => permission.permission === PermissionAction.own)
      : assetPermissions
      ).map((other, idx) => (
        <PermissionDisplay key={getAssetPermissionId(other) + idx} action={other.permission}>
          {getAssetPermissionName(other)}
        </PermissionDisplay>
      ))}
    </div>
  )
}
