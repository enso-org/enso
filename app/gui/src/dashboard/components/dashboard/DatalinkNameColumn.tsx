/** @file The icon and name of a {@link SecretAsset}. */
import DatalinkIcon from '#/assets/datalink.svg'
import type { AssetColumnProps } from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import { useSetIsAssetPanelTemporarilyVisible } from '#/layouts/AssetPanel'
import type { DatalinkAsset } from '#/services/Backend'
import { isDoubleClick } from '#/utilities/event'
import { merger } from '#/utilities/object'

/** Props for a {@link DatalinkNameColumn}. */
export interface DatalinkNameColumnProps extends AssetColumnProps {
  readonly item: DatalinkAsset
}

/**
 * The icon and name of a {@link DatalinkAsset}.
 * @throws {Error} when the asset is not a {@link DatalinkAsset}.
 * This should never happen.
 */
export default function DatalinkNameColumn(props: DatalinkNameColumnProps) {
  const { item, rowState, setRowState, isEditable } = props
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(merger({ isEditingName }))
    }
  }

  // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
  // context menu entry should be re-added.
  // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
  const doRename = () => Promise.resolve(null)

  return (
    <div
      className="flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={(event) => {
        if (isDoubleClick(event)) {
          event.stopPropagation()
          setIsAssetPanelTemporarilyVisible(true)
        }
      }}
    >
      <img src={DatalinkIcon} className="m-name-column-icon size-4" />
      <EditableSpan
        editable={false}
        onSubmit={async () => {
          await doRename()
          setIsEditing(false)
        }}
        onCancel={() => {
          setIsEditing(false)
        }}
        className="grow bg-transparent font-naming"
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
