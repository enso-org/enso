/** @file The icon and name of a {@link DirectoryAsset}. */
import { Button } from '#/components/Button'
import EditableSpan from '#/components/EditableSpan'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import {
  useIsEditingName,
  useSetCurrentDirectoryId,
  useSetEditingNameAssetId,
} from '#/providers/DriveProvider'
import { titleSchema, type DirectoryAsset } from '#/services/Backend'
import { twJoin } from '#/utilities/tailwindMerge'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { useTransition } from 'react'

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps extends AssetColumnProps {
  readonly item: DirectoryAsset
}

/**
 * The icon and name of a {@link DirectoryAsset}.
 * @throws {Error} when the asset is not a {@link DirectoryAsset}.
 * This should never happen.
 */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
  const { item, state, isEditable, isNavigating } = props
  const { backend } = state

  const [isLoading, startNavigation] = useTransition()
  const isEditing = useIsEditingName(item.id) && isEditable
  const setEditingNameAssetId = useSetEditingNameAssetId()

  const { getText } = useText()
  const setCurrentDirectoryId = useSetCurrentDirectoryId()
  const getAssetChildren = useGetAssetChildren()

  const updateAsset = useMutationCallback(
    backendMutationOptions(backend, 'updateAsset', {
      onSuccess: () => {
        setEditingNameAssetId(null)
      },
    }),
  )

  return (
    <div
      className="group flex h-table-row items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (isEditing && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
    >
      <Button
        icon="folder"
        variant="icon"
        isLoading={isLoading || isNavigating}
        aria-label={getText('open')}
        tooltipPlacement="left"
        testId="directory-row-navigate-button"
        className="mx-1 transition-transform duration-arrow"
        onPress={() => {
          startNavigation(() => {
            setCurrentDirectoryId(item.id)
          })
        }}
      />

      <EditableSpan
        data-testid="asset-row-name"
        editable={isEditing}
        schema={() =>
          titleSchema({
            asset: item,
            siblings: getAssetChildren(item.parentId),
          })
        }
        className={twJoin(
          'bg-transparent font-naming',
          isEditing ? 'cursor-text' : 'cursor-pointer',
        )}
        onSubmit={(newTitle) => updateAsset([item.id, { title: newTitle }, item.title])}
        onCancel={() => {
          setEditingNameAssetId(null)
        }}
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
