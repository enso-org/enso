/** @file A selector for all possible permission types. */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'

import * as backend from '#/services/Backend'

import * as permissions from '#/utilities/permissions'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const CAPITALIZED_ASSET_TYPE: Readonly<Record<backend.AssetType, string>> = {
  [backend.AssetType.directory]: 'Folder',
  [backend.AssetType.project]: 'Project',
  [backend.AssetType.file]: 'File',
  [backend.AssetType.datalink]: 'Datalink',
  [backend.AssetType.secret]: 'Secret',
  // These assets should never be visible, since they don't appear in the UI at all.
  [backend.AssetType.specialEmpty]: '',
  [backend.AssetType.specialError]: '',
  [backend.AssetType.specialLoading]: '',
}

/** Data needed to display a single permission type. */
interface PermissionTypeData {
  readonly type: permissions.Permission
  readonly previous: permissions.Permission | null
  readonly description: (type: backend.AssetType) => string
}

/** Data needed to display each permission type. */
const PERMISSION_TYPE_DATA: readonly PermissionTypeData[] = [
  {
    type: permissions.Permission.view,
    previous: null,
    description: (type) =>
      CAPITALIZED_ASSET_TYPE[type] +
      ` visibility only. Optionally, edit docs${
        type === backend.AssetType.project ? ' and execute project' : ''
      }.`,
  },
  {
    type: permissions.Permission.read,
    previous: permissions.Permission.view,
    description: (type) => CAPITALIZED_ASSET_TYPE[type] + ' content reading.',
  },
  {
    type: permissions.Permission.edit,
    previous: permissions.Permission.read,
    description: (type) => CAPITALIZED_ASSET_TYPE[type] + ' editing.',
  },
  {
    type: permissions.Permission.admin,
    previous: permissions.Permission.edit,
    description: () => 'Sharing management.',
  },
  {
    type: permissions.Permission.owner,
    previous: permissions.Permission.admin,
    description: (type) => CAPITALIZED_ASSET_TYPE[type] + ' removal permission.',
  },
  {
    type: permissions.Permission.delete,
    previous: null,
    description: () => 'Remove all permissions from this user.',
  },
]

// ==============================
// === PermissionTypeSelector ===
// ==============================

/** Props for a {@link PermissionTypeSelector}. */
export interface PermissionTypeSelectorProps {
  readonly showDelete?: boolean
  readonly selfPermission: permissions.PermissionAction
  readonly type: permissions.Permission
  readonly assetType: backend.AssetType
  readonly style?: React.CSSProperties
  readonly onChange: (permission: permissions.Permission) => void
}

/** A selector for all possible permission types. */
export default function PermissionTypeSelector(props: PermissionTypeSelectorProps) {
  const { showDelete = false, selfPermission, type, assetType, style, onChange } = props
  return (
    <FocusArea direction="vertical">
      {(innerProps) => (
        <div
          style={style}
          className="pointer-events-auto sticky w-min rounded-permission-type-selector before:absolute before:h-full before:w-full before:rounded-permission-type-selector before:bg-selected-frame before:backdrop-blur-default"
          onClick={(event) => {
            event.stopPropagation()
          }}
          {...innerProps}
        >
          <div className="group relative flex w-permission-type-selector flex-col p-permission-type-selector">
            {PERMISSION_TYPE_DATA.filter(
              (data) =>
                (showDelete ? true : data.type !== permissions.Permission.delete) &&
                (selfPermission === permissions.PermissionAction.own ?
                  true
                : data.type !== permissions.Permission.owner),
            ).map((data) => (
              <ariaComponents.Button
                size="custom"
                variant="custom"
                key={data.type}
                className={tailwindMerge.twMerge(
                  'flex h-row items-start justify-stretch gap-permission-type-button rounded-full p-permission-type-button hover:bg-black/5',
                  type === data.type && 'bg-black/5 hover:!bg-black/5 group-hover:bg-transparent',
                )}
                onPress={() => {
                  onChange(data.type)
                }}
              >
                <div
                  className={tailwindMerge.twMerge(
                    'h-text w-permission-type rounded-full py-permission-type-y',
                    permissions.PERMISSION_CLASS_NAME[data.type],
                  )}
                >
                  {data.type}
                </div>
                {/* This is a symbol that should never need to be localized, since it is effectively
                 * an icon. */}
                {/* eslint-disable-next-line no-restricted-syntax */}
                <aria.Text className="text font-normal">=</aria.Text>
                {data.previous != null && (
                  <>
                    <div
                      className={tailwindMerge.twMerge(
                        'h-text w-permission-type rounded-full py-permission-type-y text-center',
                        permissions.PERMISSION_CLASS_NAME[data.previous],
                      )}
                    >
                      {data.previous}
                    </div>
                    {/* This is a symbol that should never need to be localized, since it is effectively
                     * an icon. */}
                    {/* eslint-disable-next-line no-restricted-syntax */}
                    <aria.Text className="text font-normal">+</aria.Text>
                  </>
                )}
                <aria.Label className="text">{data.description(assetType)}</aria.Label>
              </ariaComponents.Button>
            ))}
          </div>
        </div>
      )}
    </FocusArea>
  )
}
