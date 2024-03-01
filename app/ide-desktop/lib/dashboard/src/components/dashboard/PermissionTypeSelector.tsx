/** @file A selector for all possible permission types. */
import * as React from 'react'

import * as backend from '#/services/Backend'

import * as permissions from '#/utilities/permissions'

// =================
// === Constants ===
// =================

const CAPITALIZED_ASSET_TYPE: Readonly<Record<backend.AssetType, string>> = {
  [backend.AssetType.directory]: 'Folder',
  [backend.AssetType.project]: 'Project',
  [backend.AssetType.file]: 'File',
  [backend.AssetType.dataLink]: 'Data Link',
  [backend.AssetType.secret]: 'Secret',
  // These assets should never be visible, since they don't have columns.
  [backend.AssetType.specialEmpty]: 'Empty asset',
  [backend.AssetType.specialLoading]: 'Loading asset',
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
    description: type =>
      CAPITALIZED_ASSET_TYPE[type] +
      ` visibility only. Optionally, edit docs${
        type === backend.AssetType.project ? ' and execute project' : ''
      }.`,
  },
  {
    type: permissions.Permission.read,
    previous: permissions.Permission.view,
    description: type => CAPITALIZED_ASSET_TYPE[type] + ' content reading.',
  },
  {
    type: permissions.Permission.edit,
    previous: permissions.Permission.read,
    description: type => CAPITALIZED_ASSET_TYPE[type] + ' editing.',
  },
  {
    type: permissions.Permission.admin,
    previous: permissions.Permission.edit,
    description: () => 'Sharing management.',
  },
  {
    type: permissions.Permission.owner,
    previous: permissions.Permission.admin,
    description: type => CAPITALIZED_ASSET_TYPE[type] + ' removal permission.',
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
    <div
      style={style}
      className="sticky pointer-events-auto w-min rounded-permission-type-selector before:absolute before:bg-selected-frame before:rounded-permission-type-selector before:backdrop-blur-default before:w-full before:h-full"
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <div className="group relative flex flex-col w-permission-type-selector p-permission-type-selector">
        {PERMISSION_TYPE_DATA.filter(
          data =>
            (showDelete ? true : data.type !== permissions.Permission.delete) &&
            (selfPermission === permissions.PermissionAction.own
              ? true
              : data.type !== permissions.Permission.owner)
        ).map(data => (
          <button
            key={data.type}
            type="button"
            className={`flex items-start rounded-full gap-permission-type-button h-row p-permission-type-button hover:bg-black/5 ${
              type === data.type ? 'bg-black/5 group-hover:bg-transparent hover:!bg-black/5' : ''
            }`}
            onClick={() => {
              onChange(data.type)
            }}
          >
            <div
              className={`rounded-full w-permission-type h-full py-permission-type-y ${
                permissions.PERMISSION_CLASS_NAME[data.type]
              }`}
            >
              {data.type}
            </div>
            <span className="text font-normal">=</span>
            {data.previous != null && (
              <>
                <div
                  className={`text-center rounded-full w-permission-type h-full py-permission-type-y ${
                    permissions.PERMISSION_CLASS_NAME[data.previous]
                  }`}
                >
                  {data.previous}
                </div>
                <span className="text font-normal">+</span>
              </>
            )}
            <span className="text">{data.description(assetType)}</span>
          </button>
        ))}
      </div>
    </div>
  )
}
