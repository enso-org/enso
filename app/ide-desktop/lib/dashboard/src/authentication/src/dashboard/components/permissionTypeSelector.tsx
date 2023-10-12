/** @file A selector for all possible permission types. */
import * as React from 'react'

import * as backend from '../backend'
import * as permissions from '../permissions'

// =================
// === Constants ===
// =================

const CAPITALIZED_ASSET_TYPE: Record<backend.AssetType, string> = {
    [backend.AssetType.directory]: 'Folder',
    [backend.AssetType.project]: 'Project',
    [backend.AssetType.file]: 'File',
    [backend.AssetType.secret]: 'Secret',
    // These assets should never be visible, since they don't have columns.
    [backend.AssetType.specialEmpty]: 'Empty asset',
    [backend.AssetType.specialLoading]: 'Loading asset',
} as const

/** Data needed to display a single permission type. */
interface PermissionTypeData {
    type: permissions.Permission
    previous: permissions.Permission | null
    description: (type: backend.AssetType) => string
}

/** Data needed to display each permission type. */
const PERMISSION_TYPE_DATA: PermissionTypeData[] = [
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
    showDelete?: boolean
    selfPermission: permissions.PermissionAction
    type: permissions.Permission
    assetType: backend.AssetType
    style?: React.CSSProperties
    onChange: (permission: permissions.Permission) => void
}

/** A selector for all possible permission types. */
export default function PermissionTypeSelector(props: PermissionTypeSelectorProps) {
    const { showDelete = false, selfPermission, type, assetType, style, onChange } = props
    return (
        <div
            style={style}
            className="pointer-events-auto sticky w-min"
            onClick={event => {
                event.stopPropagation()
            }}
        >
            <div className="absolute h-full w-full rounded-2xl bg-frame-selected backdrop-blur-3xl" />
            <div className="relative flex w-112.5 flex-col p-1">
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
                        disabled={type === data.type}
                        className={`flex h-8 items-center gap-2 rounded-full px-1 ${
                            type === data.type ? 'bg-black-a5' : ''
                        }`}
                        onClick={() => {
                            onChange(data.type)
                        }}
                    >
                        <div
                            className={`my-1 h-5 w-13 rounded-full py-0.5 ${
                                permissions.PERMISSION_CLASS_NAME[data.type]
                            }`}
                        >
                            {data.type}
                        </div>
                        <span className="h-6.5 pt-1 font-normal leading-170">
                            <span className="h-5.5 py-px">=</span>
                        </span>
                        {data.previous != null && (
                            <>
                                <div
                                    className={`my-1 h-5 w-13 rounded-full py-0.5 text-center ${
                                        permissions.PERMISSION_CLASS_NAME[data.previous]
                                    }`}
                                >
                                    {data.previous}
                                </div>
                                <span className="h-6.5 pt-1 font-normal leading-170">
                                    <span className="h-5.5 py-px">+</span>
                                </span>
                            </>
                        )}
                        <div className="h-6.5 pt-1 leading-170">
                            <span className="h-5.5 py-px">{data.description(assetType)}</span>
                        </div>
                    </button>
                ))}
            </div>
        </div>
    )
}
